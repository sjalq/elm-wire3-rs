use crate::types::*;

/// Parse an Elm source file and extract all type definitions.
pub fn parse_elm_module(source: &str) -> Result<ElmModule, String> {
    let mut parser = tree_sitter::Parser::new();
    let language = tree_sitter_elm::LANGUAGE;
    parser
        .set_language(&language.into())
        .map_err(|e| format!("Failed to set tree-sitter language: {e}"))?;

    let tree = parser
        .parse(source, None)
        .ok_or_else(|| "Failed to parse Elm source".to_string())?;

    let root = tree.root_node();

    let module_name = extract_module_name(root, source.as_bytes());
    let mut types = Vec::new();

    let mut cursor = root.walk();
    for child in root.children(&mut cursor) {
        match child.kind() {
            "type_alias_declaration" => {
                if let Some(alias) = parse_type_alias(child, source.as_bytes()) {
                    types.push(ElmTypeDef::Alias(alias));
                }
            }
            "type_declaration" => {
                if let Some(union) = parse_union_type(child, source.as_bytes()) {
                    types.push(ElmTypeDef::Union(union));
                }
            }
            _ => {}
        }
    }

    Ok(ElmModule {
        module_name: module_name.unwrap_or_else(|| "Main".to_string()),
        types,
    })
}

fn node_text<'a>(node: tree_sitter::Node, source: &'a [u8]) -> &'a str {
    node.utf8_text(source).unwrap_or("")
}

fn extract_module_name(root: tree_sitter::Node, source: &[u8]) -> Option<String> {
    let mut cursor = root.walk();
    for child in root.children(&mut cursor) {
        if child.kind() == "module_declaration" {
            // Find the module name within the declaration
            let mut inner = child.walk();
            for part in child.children(&mut inner) {
                if part.kind() == "upper_case_qid" {
                    return Some(node_text(part, source).to_string());
                }
            }
        }
    }
    None
}

fn parse_type_alias(node: tree_sitter::Node, source: &[u8]) -> Option<TypeAlias> {
    let mut name = None;
    let mut type_vars = Vec::new();
    let mut body = None;

    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        match child.kind() {
            "upper_case_identifier" => {
                if name.is_none() {
                    name = Some(node_text(child, source).to_string());
                }
            }
            "lower_type_name" => {
                type_vars.push(node_text(child, source).to_string());
            }
            "type_expression" | "record_type" | "type_ref" | "tuple_type" => {
                body = parse_type_expr(child, source);
            }
            _ => {
                // Try to parse as type expression for any unrecognized node
                // that might contain the type body
                if body.is_none() && is_type_node(child.kind()) {
                    body = parse_type_expr(child, source);
                }
            }
        }
    }

    Some(TypeAlias {
        name: name?,
        type_vars,
        body: body?,
    })
}

fn parse_union_type(node: tree_sitter::Node, source: &[u8]) -> Option<UnionType> {
    let mut name = None;
    let mut type_vars = Vec::new();
    let mut constructors = Vec::new();

    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        match child.kind() {
            "upper_case_identifier" => {
                if name.is_none() {
                    name = Some(node_text(child, source).to_string());
                }
            }
            "lower_type_name" => {
                type_vars.push(node_text(child, source).to_string());
            }
            "union_variant" => {
                if let Some(ctor) = parse_constructor(child, source) {
                    constructors.push(ctor);
                }
            }
            _ => {}
        }
    }

    Some(UnionType {
        name: name?,
        type_vars,
        constructors,
    })
}

fn parse_constructor(node: tree_sitter::Node, source: &[u8]) -> Option<Constructor> {
    let mut name = None;
    let mut params = Vec::new();

    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        match child.kind() {
            "upper_case_identifier" => {
                if name.is_none() {
                    name = Some(node_text(child, source).to_string());
                } else {
                    // An upper_case_identifier as a parameter means a type reference
                    params.push(ElmType::Named {
                        module: None,
                        name: node_text(child, source).to_string(),
                        args: vec![],
                    });
                }
            }
            _ => {
                if let Some(t) = parse_type_expr(child, source) {
                    params.push(t);
                }
            }
        }
    }

    Some(Constructor {
        name: name?,
        params,
    })
}

fn is_type_node(kind: &str) -> bool {
    matches!(
        kind,
        "type_expression"
            | "type_ref"
            | "record_type"
            | "tuple_type"
            | "type_variable"
            | "unit_expr"
    )
}

fn parse_type_expr(node: tree_sitter::Node, source: &[u8]) -> Option<ElmType> {
    match node.kind() {
        "type_expression" => {
            // Could be a function type (a -> b) or a simple type
            let mut children: Vec<tree_sitter::Node> = Vec::new();
            let mut cursor = node.walk();
            for child in node.children(&mut cursor) {
                if child.kind() != "arrow" && child.kind() != "->" {
                    children.push(child);
                }
            }

            if children.len() >= 2 {
                // Check if this is a function type by looking for arrow
                let full_text = node_text(node, source);
                if full_text.contains("->") {
                    // Function type: fold right
                    let mut types: Vec<ElmType> = children
                        .iter()
                        .filter_map(|c| parse_type_expr(*c, source))
                        .collect();
                    if types.len() >= 2 {
                        let mut result = types.pop().unwrap();
                        while let Some(param) = types.pop() {
                            result = ElmType::Function(Box::new(param), Box::new(result));
                        }
                        return Some(result);
                    }
                }
            }

            // Not a function type — delegate to the single child
            if children.len() == 1 {
                return parse_type_expr(children[0], source);
            }

            // Multiple children without arrow — might be type application
            parse_type_application(&children, source)
        }

        "type_ref" => {
            // Named type possibly with arguments: `List Int`, `Dict String Int`
            let mut children: Vec<tree_sitter::Node> = Vec::new();
            let mut cursor = node.walk();
            for child in node.children(&mut cursor) {
                children.push(child);
            }
            parse_type_application(&children, source)
        }

        "upper_case_qid" => {
            let text = node_text(node, source);
            let parts: Vec<&str> = text.split('.').collect();
            if parts.len() == 1 {
                Some(ElmType::Named {
                    module: None,
                    name: parts[0].to_string(),
                    args: vec![],
                })
            } else {
                let name = parts.last().unwrap().to_string();
                let module = parts[..parts.len() - 1].join(".");
                Some(ElmType::Named {
                    module: Some(module),
                    name,
                    args: vec![],
                })
            }
        }

        "upper_case_identifier" => Some(ElmType::Named {
            module: None,
            name: node_text(node, source).to_string(),
            args: vec![],
        }),

        "type_variable" | "lower_type_name" => {
            Some(ElmType::Var(node_text(node, source).to_string()))
        }

        "record_type" => {
            let mut fields = Vec::new();
            let mut base_var = None;

            let mut cursor = node.walk();
            for child in node.children(&mut cursor) {
                match child.kind() {
                    "field_type" => {
                        if let Some(field) = parse_record_field(child, source) {
                            fields.push(field);
                        }
                    }
                    "type_variable" | "lower_type_name" => {
                        // Extensible record: { a | field : Type }
                        base_var = Some(node_text(child, source).to_string());
                    }
                    _ => {}
                }
            }

            if let Some(base) = base_var {
                Some(ElmType::ExtensibleRecord {
                    base_var: base,
                    fields,
                })
            } else {
                Some(ElmType::Record(fields))
            }
        }

        "tuple_type" => {
            let mut elems = Vec::new();
            let mut cursor = node.walk();
            for child in node.children(&mut cursor) {
                if let Some(t) = parse_type_expr(child, source) {
                    elems.push(t);
                }
            }
            if elems.is_empty() {
                Some(ElmType::Unit)
            } else {
                Some(ElmType::Tuple(elems))
            }
        }

        "unit_expr" | "unit_type" => Some(ElmType::Unit),

        // Handle parenthesized types
        "parenthesized_type" => {
            let mut cursor = node.walk();
            for child in node.children(&mut cursor) {
                if let Some(t) = parse_type_expr(child, source) {
                    return Some(t);
                }
            }
            None
        }

        _ => {
            // Try children for unrecognized wrapper nodes
            let mut cursor = node.walk();
            for child in node.children(&mut cursor) {
                if let Some(t) = parse_type_expr(child, source) {
                    return Some(t);
                }
            }
            None
        }
    }
}

fn parse_type_application(children: &[tree_sitter::Node], source: &[u8]) -> Option<ElmType> {
    if children.is_empty() {
        return None;
    }

    // First child is the type name, rest are arguments
    let first = children[0];
    let base = match first.kind() {
        "upper_case_qid" => {
            let text = node_text(first, source);
            let parts: Vec<&str> = text.split('.').collect();
            if parts.len() == 1 {
                (None, parts[0].to_string())
            } else {
                let name = parts.last().unwrap().to_string();
                let module = parts[..parts.len() - 1].join(".");
                (Some(module), name)
            }
        }
        "upper_case_identifier" => (None, node_text(first, source).to_string()),
        _ => return parse_type_expr(first, source),
    };

    let args: Vec<ElmType> = children[1..]
        .iter()
        .filter_map(|c| parse_type_expr(*c, source))
        .collect();

    Some(ElmType::Named {
        module: base.0,
        name: base.1,
        args,
    })
}

fn parse_record_field(node: tree_sitter::Node, source: &[u8]) -> Option<RecordField> {
    let mut name = None;
    let mut type_ = None;

    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        match child.kind() {
            "lower_case_identifier" => {
                if name.is_none() {
                    name = Some(node_text(child, source).to_string());
                }
            }
            _ => {
                if type_.is_none() {
                    type_ = parse_type_expr(child, source);
                }
            }
        }
    }

    Some(RecordField {
        name: name?,
        type_: type_?,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_simple_record_alias() {
        let source = r#"
module Test exposing (..)

type alias Person =
    { name : String
    , age : Int
    , email : String
    }
"#;
        let module = parse_elm_module(source).unwrap();
        assert_eq!(module.module_name, "Test");
        assert_eq!(module.types.len(), 1);

        match &module.types[0] {
            ElmTypeDef::Alias(a) => {
                assert_eq!(a.name, "Person");
                assert!(a.type_vars.is_empty());
                match &a.body {
                    ElmType::Record(fields) => {
                        assert_eq!(fields.len(), 3);
                        assert_eq!(fields[0].name, "name");
                        assert_eq!(fields[1].name, "age");
                        assert_eq!(fields[2].name, "email");
                    }
                    other => panic!("Expected Record, got {other:?}"),
                }
            }
            other => panic!("Expected Alias, got {other:?}"),
        }
    }

    #[test]
    fn parse_union_type() {
        let source = r#"
module Test exposing (..)

type Color
    = Red
    | Green
    | Blue
    | Custom String Int
"#;
        let module = parse_elm_module(source).unwrap();
        assert_eq!(module.types.len(), 1);

        match &module.types[0] {
            ElmTypeDef::Union(u) => {
                assert_eq!(u.name, "Color");
                assert_eq!(u.constructors.len(), 4);
                assert_eq!(u.constructors[0].name, "Red");
                assert_eq!(u.constructors[0].params.len(), 0);
                assert_eq!(u.constructors[3].name, "Custom");
                assert_eq!(u.constructors[3].params.len(), 2);
            }
            other => panic!("Expected Union, got {other:?}"),
        }
    }

    #[test]
    fn parse_parameterized_type() {
        let source = r#"
module Test exposing (..)

type Container a
    = Item a
    | Empty
"#;
        let module = parse_elm_module(source).unwrap();
        match &module.types[0] {
            ElmTypeDef::Union(u) => {
                assert_eq!(u.name, "Container");
                assert_eq!(u.type_vars, vec!["a"]);
                assert_eq!(u.constructors.len(), 2);
            }
            other => panic!("Expected Union, got {other:?}"),
        }
    }

    #[test]
    fn parse_type_alias_simple() {
        let source = r#"
module Test exposing (..)

type alias UserId = Int
"#;
        let module = parse_elm_module(source).unwrap();
        match &module.types[0] {
            ElmTypeDef::Alias(a) => {
                assert_eq!(a.name, "UserId");
                match &a.body {
                    ElmType::Named { name, .. } => assert_eq!(name, "Int"),
                    other => panic!("Expected Named Int, got {other:?}"),
                }
            }
            other => panic!("Expected Alias, got {other:?}"),
        }
    }

    #[test]
    fn parse_complex_types() {
        let source = r#"
module Types exposing (..)

type alias Model =
    { users : List User
    , selected : Maybe UserId
    , errors : Dict String (List String)
    }

type alias UserId = Int

type alias User =
    { id : UserId
    , name : String
    , role : Role
    }

type Role
    = Admin
    | Member
    | Guest String
"#;
        let module = parse_elm_module(source).unwrap();
        assert_eq!(module.types.len(), 4);
    }
}
