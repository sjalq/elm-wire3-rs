// Internal type AST — the intermediate representation between parsing and codegen.
//
// The parser extracts Elm type definitions into this format.
// The codegen module then walks this AST to emit Rust types + Wire3 impls.

/// A parsed Elm module's type definitions.
#[derive(Debug, Clone)]
pub struct ElmModule {
    pub module_name: String,
    pub types: Vec<ElmTypeDef>,
}

/// A single type definition from an Elm file.
#[derive(Debug, Clone)]
pub enum ElmTypeDef {
    /// `type alias Foo = { bar : Int, baz : String }`
    Alias(TypeAlias),
    /// `type MyUnion = A Int | B String Float | C`
    Union(UnionType),
}

impl ElmTypeDef {
    pub fn name(&self) -> &str {
        match self {
            ElmTypeDef::Alias(a) => &a.name,
            ElmTypeDef::Union(u) => &u.name,
        }
    }

    pub fn type_vars(&self) -> &[String] {
        match self {
            ElmTypeDef::Alias(a) => &a.type_vars,
            ElmTypeDef::Union(u) => &u.type_vars,
        }
    }
}

/// `type alias Name a b = ...`
#[derive(Debug, Clone)]
pub struct TypeAlias {
    pub name: String,
    pub type_vars: Vec<String>,
    pub body: ElmType,
}

/// `type Name a b = Ctor1 ... | Ctor2 ...`
#[derive(Debug, Clone)]
pub struct UnionType {
    pub name: String,
    pub type_vars: Vec<String>,
    pub constructors: Vec<Constructor>,
}

/// A single constructor: `Ctor Int String`
#[derive(Debug, Clone)]
pub struct Constructor {
    pub name: String,
    pub params: Vec<ElmType>,
}

/// An Elm type expression.
#[derive(Debug, Clone)]
pub enum ElmType {
    /// Primitive: Int, Float, Bool, Char, String, Bytes, Order, Never
    Named {
        module: Option<String>,
        name: String,
        args: Vec<ElmType>,
    },
    /// Type variable: `a`, `comparable`, `msg`
    Var(String),
    /// Record: `{ field1 : Type1, field2 : Type2 }`
    Record(Vec<RecordField>),
    /// Extensible record: `{ a | field1 : Type1 }`
    ExtensibleRecord {
        base_var: String,
        fields: Vec<RecordField>,
    },
    /// Tuple: `(A, B)` or `(A, B, C)`
    Tuple(Vec<ElmType>),
    /// Unit: `()`
    Unit,
    /// Function: `a -> b` (not encodable, but we need to detect them)
    Function(Box<ElmType>, Box<ElmType>),
}

/// A record field: `name : Type`
#[derive(Debug, Clone)]
pub struct RecordField {
    pub name: String,
    pub type_: ElmType,
}

impl ElmType {
    /// Check if this type (or any nested type) contains a function arrow.
    pub fn contains_function(&self) -> bool {
        match self {
            ElmType::Function(_, _) => true,
            ElmType::Named { args, .. } => args.iter().any(|a| a.contains_function()),
            ElmType::Record(fields) => fields.iter().any(|f| f.type_.contains_function()),
            ElmType::ExtensibleRecord { fields, .. } => {
                fields.iter().any(|f| f.type_.contains_function())
            }
            ElmType::Tuple(elems) => elems.iter().any(|e| e.contains_function()),
            ElmType::Var(_) | ElmType::Unit => false,
        }
    }

    /// Check if this is a known built-in type that Wire3 handles directly.
    pub fn is_builtin(&self) -> bool {
        match self {
            ElmType::Named {
                module, name, args, ..
            } => {
                let is_core = module.is_none()
                    || module.as_deref() == Some("Basics")
                    || module.as_deref() == Some("String")
                    || module.as_deref() == Some("Char");
                if is_core {
                    matches!(
                        name.as_str(),
                        "Int"
                            | "Float"
                            | "Bool"
                            | "Char"
                            | "String"
                            | "Order"
                            | "Never"
                    )
                } else {
                    let is_container = matches!(
                        (module.as_deref(), name.as_str()),
                        (Some("List"), "List")
                            | (None, "List")
                            | (Some("Array"), "Array")
                            | (None, "Array")
                            | (Some("Set"), "Set")
                            | (None, "Set")
                            | (Some("Dict"), "Dict")
                            | (None, "Dict")
                            | (Some("Maybe"), "Maybe")
                            | (None, "Maybe")
                            | (Some("Result"), "Result")
                            | (None, "Result")
                            | (Some("Bytes"), "Bytes")
                            | (None, "Bytes")
                            | (Some("Time"), "Posix")
                    );
                    if is_container {
                        return true;
                    }
                    // Check if args contain builtins (for nested types)
                    let _ = args;
                    false
                }
            }
            ElmType::Unit | ElmType::Tuple(_) => true,
            _ => false,
        }
    }
}
