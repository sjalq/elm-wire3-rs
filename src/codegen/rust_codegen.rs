use crate::types::*;
use std::fmt::Write;

/// Generate a complete Rust module from parsed Elm type definitions.
pub fn generate_rust_module(module: &ElmModule) -> Result<String, String> {
    let mut out = String::new();

    writeln!(out, "// Auto-generated from Elm module: {}", module.module_name).unwrap();
    writeln!(out, "// Do not edit manually — regenerate from the Elm source.").unwrap();
    writeln!(out).unwrap();
    writeln!(out, "#![allow(dead_code, unused_imports)]").unwrap();
    writeln!(out).unwrap();
    writeln!(out, "use std::collections::{{BTreeMap, BTreeSet}};").unwrap();
    writeln!(
        out,
        "use elm_wire3_rs::wire3::{{Wire3Encoder, Wire3Decoder, types::*}};"
    )
    .unwrap();
    writeln!(out).unwrap();

    for type_def in &module.types {
        if type_def_contains_function(type_def) {
            writeln!(
                out,
                "// Skipped {} (contains function types, not Wire3-encodable)",
                type_def.name()
            )
            .unwrap();
            writeln!(out).unwrap();
            continue;
        }

        match type_def {
            ElmTypeDef::Alias(alias) => {
                generate_alias(&mut out, alias)?;
            }
            ElmTypeDef::Union(union) => {
                generate_union(&mut out, union)?;
            }
        }
        writeln!(out).unwrap();
    }

    Ok(out)
}

fn type_def_contains_function(td: &ElmTypeDef) -> bool {
    match td {
        ElmTypeDef::Alias(a) => a.body.contains_function(),
        ElmTypeDef::Union(u) => u
            .constructors
            .iter()
            .any(|c| c.params.iter().any(|p| p.contains_function())),
    }
}

// ── Type alias codegen ──────────────────────────────────────────

fn generate_alias(out: &mut String, alias: &TypeAlias) -> Result<(), String> {
    let generics = generic_params(&alias.type_vars);
    let derives = derives_for_type(&alias.body);

    match &alias.body {
        ElmType::Record(fields) => {
            // Record alias → Rust struct
            writeln!(out, "{derives}").unwrap();
            writeln!(out, "pub struct {}{generics} {{", alias.name).unwrap();
            // Fields sorted alphabetically (Wire3 encodes in alpha order)
            let mut sorted_fields: Vec<&RecordField> = fields.iter().collect();
            sorted_fields.sort_by_key(|f| &f.name);
            for field in &sorted_fields {
                let rust_type = elm_type_to_rust(&field.type_)?;
                let field_name = sanitize_field_name(&field.name);
                writeln!(out, "    pub {field_name}: {rust_type},").unwrap();
            }
            writeln!(out, "}}").unwrap();
            writeln!(out).unwrap();

            // Encoder
            generate_record_encoder(out, &alias.name, &alias.type_vars, fields)?;
            // Decoder
            generate_record_decoder(out, &alias.name, &alias.type_vars, fields)?;
        }
        _ => {
            // Simple type alias → Rust type alias
            let rust_type = elm_type_to_rust(&alias.body)?;
            writeln!(out, "pub type {}{generics} = {rust_type};", alias.name).unwrap();
            writeln!(out).unwrap();
            // For simple aliases, encoder/decoder just delegates
            generate_alias_codec(out, &alias.name, &alias.type_vars, &alias.body)?;
        }
    }

    Ok(())
}

// ── Union type codegen ──────────────────────────────────────────

fn generate_union(out: &mut String, union: &UnionType) -> Result<(), String> {
    let generics = generic_params(&union.type_vars);

    // Sort constructors alphabetically (Wire3 assigns tags in alpha order)
    let mut sorted_ctors: Vec<&Constructor> = union.constructors.iter().collect();
    sorted_ctors.sort_by_key(|c| &c.name);

    let derives = derives_for_union(union);
    writeln!(out, "{derives}").unwrap();
    writeln!(out, "pub enum {}{generics} {{", union.name).unwrap();
    for ctor in &sorted_ctors {
        if ctor.params.is_empty() {
            writeln!(out, "    {},", ctor.name).unwrap();
        } else {
            let params: Result<Vec<String>, String> =
                ctor.params.iter().map(elm_type_to_rust).collect();
            let params = params?.join(", ");
            writeln!(out, "    {}({params}),", ctor.name).unwrap();
        }
    }
    writeln!(out, "}}").unwrap();
    writeln!(out).unwrap();

    // Encoder
    generate_union_encoder(out, &union.name, &union.type_vars, &sorted_ctors)?;
    // Decoder
    generate_union_decoder(out, &union.name, &union.type_vars, &sorted_ctors)?;

    Ok(())
}

// ── Encoder generation ──────────────────────────────────────────

fn generate_record_encoder(
    out: &mut String,
    name: &str,
    type_vars: &[String],
    fields: &[RecordField],
) -> Result<(), String> {
    let generics = generic_params(type_vars);
    let where_clause = wire3_where_clause(type_vars);

    writeln!(
        out,
        "impl{generics} {name}{generics} {where_clause}{{"
    )
    .unwrap();
    writeln!(
        out,
        "    pub fn wire3_encode(&self, enc: &mut Wire3Encoder) {{"
    )
    .unwrap();

    // Encode fields in ALPHABETICAL order
    let mut sorted_fields: Vec<&RecordField> = fields.iter().collect();
    sorted_fields.sort_by_key(|f| &f.name);

    for field in &sorted_fields {
        let field_name = sanitize_field_name(&field.name);
        let encode_expr = generate_encode_expr(&field.type_, &format!("self.{field_name}"))?;
        writeln!(out, "        {encode_expr};").unwrap();
    }

    writeln!(out, "    }}").unwrap();
    writeln!(out, "}}").unwrap();

    Ok(())
}

fn generate_record_decoder(
    out: &mut String,
    name: &str,
    type_vars: &[String],
    fields: &[RecordField],
) -> Result<(), String> {
    let generics = generic_params(type_vars);
    let where_clause = wire3_where_clause(type_vars);

    writeln!(out).unwrap();
    writeln!(
        out,
        "impl{generics} {name}{generics} {where_clause}{{"
    )
    .unwrap();
    writeln!(
        out,
        "    pub fn wire3_decode(dec: &mut Wire3Decoder) -> Result<Self, Wire3DecodeError> {{"
    )
    .unwrap();

    // Decode fields in ALPHABETICAL order
    let mut sorted_fields: Vec<&RecordField> = fields.iter().collect();
    sorted_fields.sort_by_key(|f| &f.name);

    for field in &sorted_fields {
        let field_name = sanitize_field_name(&field.name);
        let decode_expr = generate_decode_expr(&field.type_)?;
        writeln!(out, "        let {field_name} = {decode_expr}?;").unwrap();
    }

    writeln!(out, "        Ok({name} {{").unwrap();
    for field in &sorted_fields {
        let field_name = sanitize_field_name(&field.name);
        writeln!(out, "            {field_name},").unwrap();
    }
    writeln!(out, "        }})").unwrap();
    writeln!(out, "    }}").unwrap();
    writeln!(out, "}}").unwrap();

    Ok(())
}

fn generate_union_encoder(
    out: &mut String,
    name: &str,
    type_vars: &[String],
    sorted_ctors: &[&Constructor],
) -> Result<(), String> {
    let generics = generic_params(type_vars);
    let where_clause = wire3_where_clause(type_vars);
    let tag_fn = if sorted_ctors.len() > 255 {
        "encode_tag16"
    } else {
        "encode_tag8"
    };

    writeln!(
        out,
        "impl{generics} {name}{generics} {where_clause}{{"
    )
    .unwrap();
    writeln!(
        out,
        "    pub fn wire3_encode(&self, enc: &mut Wire3Encoder) {{"
    )
    .unwrap();
    writeln!(out, "        match self {{").unwrap();

    for (tag, ctor) in sorted_ctors.iter().enumerate() {
        if ctor.params.is_empty() {
            writeln!(
                out,
                "            {name}::{} => enc.{tag_fn}({tag}{}),",
                ctor.name,
                if sorted_ctors.len() > 255 {
                    " as u16"
                } else {
                    ""
                }
            )
            .unwrap();
        } else {
            let bindings: Vec<String> = (0..ctor.params.len()).map(|i| format!("v{i}")).collect();
            let binding_str = bindings.join(", ");
            writeln!(
                out,
                "            {name}::{}({binding_str}) => {{",
                ctor.name
            )
            .unwrap();
            writeln!(
                out,
                "                enc.{tag_fn}({tag}{});",
                if sorted_ctors.len() > 255 {
                    " as u16"
                } else {
                    ""
                }
            )
            .unwrap();
            for (i, param) in ctor.params.iter().enumerate() {
                let encode_expr = generate_encode_expr(param, &format!("v{i}"))?;
                // For reference types, we need to handle borrowing
                writeln!(out, "                {encode_expr};").unwrap();
            }
            writeln!(out, "            }}").unwrap();
        }
    }

    writeln!(out, "        }}").unwrap();
    writeln!(out, "    }}").unwrap();
    writeln!(out, "}}").unwrap();

    Ok(())
}

fn generate_union_decoder(
    out: &mut String,
    name: &str,
    type_vars: &[String],
    sorted_ctors: &[&Constructor],
) -> Result<(), String> {
    let generics = generic_params(type_vars);
    let where_clause = wire3_where_clause(type_vars);
    let tag_fn = if sorted_ctors.len() > 255 {
        "decode_tag16"
    } else {
        "decode_tag8"
    };

    writeln!(out).unwrap();
    writeln!(
        out,
        "impl{generics} {name}{generics} {where_clause}{{"
    )
    .unwrap();
    writeln!(
        out,
        "    pub fn wire3_decode(dec: &mut Wire3Decoder) -> Result<Self, Wire3DecodeError> {{"
    )
    .unwrap();
    writeln!(
        out,
        "        let tag = dec.{tag_fn}()?{};\n        match tag {{",
        if sorted_ctors.len() > 255 {
            " as usize"
        } else {
            ""
        }
    )
    .unwrap();

    for (tag, ctor) in sorted_ctors.iter().enumerate() {
        if ctor.params.is_empty() {
            writeln!(out, "            {tag} => Ok({name}::{}),", ctor.name).unwrap();
        } else {
            writeln!(out, "            {tag} => {{").unwrap();
            for (i, param) in ctor.params.iter().enumerate() {
                let decode_expr = generate_decode_expr(param)?;
                writeln!(out, "                let v{i} = {decode_expr}?;").unwrap();
            }
            let bindings: Vec<String> = (0..ctor.params.len()).map(|i| format!("v{i}")).collect();
            writeln!(
                out,
                "                Ok({name}::{}({}))",
                ctor.name,
                bindings.join(", ")
            )
            .unwrap();
            writeln!(out, "            }}").unwrap();
        }
    }

    writeln!(
        out,
        "            _ => Err(Wire3DecodeError::InvalidTag {{ tag: tag as u8, type_name: \"{name}\" }}),"
    )
    .unwrap();
    writeln!(out, "        }}").unwrap();
    writeln!(out, "    }}").unwrap();
    writeln!(out, "}}").unwrap();

    Ok(())
}

fn generate_alias_codec(
    out: &mut String,
    _name: &str,
    _type_vars: &[String],
    _body: &ElmType,
) -> Result<(), String> {
    // For simple type aliases (e.g., `type alias UserId = Int`),
    // the Rust type alias means encode/decode works automatically
    // through the underlying type. No additional impls needed.
    writeln!(
        out,
        "// Codec for {_name}: delegates to underlying type via type alias."
    )
    .unwrap();
    Ok(())
}

// ── Type mapping ────────────────────────────────────────────────

fn elm_type_to_rust(elm_type: &ElmType) -> Result<String, String> {
    match elm_type {
        ElmType::Named {
            module, name, args, ..
        } => {
            let rust_name = match (module.as_deref(), name.as_str()) {
                // Primitives
                (_, "Int") => "ElmInt".to_string(),
                (_, "Float") => "ElmFloat".to_string(),
                (_, "Bool") => "bool".to_string(),
                (_, "Char") => "ElmChar".to_string(),
                (_, "String") => "String".to_string(),
                (_, "Order") => "ElmOrder".to_string(),
                (_, "Never") => {
                    return Err("Never type cannot be encoded".to_string());
                }

                // Containers
                (_, "List") => {
                    if args.len() != 1 {
                        return Err(format!("List expects 1 type arg, got {}", args.len()));
                    }
                    let inner = elm_type_to_rust(&args[0])?;
                    return Ok(format!("Vec<{inner}>"));
                }
                (_, "Array") => {
                    if args.len() != 1 {
                        return Err(format!("Array expects 1 type arg, got {}", args.len()));
                    }
                    let inner = elm_type_to_rust(&args[0])?;
                    return Ok(format!("Vec<{inner}>"));
                }
                (_, "Set") => {
                    if args.len() != 1 {
                        return Err(format!("Set expects 1 type arg, got {}", args.len()));
                    }
                    let inner = elm_type_to_rust(&args[0])?;
                    return Ok(format!("BTreeSet<{inner}>"));
                }
                (_, "Dict") => {
                    if args.len() != 2 {
                        return Err(format!("Dict expects 2 type args, got {}", args.len()));
                    }
                    let k = elm_type_to_rust(&args[0])?;
                    let v = elm_type_to_rust(&args[1])?;
                    return Ok(format!("BTreeMap<{k}, {v}>"));
                }
                (_, "Maybe") => {
                    if args.len() != 1 {
                        return Err(format!("Maybe expects 1 type arg, got {}", args.len()));
                    }
                    let inner = elm_type_to_rust(&args[0])?;
                    return Ok(format!("Option<{inner}>"));
                }
                (_, "Result") => {
                    if args.len() != 2 {
                        return Err(format!("Result expects 2 type args, got {}", args.len()));
                    }
                    let err = elm_type_to_rust(&args[0])?;
                    let ok = elm_type_to_rust(&args[1])?;
                    return Ok(format!("Result<{ok}, {err}>"));
                }
                (_, "Bytes") => "Vec<u8>".to_string(),
                (Some("Time"), "Posix") => "ElmInt".to_string(), // millis since epoch

                // User-defined type — keep the name
                (_, user_type) => {
                    if args.is_empty() {
                        user_type.to_string()
                    } else {
                        let type_args: Result<Vec<String>, String> =
                            args.iter().map(elm_type_to_rust).collect();
                        let type_args = type_args?.join(", ");
                        return Ok(format!("{user_type}<{type_args}>"));
                    }
                }
            };
            Ok(rust_name)
        }

        ElmType::Var(name) => {
            // Type variable → Rust generic (uppercase)
            Ok(name[0..1].to_uppercase() + &name[1..])
        }

        ElmType::Record(fields) => {
            // Anonymous record in a constructor param.
            // Wire3 encodes fields in alphabetical order, so we use a tuple
            // with fields sorted alphabetically.
            let mut sorted: Vec<&RecordField> = fields.iter().collect();
            sorted.sort_by_key(|f| &f.name);
            let field_types: Result<Vec<String>, String> =
                sorted.iter().map(|f| elm_type_to_rust(&f.type_)).collect();
            let field_types = field_types?;
            if field_types.len() == 1 {
                Ok(field_types[0].clone())
            } else {
                Ok(format!("({})", field_types.join(", ")))
            }
        }

        ElmType::ExtensibleRecord { .. } => {
            Err("Extensible records cannot be directly translated (need concrete instantiation)".to_string())
        }

        ElmType::Tuple(elems) => {
            let types: Result<Vec<String>, String> =
                elems.iter().map(elm_type_to_rust).collect();
            let types = types?;
            Ok(format!("({})", types.join(", ")))
        }

        ElmType::Unit => Ok("()".to_string()),

        ElmType::Function(_, _) => Err("Function types cannot be Wire3-encoded".to_string()),
    }
}

// ── Encode expression generation ────────────────────────────────

fn generate_encode_expr(elm_type: &ElmType, value_expr: &str) -> Result<String, String> {
    match elm_type {
        ElmType::Named {
            module, name, args, ..
        } => match (module.as_deref(), name.as_str()) {
            (_, "Int") => {
                Ok(format!("enc.encode_int(&{value_expr})"))
            }
            (_, "Float") => {
                Ok(format!("enc.encode_float(&{value_expr})"))
            }
            (_, "Bool") => {
                Ok(format!("enc.encode_bool({value_expr})"))
            }
            (_, "Char") => {
                Ok(format!("enc.encode_char(&{value_expr})"))
            }
            (_, "String") => {
                Ok(format!("enc.encode_string(&{value_expr})"))
            }
            (_, "Order") => {
                Ok(format!("enc.encode_order({value_expr})"))
            }
            (_, "Bytes") => {
                Ok(format!("enc.encode_bytes(&{value_expr})"))
            }
            (Some("Time"), "Posix") => {
                Ok(format!("enc.encode_int(&{value_expr})"))
            }
            (_, "List") if args.len() == 1 => {
                let elem_enc = generate_encode_closure(&args[0])?;
                Ok(format!("enc.encode_list(&{value_expr}, {elem_enc})"))
            }
            (_, "Array") if args.len() == 1 => {
                let elem_enc = generate_encode_closure(&args[0])?;
                Ok(format!("enc.encode_list(&{value_expr}, {elem_enc})"))
            }
            (_, "Set") if args.len() == 1 => {
                let elem_enc = generate_encode_closure(&args[0])?;
                Ok(format!("enc.encode_set(&{value_expr}, {elem_enc})"))
            }
            (_, "Dict") if args.len() == 2 => {
                let key_enc = generate_encode_closure(&args[0])?;
                let val_enc = generate_encode_closure(&args[1])?;
                Ok(format!(
                    "enc.encode_dict(&{value_expr}, {key_enc}, {val_enc})"
                ))
            }
            (_, "Maybe") if args.len() == 1 => {
                let inner_enc = generate_encode_closure(&args[0])?;
                Ok(format!("enc.encode_maybe(&{value_expr}, {inner_enc})"))
            }
            (_, "Result") if args.len() == 2 => {
                let err_enc = generate_encode_closure(&args[0])?;
                let ok_enc = generate_encode_closure(&args[1])?;
                Ok(format!(
                    "enc.encode_result(&{value_expr}, {err_enc}, {ok_enc})"
                ))
            }
            // User-defined type
            (_, _) => Ok(format!("{value_expr}.wire3_encode(enc)")),
        },

        ElmType::Var(v) => {
            // Generic type — call wire3_encode on it
            let _ = v;
            Ok(format!("{value_expr}.wire3_encode(enc)"))
        }

        ElmType::Tuple(elems) if elems.len() == 2 => {
            let a_enc = generate_encode_closure(&elems[0])?;
            let b_enc = generate_encode_closure(&elems[1])?;
            Ok(format!("enc.encode_pair(&{value_expr}, {a_enc}, {b_enc})"))
        }

        ElmType::Tuple(elems) if elems.len() == 3 => {
            let a_enc = generate_encode_closure(&elems[0])?;
            let b_enc = generate_encode_closure(&elems[1])?;
            let c_enc = generate_encode_closure(&elems[2])?;
            Ok(format!(
                "enc.encode_triple(&{value_expr}, {a_enc}, {b_enc}, {c_enc})"
            ))
        }

        ElmType::Unit => Ok("enc.encode_unit()".to_string()),

        ElmType::Record(fields) => {
            // Anonymous record in a constructor param — encode fields alphabetically
            let mut sorted: Vec<&RecordField> = fields.iter().collect();
            sorted.sort_by_key(|f| &f.name);
            let mut parts = Vec::new();
            for field in &sorted {
                let field_name = sanitize_field_name(&field.name);
                let encode = generate_encode_expr(&field.type_, &format!("{value_expr}.{field_name}"))?;
                parts.push(encode);
            }
            Ok(parts.join(";\n                "))
        }

        _ => Err(format!("Cannot generate encoder for {elm_type:?}")),
    }
}

fn generate_encode_closure(elm_type: &ElmType) -> Result<String, String> {
    match elm_type {
        ElmType::Named {
            module, name, args, ..
        } => match (module.as_deref(), name.as_str()) {
            (_, "Int") => Ok("|enc, v| enc.encode_int(v)".to_string()),
            (_, "Float") => {
                Ok("|enc, v| enc.encode_float(v)".to_string())
            }
            (_, "Bool") => {
                Ok("|enc, v| enc.encode_bool(*v)".to_string())
            }
            (_, "Char") => {
                Ok("|enc, v| enc.encode_char(v)".to_string())
            }
            (_, "String") => {
                Ok("|enc, v| enc.encode_string(v)".to_string())
            }
            (_, "Bytes") => {
                Ok("|enc, v| enc.encode_bytes(v)".to_string())
            }
            (_, "Order") => {
                Ok("|enc, v| enc.encode_order(*v)".to_string())
            }
            // Nested containers
            (_, "List") if args.len() == 1 => {
                let inner = generate_encode_closure(&args[0])?;
                Ok(format!("|enc, v| enc.encode_list(v, {inner})"))
            }
            (_, "Maybe") if args.len() == 1 => {
                let inner = generate_encode_closure(&args[0])?;
                Ok(format!("|enc, v| enc.encode_maybe(v, {inner})"))
            }
            (_, "Dict") if args.len() == 2 => {
                let key = generate_encode_closure(&args[0])?;
                let val = generate_encode_closure(&args[1])?;
                Ok(format!("|enc, v| enc.encode_dict(v, {key}, {val})"))
            }
            (_, "Set") if args.len() == 1 => {
                let inner = generate_encode_closure(&args[0])?;
                Ok(format!("|enc, v| enc.encode_set(v, {inner})"))
            }
            (_, "Result") if args.len() == 2 => {
                let err = generate_encode_closure(&args[0])?;
                let ok = generate_encode_closure(&args[1])?;
                Ok(format!("|enc, v| enc.encode_result(v, {err}, {ok})"))
            }
            // User-defined type
            (_, _) => Ok("|enc, v| v.wire3_encode(enc)".to_string()),
        },
        ElmType::Var(_) => Ok("|enc, v| v.wire3_encode(enc)".to_string()),
        ElmType::Tuple(elems) if elems.len() == 2 => {
            let a = generate_encode_closure(&elems[0])?;
            let b = generate_encode_closure(&elems[1])?;
            Ok(format!("|enc, v| enc.encode_pair(v, {a}, {b})"))
        }
        ElmType::Tuple(elems) if elems.len() == 3 => {
            let a = generate_encode_closure(&elems[0])?;
            let b = generate_encode_closure(&elems[1])?;
            let c = generate_encode_closure(&elems[2])?;
            Ok(format!("|enc, v| enc.encode_triple(v, {a}, {b}, {c})"))
        }
        ElmType::Unit => Ok("|enc, _| enc.encode_unit()".to_string()),
        ElmType::Record(fields) => {
            // Anonymous record — encode each field in alphabetical order
            let mut sorted: Vec<&RecordField> = fields.iter().collect();
            sorted.sort_by_key(|f| &f.name);
            let mut field_encs = Vec::new();
            for (i, field) in sorted.iter().enumerate() {
                let enc = generate_encode_expr(&field.type_, &format!("v.{i}"))?;
                field_encs.push(enc);
            }
            Ok(format!("|enc, v| {{ {} }}", field_encs.join("; ")))
        }
        _ => Err(format!("Cannot generate encode closure for {elm_type:?}")),
    }
}

// ── Decode expression generation ────────────────────────────────

fn generate_decode_expr(elm_type: &ElmType) -> Result<String, String> {
    match elm_type {
        ElmType::Named {
            module, name, args, ..
        } => match (module.as_deref(), name.as_str()) {
            (_, "Int") => Ok("dec.decode_int()".to_string()),
            (_, "Float") => Ok("dec.decode_float()".to_string()),
            (_, "Bool") => Ok("dec.decode_bool()".to_string()),
            (_, "Char") => Ok("dec.decode_char()".to_string()),
            (_, "String") => Ok("dec.decode_string()".to_string()),
            (_, "Order") => Ok("dec.decode_order()".to_string()),
            (_, "Bytes") => Ok("dec.decode_bytes()".to_string()),
            (Some("Time"), "Posix") => Ok("dec.decode_int()".to_string()),
            (_, "List") if args.len() == 1 => {
                let inner = generate_decode_closure(&args[0])?;
                Ok(format!("dec.decode_list({inner})"))
            }
            (_, "Array") if args.len() == 1 => {
                let inner = generate_decode_closure(&args[0])?;
                Ok(format!("dec.decode_list({inner})"))
            }
            (_, "Set") if args.len() == 1 => {
                let inner = generate_decode_closure(&args[0])?;
                Ok(format!("dec.decode_set({inner})"))
            }
            (_, "Dict") if args.len() == 2 => {
                let key = generate_decode_closure(&args[0])?;
                let val = generate_decode_closure(&args[1])?;
                Ok(format!("dec.decode_dict({key}, {val})"))
            }
            (_, "Maybe") if args.len() == 1 => {
                let inner = generate_decode_closure(&args[0])?;
                Ok(format!("dec.decode_maybe({inner})"))
            }
            (_, "Result") if args.len() == 2 => {
                let err = generate_decode_closure(&args[0])?;
                let ok = generate_decode_closure(&args[1])?;
                Ok(format!("dec.decode_result({err}, {ok})"))
            }
            // User-defined type
            (_, user_type) => Ok(format!("{user_type}::wire3_decode(dec)")),
        },
        ElmType::Var(v) => {
            let upper = v[0..1].to_uppercase() + &v[1..];
            Ok(format!("{upper}::wire3_decode(dec)"))
        }
        ElmType::Tuple(elems) if elems.len() == 2 => {
            let a = generate_decode_closure(&elems[0])?;
            let b = generate_decode_closure(&elems[1])?;
            Ok(format!("dec.decode_pair({a}, {b})"))
        }
        ElmType::Tuple(elems) if elems.len() == 3 => {
            let a = generate_decode_closure(&elems[0])?;
            let b = generate_decode_closure(&elems[1])?;
            let c = generate_decode_closure(&elems[2])?;
            Ok(format!("dec.decode_triple({a}, {b}, {c})"))
        }
        ElmType::Unit => Ok("dec.decode_unit()".to_string()),
        ElmType::Record(fields) => {
            // Anonymous record — decode fields in alphabetical order into a tuple
            let mut sorted: Vec<&RecordField> = fields.iter().collect();
            sorted.sort_by_key(|f| &f.name);
            let mut parts = Vec::new();
            for (i, field) in sorted.iter().enumerate() {
                let decode = generate_decode_expr(&field.type_)?;
                parts.push(format!("let _f{i} = {decode}?;"));
            }
            let tuple_fields: Vec<String> = (0..sorted.len()).map(|i| format!("_f{i}")).collect();
            let mut result = parts.join("\n                ");
            result.push_str(&format!(
                "\n                Ok(({})))",
                tuple_fields.join(", ")
            ));
            Ok(format!("{{\n                {result}\n            }}"))
        }
        _ => Err(format!("Cannot generate decoder for {elm_type:?}")),
    }
}

fn generate_decode_closure(elm_type: &ElmType) -> Result<String, String> {
    match elm_type {
        ElmType::Named {
            module, name, args, ..
        } => match (module.as_deref(), name.as_str()) {
            (_, "Int") => Ok("|dec| dec.decode_int()".to_string()),
            (_, "Float") => {
                Ok("|dec| dec.decode_float()".to_string())
            }
            (_, "Bool") => Ok("|dec| dec.decode_bool()".to_string()),
            (_, "Char") => Ok("|dec| dec.decode_char()".to_string()),
            (_, "String") => {
                Ok("|dec| dec.decode_string()".to_string())
            }
            (_, "Bytes") => {
                Ok("|dec| dec.decode_bytes()".to_string())
            }
            (_, "Order") => {
                Ok("|dec| dec.decode_order()".to_string())
            }
            (_, "List") if args.len() == 1 => {
                let inner = generate_decode_closure(&args[0])?;
                Ok(format!("|dec| dec.decode_list({inner})"))
            }
            (_, "Maybe") if args.len() == 1 => {
                let inner = generate_decode_closure(&args[0])?;
                Ok(format!("|dec| dec.decode_maybe({inner})"))
            }
            (_, "Dict") if args.len() == 2 => {
                let key = generate_decode_closure(&args[0])?;
                let val = generate_decode_closure(&args[1])?;
                Ok(format!("|dec| dec.decode_dict({key}, {val})"))
            }
            (_, "Set") if args.len() == 1 => {
                let inner = generate_decode_closure(&args[0])?;
                Ok(format!("|dec| dec.decode_set({inner})"))
            }
            (_, "Result") if args.len() == 2 => {
                let err = generate_decode_closure(&args[0])?;
                let ok = generate_decode_closure(&args[1])?;
                Ok(format!("|dec| dec.decode_result({err}, {ok})"))
            }
            (_, user_type) => Ok(format!("|dec| {user_type}::wire3_decode(dec)")),
        },
        ElmType::Var(v) => {
            let upper = v[0..1].to_uppercase() + &v[1..];
            Ok(format!("|dec| {upper}::wire3_decode(dec)"))
        }
        ElmType::Tuple(elems) if elems.len() == 2 => {
            let a = generate_decode_closure(&elems[0])?;
            let b = generate_decode_closure(&elems[1])?;
            Ok(format!("|dec| dec.decode_pair({a}, {b})"))
        }
        ElmType::Tuple(elems) if elems.len() == 3 => {
            let a = generate_decode_closure(&elems[0])?;
            let b = generate_decode_closure(&elems[1])?;
            let c = generate_decode_closure(&elems[2])?;
            Ok(format!("|dec| dec.decode_triple({a}, {b}, {c})"))
        }
        ElmType::Unit => Ok("|dec| dec.decode_unit()".to_string()),
        ElmType::Record(fields) => {
            let mut sorted: Vec<&RecordField> = fields.iter().collect();
            sorted.sort_by_key(|f| &f.name);
            let mut parts = Vec::new();
            for (i, field) in sorted.iter().enumerate() {
                let decode = generate_decode_expr(&field.type_)?;
                parts.push(format!("let _f{i} = {decode}?;"));
            }
            let tuple_fields: Vec<String> = (0..sorted.len()).map(|i| format!("_f{i}")).collect();
            Ok(format!(
                "|dec| {{ {} Ok(({},)) }}",
                parts.join(" "),
                tuple_fields.join(", ")
            ))
        }
        _ => Err(format!("Cannot generate decode closure for {elm_type:?}")),
    }
}

// ── Helpers ─────────────────────────────────────────────────────

fn generic_params(type_vars: &[String]) -> String {
    if type_vars.is_empty() {
        String::new()
    } else {
        let params: Vec<String> = type_vars
            .iter()
            .map(|v| v[0..1].to_uppercase() + &v[1..])
            .collect();
        format!("<{}>", params.join(", "))
    }
}

fn wire3_where_clause(type_vars: &[String]) -> String {
    if type_vars.is_empty() {
        String::new()
    } else {
        let bounds: Vec<String> = type_vars
            .iter()
            .map(|v| {
                let upper = v[0..1].to_uppercase() + &v[1..];
                format!("{upper}: Wire3Encode + Wire3Decode")
            })
            .collect();
        format!("where {} ", bounds.join(", "))
    }
}

fn derives_for_type(body: &ElmType) -> String {
    // Records always get Debug + Clone at minimum
    match body {
        ElmType::Record(_) => "#[derive(Debug, Clone, PartialEq)]".to_string(),
        _ => String::new(),
    }
}

fn derives_for_union(union: &UnionType) -> String {
    // Check if all constructors have no params or simple params
    let has_float = union.constructors.iter().any(|c| {
        c.params.iter().any(|p| matches!(p, ElmType::Named { name, .. } if name == "Float"))
    });
    if has_float {
        "#[derive(Debug, Clone, PartialEq)]".to_string()
    } else {
        "#[derive(Debug, Clone, PartialEq, Eq, Hash)]".to_string()
    }
}

/// Sanitize Elm field names that are Rust keywords.
fn sanitize_field_name(name: &str) -> String {
    match name {
        "type" | "match" | "ref" | "self" | "super" | "crate" | "mod" | "fn" | "let" | "mut"
        | "pub" | "use" | "where" | "async" | "await" | "loop" | "move" | "return" | "static"
        | "struct" | "enum" | "trait" | "impl" | "for" | "in" | "if" | "else" | "while"
        | "break" | "continue" | "extern" | "unsafe" | "dyn" | "abstract" | "become"
        | "box" | "do" | "final" | "macro" | "override" | "priv" | "try" | "typeof"
        | "unsized" | "virtual" | "yield" => format!("r#{name}"),
        _ => name.to_string(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::parse_elm_module;

    #[test]
    fn codegen_simple_record() {
        let source = r#"
module Test exposing (..)

type alias Person =
    { name : String
    , age : Int
    , email : String
    }
"#;
        let module = parse_elm_module(source).unwrap();
        let rust_code = generate_rust_module(&module).unwrap();

        // Should contain the struct with fields in alpha order
        assert!(rust_code.contains("pub struct Person"));
        assert!(rust_code.contains("pub age: ElmInt"));
        assert!(rust_code.contains("pub email: String"));
        assert!(rust_code.contains("pub name: String"));

        // Should contain encoder/decoder
        assert!(rust_code.contains("wire3_encode"));
        assert!(rust_code.contains("wire3_decode"));
    }

    #[test]
    fn codegen_union_type() {
        let source = r#"
module Test exposing (..)

type Color
    = Red
    | Green
    | Blue
    | Custom String Int
"#;
        let module = parse_elm_module(source).unwrap();
        let rust_code = generate_rust_module(&module).unwrap();

        // Constructors should be alphabetically sorted
        assert!(rust_code.contains("pub enum Color"));
        assert!(rust_code.contains("Blue,"));
        assert!(rust_code.contains("Custom(String, ElmInt)"));
        assert!(rust_code.contains("Green,"));
        assert!(rust_code.contains("Red,"));
    }

    #[test]
    fn codegen_type_alias() {
        let source = r#"
module Test exposing (..)

type alias UserId = Int
"#;
        let module = parse_elm_module(source).unwrap();
        let rust_code = generate_rust_module(&module).unwrap();
        assert!(rust_code.contains("pub type UserId = ElmInt;"));
    }

    #[test]
    fn codegen_skips_functions() {
        let source = r#"
module Test exposing (..)

type alias Callback =
    { handler : Int -> String
    , name : String
    }
"#;
        let module = parse_elm_module(source).unwrap();
        let rust_code = generate_rust_module(&module).unwrap();
        assert!(rust_code.contains("Skipped Callback"));
    }
}
