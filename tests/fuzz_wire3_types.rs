// Full bidirectional Wire3 round-trip test for randomly generated Elm types.
//
// This is the ultimate integration test: it generates random type definitions,
// compiles them with BOTH Lamdera (Elm) and our codegen (Rust), then verifies
// that randomly generated instances encode to identical bytes in both directions.
//
// Flow:
//   1. Generate random Elm type definitions (records + unions + aliases)
//   2. Write GenTypes.elm → compile with `lamdera make` → Elm harness
//   3. Parse + codegen → Rust types → compile as a test binary
//   4. Rust→Elm: Rust encodes random instance → Elm decodes+re-encodes → compare
//   5. Elm→Rust: Same bytes sent to Rust → Rust decodes+re-encodes → compare
//
// Requires: `lamdera` and `node` in PATH. Skips gracefully if missing.

mod common;

use common::type_gen::{ElmTypeGen, TypeKind};
use elm_wire3_rs::codegen::generate_rust_module;
use elm_wire3_rs::parser::parse_elm_module;
use elm_wire3_rs::types::*;

use std::fmt::Write as FmtWrite;
use std::io::{BufRead, BufReader, Write};
use std::path::PathBuf;
use std::process::{Command, Stdio};

const NUM_TYPES: usize = 15;
const SEED: u64 = 54321;
const VALUES_PER_TYPE: usize = 20;

// ── Process handle ────────────────────────────────────────────

struct ProcessHandle {
    child: std::process::Child,
    stdin: std::process::ChildStdin,
    reader: BufReader<std::process::ChildStdout>,
}

impl ProcessHandle {
    fn send(&mut self, msg: &str) -> String {
        writeln!(self.stdin, "{msg}").expect("write to process stdin");
        self.stdin.flush().expect("flush process stdin");
        let mut response = String::new();
        self.reader
            .read_line(&mut response)
            .expect("read from process stdout");
        response.trim().to_string()
    }
}

impl Drop for ProcessHandle {
    fn drop(&mut self) {
        let _ = self.child.kill();
    }
}

fn spawn_process(program: &str, args: &[&str], cwd: &std::path::Path) -> ProcessHandle {
    let mut child = Command::new(program)
        .args(args)
        .current_dir(cwd)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .unwrap_or_else(|e| panic!("Failed to spawn {program}: {e}"));
    let stdin = child.stdin.take().unwrap();
    let stdout = child.stdout.take().unwrap();
    ProcessHandle {
        child,
        stdin,
        reader: BufReader::new(stdout),
    }
}

fn command_exists(name: &str) -> bool {
    Command::new("which")
        .arg(name)
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .status()
        .is_ok_and(|s| s.success())
}

// ── Elm code generation ───────────────────────────────────────

fn generate_gen_types_elm(tgen: &mut ElmTypeGen) -> String {
    let mut source = "module GenTypes exposing (..)\n\n".to_string();
    source.push_str("import Dict exposing (Dict)\n");
    source.push_str("import Set exposing (Set)\n\n");
    for _ in 0..NUM_TYPES {
        source.push_str(&tgen.generate_type_def());
        source.push_str("\n\n");
    }
    source
}

fn generate_gen_harness_elm(type_info: &[(&str, TypeKind)]) -> String {
    let mut s = String::new();
    s.push_str("port module GenHarness exposing (main)\n\n");
    s.push_str("import Bytes exposing (Bytes)\n");
    s.push_str("import Bytes.Decode as BD\n");
    s.push_str("import Bytes.Encode as BE\n");
    s.push_str("import GenTypes exposing (..)\n");
    s.push_str("import Lamdera.Wire3 as Wire3\n");
    s.push_str("import Platform\n\n\n");
    s.push_str("port requestPort : (String -> msg) -> Sub msg\n\n\n");
    s.push_str("port responsePort : String -> Cmd msg\n\n\n");
    s.push_str("type Msg\n    = GotRequest String\n\n\n");
    s.push_str("main : Program () () Msg\n");
    s.push_str("main =\n    Platform.worker\n");
    s.push_str("        { init = \\_ -> ( (), Cmd.none )\n");
    s.push_str("        , update = update\n");
    s.push_str("        , subscriptions = \\_ -> requestPort GotRequest\n");
    s.push_str("        }\n\n\n");
    s.push_str("update : Msg -> () -> ( (), Cmd Msg )\n");
    s.push_str("update msg _ =\n    case msg of\n");
    s.push_str("        GotRequest line ->\n");
    s.push_str("            ( (), responsePort (processRequest line) )\n\n\n");
    s.push_str("processRequest : String -> String\n");
    s.push_str("processRequest line =\n    case String.words line of\n");
    s.push_str("        typeName :: hexStr :: _ ->\n");
    s.push_str("            handleType typeName (hexToBytes hexStr)\n\n");
    s.push_str("        _ ->\n            \"ERR invalid-request-format\"\n\n\n");

    // handleType dispatch
    s.push_str("handleType : String -> Bytes -> String\n");
    s.push_str("handleType typeName bytes =\n    case typeName of\n");
    for (name, kind) in type_info {
        if *kind != TypeKind::SimpleAlias {
            let _ = writeln!(
                s,
                "        \"{name}\" ->\n            roundTrip w3_decode_{name} w3_encode_{name} bytes\n"
            );
        }
    }
    s.push_str("        _ ->\n            \"ERR unknown-type:\" ++ typeName\n\n\n");

    // Hex utilities (same as Harness.elm)
    s.push_str(ELM_HEX_UTILS);

    s
}

const ELM_HEX_UTILS: &str = r#"roundTrip : BD.Decoder a -> (a -> BE.Encoder) -> Bytes -> String
roundTrip decoder encoder bytes =
    case BD.decode decoder bytes of
        Nothing ->
            "ERR decode-failed"

        Just value ->
            "OK " ++ bytesToHex (BE.encode (encoder value))


bytesToHex : Bytes -> String
bytesToHex bytes =
    let
        width =
            Bytes.width bytes
    in
    case BD.decode (hexEncodeDecoder width) bytes of
        Just hex ->
            hex

        Nothing ->
            ""


hexEncodeDecoder : Int -> BD.Decoder String
hexEncodeDecoder width =
    BD.loop ( width, [] ) hexEncodeStep


hexEncodeStep : ( Int, List String ) -> BD.Decoder (BD.Step ( Int, List String ) String)
hexEncodeStep ( remaining, acc ) =
    if remaining <= 0 then
        BD.succeed (BD.Done (String.join "" (List.reverse acc)))

    else
        BD.map
            (\byte -> BD.Loop ( remaining - 1, byteToHex byte :: acc ))
            BD.unsignedInt8


byteToHex : Int -> String
byteToHex n =
    String.fromChar (hexDigit (n // 16)) ++ String.fromChar (hexDigit (modBy 16 n))


hexDigit : Int -> Char
hexDigit n =
    case n of
        0 -> '0'
        1 -> '1'
        2 -> '2'
        3 -> '3'
        4 -> '4'
        5 -> '5'
        6 -> '6'
        7 -> '7'
        8 -> '8'
        9 -> '9'
        10 -> 'a'
        11 -> 'b'
        12 -> 'c'
        13 -> 'd'
        14 -> 'e'
        _ -> 'f'


hexToBytes : String -> Bytes
hexToBytes hex =
    BE.encode (BE.sequence (List.map BE.unsignedInt8 (hexPairs (String.toList hex))))


hexPairs : List Char -> List Int
hexPairs chars =
    case chars of
        hi :: lo :: rest ->
            (hexValue hi * 16 + hexValue lo) :: hexPairs rest

        _ ->
            []


hexValue : Char -> Int
hexValue c =
    case c of
        '0' -> 0
        '1' -> 1
        '2' -> 2
        '3' -> 3
        '4' -> 4
        '5' -> 5
        '6' -> 6
        '7' -> 7
        '8' -> 8
        '9' -> 9
        'a' -> 10
        'A' -> 10
        'b' -> 11
        'B' -> 11
        'c' -> 12
        'C' -> 12
        'd' -> 13
        'D' -> 13
        'e' -> 14
        'E' -> 14
        'f' -> 15
        'F' -> 15
        _ -> 0
"#;

fn generate_gen_runner_js() -> &'static str {
    r#"const { Elm } = require('./gen-harness.js');
const app = Elm.GenHarness.init();
app.ports.responsePort.subscribe((response) => {
    process.stdout.write(response + '\n');
});
let buffer = '';
process.stdin.setEncoding('utf8');
process.stdin.on('data', (chunk) => {
    buffer += chunk;
    let lines = buffer.split('\n');
    buffer = lines.pop();
    for (const line of lines) {
        if (line.trim()) app.ports.requestPort.send(line.trim());
    }
});
process.stdin.on('end', () => {
    if (buffer.trim()) app.ports.requestPort.send(buffer.trim());
    setTimeout(() => process.exit(0), 100);
});
"#
}

// ── Rust binary generation ────────────────────────────────────

/// Strip the header (comments, #![allow], use statements) from codegen output,
/// keeping only the type definitions and impls.
fn strip_codegen_header(code: &str) -> &str {
    let lines: Vec<&str> = code.lines().collect();
    let start = lines
        .iter()
        .position(|l| {
            l.starts_with("#[derive")
                || l.starts_with("pub struct")
                || l.starts_with("pub enum")
                || l.starts_with("pub type")
                || l.starts_with("// Skipped")
                || l.starts_with("// Codec")
        })
        .unwrap_or(0);
    let byte_offset: usize = lines[..start].iter().map(|l| l.len() + 1).sum();
    &code[byte_offset..]
}

fn generate_test_binary(
    codegen_output: &str,
    parsed_types: &[ElmTypeDef],
    direct_type_names: &[String],
) -> String {
    let mut s = String::new();

    // Crate-level attributes
    s.push_str("#![allow(dead_code, unused_imports, unreachable_patterns)]\n\n");

    // Imports
    s.push_str("use std::io::{BufRead, BufReader, Write};\n");
    s.push_str("use std::collections::{BTreeMap, BTreeSet};\n");
    s.push_str("use elm_wire3_rs::wire3::{Wire3Encoder, Wire3Decoder, types::*};\n");
    s.push_str("use rand::rngs::SmallRng;\n");
    s.push_str("use rand::{Rng, SeedableRng};\n\n");

    // Type definitions from codegen (stripped of header)
    s.push_str(strip_codegen_header(codegen_output));
    s.push_str("\n");

    // Random value helpers
    s.push_str(RANDOM_HELPERS);
    s.push_str("\n");

    // Per-type random value generators
    for td in parsed_types {
        s.push_str(&gen_random_fn(td));
        s.push_str("\n\n");
    }

    // Process command dispatch
    s.push_str(&gen_process_command(direct_type_names));
    s.push_str("\n");

    // Hex utilities
    s.push_str(RUST_HEX_UTILS);
    s.push_str("\n");

    // Main
    s.push_str(MAIN_FN);

    s
}

const RANDOM_HELPERS: &str = r#"
fn random_int(rng: &mut SmallRng) -> ElmInt {
    let v = match rng.random_range(0..3u32) {
        0 => rng.random_range(-100..100i64),
        1 => rng.random_range(-10000..10000i64),
        _ => rng.random_range(-1000000..1000000i64),
    };
    ElmInt::new(v).unwrap()
}

fn random_float(rng: &mut SmallRng) -> ElmFloat {
    let v: f64 = match rng.random_range(0..3u32) {
        0 => rng.random_range(-100.0..100.0f64),
        1 => rng.random_range(-1e6..1e6f64),
        _ => {
            let bits = rng.random::<u64>();
            let f = f64::from_bits(bits);
            if f.is_finite() { f } else { 0.0 }
        }
    };
    ElmFloat::new(v)
}

fn random_bool(rng: &mut SmallRng) -> bool {
    rng.random_bool(0.5)
}

fn random_string(rng: &mut SmallRng) -> String {
    let len = rng.random_range(0..20u32) as usize;
    (0..len)
        .map(|_| {
            let c = rng.random_range(32..127u32);
            char::from_u32(c).unwrap_or('?')
        })
        .collect()
}
"#;

const RUST_HEX_UTILS: &str = r#"
fn bytes_to_hex(bytes: &[u8]) -> String {
    bytes.iter().map(|b| format!("{b:02x}")).collect()
}

fn hex_to_bytes(hex: &str) -> Vec<u8> {
    (0..hex.len())
        .step_by(2)
        .filter_map(|i| {
            if i + 2 <= hex.len() {
                u8::from_str_radix(&hex[i..i + 2], 16).ok()
            } else {
                None
            }
        })
        .collect()
}
"#;

const MAIN_FN: &str = r#"
fn main() {
    let stdin = std::io::stdin();
    let reader = BufReader::new(stdin.lock());
    let stdout = std::io::stdout();
    let mut writer = stdout.lock();

    for line in reader.lines() {
        let line = line.unwrap();
        if line.trim().is_empty() {
            continue;
        }
        let response = process_command(&line);
        writeln!(writer, "{}", response).unwrap();
        writer.flush().unwrap();
    }
}
"#;

/// Generate the process_command function that dispatches ENCODE/DECODE for each type.
fn gen_process_command(direct_types: &[String]) -> String {
    let mut s = String::new();
    s.push_str("fn process_command(line: &str) -> String {\n");
    s.push_str("    let parts: Vec<&str> = line.split_whitespace().collect();\n");
    s.push_str("    match parts.as_slice() {\n");

    // ENCODE: generate random value and encode it
    s.push_str("        [\"ENCODE\", type_name, seed_str] => {\n");
    s.push_str("            let seed: u64 = seed_str.parse().unwrap_or(0);\n");
    s.push_str("            let mut rng = SmallRng::seed_from_u64(seed);\n");
    s.push_str("            let mut enc = Wire3Encoder::new();\n");
    s.push_str("            match *type_name {\n");
    for name in direct_types {
        let lower = name.to_lowercase();
        let _ = writeln!(
            s,
            "                \"{name}\" => random_{lower}(&mut rng).wire3_encode(&mut enc),"
        );
    }
    s.push_str("                _ => return format!(\"ERR unknown-type:{}\", type_name),\n");
    s.push_str("            }\n");
    s.push_str("            format!(\"OK {}\", bytes_to_hex(&enc.into_bytes()))\n");
    s.push_str("        }\n");

    // DECODE: decode bytes and re-encode
    s.push_str("        [type_name, hex_str] => {\n");
    s.push_str("            let bytes = hex_to_bytes(hex_str);\n");
    s.push_str("            let mut dec = Wire3Decoder::new(&bytes);\n");
    s.push_str("            let mut enc = Wire3Encoder::new();\n");
    s.push_str("            let result = match *type_name {\n");
    for name in direct_types {
        let _ = writeln!(
            s,
            "                \"{name}\" => {name}::wire3_decode(&mut dec).map(|v| v.wire3_encode(&mut enc)),"
        );
    }
    s.push_str(
        "                _ => return format!(\"ERR unknown-type:{}\", type_name),\n",
    );
    s.push_str("            };\n");
    s.push_str("            match result {\n");
    s.push_str(
        "                Ok(_) => format!(\"OK {}\", bytes_to_hex(&enc.into_bytes())),\n",
    );
    s.push_str("                Err(e) => format!(\"ERR {:?}\", e),\n");
    s.push_str("            }\n");
    s.push_str("        }\n");

    s.push_str("        _ => \"ERR invalid-command\".to_string(),\n");
    s.push_str("    }\n");
    s.push_str("}\n");
    s
}

/// Generate a `fn random_genN(rng: &mut SmallRng) -> GenN { ... }` for each type def.
fn gen_random_fn(td: &ElmTypeDef) -> String {
    let name = td.name();
    let lower = name.to_lowercase();
    match td {
        ElmTypeDef::Alias(alias) => match &alias.body {
            ElmType::Record(fields) => {
                let mut sorted: Vec<&RecordField> = fields.iter().collect();
                sorted.sort_by_key(|f| &f.name);
                let inits: Vec<String> = sorted
                    .iter()
                    .map(|f| format!("        {}: {},", sanitize_field(&f.name), gen_rand_expr(&f.type_)))
                    .collect();
                format!(
                    "fn random_{lower}(rng: &mut SmallRng) -> {name} {{\n    {name} {{\n{}\n    }}\n}}",
                    inits.join("\n")
                )
            }
            _ => {
                // Simple alias — generate the underlying type
                let expr = gen_rand_expr(&alias.body);
                format!("fn random_{lower}(rng: &mut SmallRng) -> {name} {{ {expr} }}")
            }
        },
        ElmTypeDef::Union(union) => {
            let mut sorted: Vec<&Constructor> = union.constructors.iter().collect();
            sorted.sort_by_key(|c| &c.name);
            let n = sorted.len();
            let arms: Vec<String> = sorted
                .iter()
                .enumerate()
                .map(|(i, ctor)| {
                    if ctor.params.is_empty() {
                        format!("        {i} => {name}::{},", ctor.name)
                    } else {
                        let params: Vec<String> =
                            ctor.params.iter().map(|p| gen_rand_expr(p)).collect();
                        format!(
                            "        {i} => {name}::{}({}),",
                            ctor.name,
                            params.join(", ")
                        )
                    }
                })
                .collect();
            format!(
                "fn random_{lower}(rng: &mut SmallRng) -> {name} {{\n    match rng.random_range(0..{n}u32) {{\n{}\n        _ => unreachable!(),\n    }}\n}}",
                arms.join("\n")
            )
        }
    }
}

/// Generate a Rust expression that produces a random value of the given Elm type.
fn gen_rand_expr(elm_type: &ElmType) -> String {
    match elm_type {
        ElmType::Named {
            module, name, args, ..
        } => match (module.as_deref(), name.as_str()) {
            (_, "Int") => "random_int(rng)".to_string(),
            (_, "Float") => "random_float(rng)".to_string(),
            (_, "Bool") => "random_bool(rng)".to_string(),
            (_, "String") => "random_string(rng)".to_string(),
            (_, "List") | (_, "Array") if args.len() == 1 => {
                let inner = gen_rand_expr(&args[0]);
                format!(
                    "{{ let n = rng.random_range(0..4u32) as usize; (0..n).map(|_| {inner}).collect::<Vec<_>>() }}"
                )
            }
            (_, "Set") if args.len() == 1 => {
                let inner = gen_rand_expr(&args[0]);
                format!(
                    "{{ let n = rng.random_range(0..4u32) as usize; (0..n).map(|_| {inner}).collect::<BTreeSet<_>>() }}"
                )
            }
            (_, "Dict") if args.len() == 2 => {
                let k = gen_rand_expr(&args[0]);
                let v = gen_rand_expr(&args[1]);
                format!(
                    "{{ let n = rng.random_range(0..4u32) as usize; (0..n).map(|_| ({k}, {v})).collect::<BTreeMap<_, _>>() }}"
                )
            }
            (_, "Maybe") if args.len() == 1 => {
                let inner = gen_rand_expr(&args[0]);
                format!("if random_bool(rng) {{ Some({inner}) }} else {{ None }}")
            }
            (_, "Result") if args.len() == 2 => {
                let err = gen_rand_expr(&args[0]);
                let ok = gen_rand_expr(&args[1]);
                format!("if random_bool(rng) {{ Ok({ok}) }} else {{ Err({err}) }}")
            }
            // User-defined type
            (_, user_type) => format!("random_{}(rng)", user_type.to_lowercase()),
        },
        ElmType::Tuple(elems) if elems.len() == 2 => {
            let a = gen_rand_expr(&elems[0]);
            let b = gen_rand_expr(&elems[1]);
            format!("({a}, {b})")
        }
        ElmType::Tuple(elems) if elems.len() == 3 => {
            let a = gen_rand_expr(&elems[0]);
            let b = gen_rand_expr(&elems[1]);
            let c = gen_rand_expr(&elems[2]);
            format!("({a}, {b}, {c})")
        }
        ElmType::Unit => "()".to_string(),
        _ => "()".to_string(),
    }
}

/// Sanitize Elm field names that are Rust keywords.
fn sanitize_field(name: &str) -> String {
    match name {
        "type" | "match" | "ref" | "self" | "super" | "crate" | "mod" | "fn" | "let" | "mut"
        | "pub" | "use" | "where" | "async" | "await" | "loop" | "move" | "return" | "static"
        | "struct" | "enum" | "trait" | "impl" | "for" | "in" | "if" | "else" | "while"
        | "break" | "continue" | "extern" | "unsafe" | "dyn" | "abstract" | "become" | "box"
        | "do" | "final" | "macro" | "override" | "priv" | "try" | "typeof" | "unsized"
        | "virtual" | "yield" => format!("r#{name}"),
        _ => name.to_string(),
    }
}

// ── Cleanup ───────────────────────────────────────────────────

struct CleanupGuard {
    files: Vec<PathBuf>,
    dirs: Vec<PathBuf>,
}

impl Drop for CleanupGuard {
    fn drop(&mut self) {
        for f in &self.files {
            let _ = std::fs::remove_file(f);
        }
        for d in &self.dirs {
            let _ = std::fs::remove_dir_all(d);
        }
    }
}

// ── Main test ─────────────────────────────────────────────────

#[test]
fn wire3_round_trip_random_types() {
    let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let harness_dir = manifest_dir.join("elm-harness");

    // Check prerequisites
    if !command_exists("lamdera") {
        eprintln!("Skipping wire3_round_trip_random_types: lamdera not found in PATH");
        return;
    }
    if !command_exists("node") {
        eprintln!("Skipping wire3_round_trip_random_types: node not found in PATH");
        return;
    }

    // 1. Generate random types
    let mut tgen = ElmTypeGen::new(SEED);
    let elm_source = generate_gen_types_elm(&mut tgen);

    // Identify which types are directly testable (record or union)
    let direct_types: Vec<String> = tgen
        .available
        .iter()
        .zip(tgen.type_kinds.iter())
        .filter(|(_, k)| **k != TypeKind::SimpleAlias)
        .map(|(n, _)| n.clone())
        .collect();

    eprintln!(
        "Generated {} types ({} direct: records/unions)",
        tgen.available.len(),
        direct_types.len()
    );

    // 2. Parse the Elm source and generate Rust code
    // Use "GenTypes" as the module name for parsing
    let elm_for_parse = elm_source.replace("module Generated", "module GenTypes");
    let module =
        parse_elm_module(&elm_for_parse).unwrap_or_else(|e| panic!("Parse failed:\n{elm_source}\nError: {e}"));
    let rust_code = generate_rust_module(&module)
        .unwrap_or_else(|e| panic!("Codegen failed:\n{elm_source}\nError: {e}"));

    // 3. Write Elm files
    let gen_types_path = harness_dir.join("src/GenTypes.elm");
    let gen_harness_path = harness_dir.join("src/GenHarness.elm");
    let gen_runner_path = harness_dir.join("gen-runner.js");
    let gen_js_path = harness_dir.join("gen-harness.js");
    let tmp_dir = manifest_dir.join("target/gen-wire3-test");

    let _cleanup = CleanupGuard {
        files: vec![
            gen_types_path.clone(),
            gen_harness_path.clone(),
            gen_runner_path.clone(),
            gen_js_path.clone(),
        ],
        dirs: vec![tmp_dir.clone()],
    };

    // Write GenTypes.elm with correct module name
    std::fs::write(&gen_types_path, elm_source.replace("module Generated", "module GenTypes"))
        .expect("write GenTypes.elm");

    let type_info: Vec<(&str, TypeKind)> = tgen
        .available
        .iter()
        .zip(tgen.type_kinds.iter())
        .map(|(n, k)| (n.as_str(), *k))
        .collect();
    std::fs::write(&gen_harness_path, generate_gen_harness_elm(&type_info))
        .expect("write GenHarness.elm");
    std::fs::write(&gen_runner_path, generate_gen_runner_js()).expect("write gen-runner.js");

    // 4. Compile Elm with lamdera
    eprintln!("Compiling Elm harness with lamdera...");
    let output = Command::new("lamdera")
        .args(["make", "src/GenHarness.elm", "--output=gen-harness.js"])
        .current_dir(&harness_dir)
        .output()
        .expect("run lamdera make");

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        let stdout = String::from_utf8_lossy(&output.stdout);
        let gen_types = std::fs::read_to_string(&gen_types_path).unwrap_or_default();
        let gen_harness = std::fs::read_to_string(&gen_harness_path).unwrap_or_default();
        panic!(
            "lamdera make failed!\n\n\
             === GenTypes.elm ===\n{gen_types}\n\n\
             === GenHarness.elm ===\n{gen_harness}\n\n\
             === stderr ===\n{stderr}\n\n\
             === stdout ===\n{stdout}"
        );
    }

    // 5. Generate and compile Rust test binary
    let binary_code = generate_test_binary(&rust_code, &module.types, &direct_types);

    let _ = std::fs::remove_dir_all(&tmp_dir);
    std::fs::create_dir_all(tmp_dir.join("src")).expect("create temp dir");

    std::fs::write(
        tmp_dir.join("Cargo.toml"),
        format!(
            r#"[package]
name = "gen-wire3-test"
version = "0.1.0"
edition = "2024"

[dependencies]
elm-wire3-rs = {{ path = "{}" }}
rand = "0.9"
"#,
            manifest_dir.display()
        ),
    )
    .expect("write Cargo.toml");

    std::fs::write(tmp_dir.join("src/main.rs"), &binary_code).expect("write main.rs");

    eprintln!("Compiling Rust test binary...");
    let output = Command::new("cargo")
        .args(["build", "--quiet"])
        .current_dir(&tmp_dir)
        .output()
        .expect("run cargo build");

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        let numbered: String = binary_code
            .lines()
            .enumerate()
            .map(|(i, l)| format!("{:4} | {l}", i + 1))
            .collect::<Vec<_>>()
            .join("\n");
        panic!(
            "Rust binary compilation failed!\n\n\
             === Generated source ===\n{numbered}\n\n\
             === Compiler errors ===\n{stderr}"
        );
    }

    // 6. Spawn both processes
    let binary_path = tmp_dir.join("target/debug/gen-wire3-test");
    let mut rust_proc = spawn_process(binary_path.to_str().unwrap(), &[], &tmp_dir);
    let mut elm_proc = spawn_process("node", &["gen-runner.js"], &harness_dir);

    // 7. Run bidirectional round-trip tests
    let mut passed = 0;
    let mut failed = 0;
    let mut failures: Vec<String> = Vec::new();

    for type_name in &direct_types {
        for seed in 0..VALUES_PER_TYPE as u64 {
            // ── Rust→Elm direction ──
            // Rust generates random instance, encodes it
            let rust_response = rust_proc.send(&format!("ENCODE {type_name} {seed}"));
            let rust_hex = match rust_response.strip_prefix("OK ") {
                Some(h) => h.to_string(),
                None => {
                    let msg =
                        format!("Rust ENCODE {type_name} seed={seed} failed: {rust_response}");
                    eprintln!("FAIL: {msg}");
                    failures.push(msg);
                    failed += 1;
                    continue;
                }
            };

            // Elm decodes + re-encodes
            let elm_response = elm_proc.send(&format!("{type_name} {rust_hex}"));
            let elm_hex = match elm_response.strip_prefix("OK ") {
                Some(h) => h.to_string(),
                None => {
                    let msg = format!(
                        "Elm decode {type_name} seed={seed} failed: {elm_response}\n  Rust bytes: {rust_hex}"
                    );
                    eprintln!("FAIL: {msg}");
                    failures.push(msg);
                    failed += 1;
                    continue;
                }
            };

            // Compare Rust encoding vs Elm encoding
            if rust_hex != elm_hex {
                let msg = format!(
                    "Rust→Elm MISMATCH {type_name} seed={seed}\n  Rust: {rust_hex}\n  Elm:  {elm_hex}"
                );
                eprintln!("FAIL: {msg}");
                failures.push(msg);
                failed += 1;
                continue;
            }

            // ── Elm→Rust direction ──
            // Send the same (now verified) bytes to Rust for decode+re-encode
            let rust_rt = rust_proc.send(&format!("{type_name} {elm_hex}"));
            let rust_rt_hex = match rust_rt.strip_prefix("OK ") {
                Some(h) => h.to_string(),
                None => {
                    let msg = format!(
                        "Rust DECODE {type_name} seed={seed} failed: {rust_rt}\n  Bytes: {elm_hex}"
                    );
                    eprintln!("FAIL: {msg}");
                    failures.push(msg);
                    failed += 1;
                    continue;
                }
            };

            // Compare Rust decode+re-encode vs original
            if elm_hex != rust_rt_hex {
                let msg = format!(
                    "Elm→Rust MISMATCH {type_name} seed={seed}\n  Original: {elm_hex}\n  Rust RT:  {rust_rt_hex}"
                );
                eprintln!("FAIL: {msg}");
                failures.push(msg);
                failed += 1;
                continue;
            }

            passed += 1;
        }
    }

    // 8. Report results
    eprintln!(
        "\nWire3 random type round-trip: {passed} passed, {failed} failed \
         ({} types × {VALUES_PER_TYPE} values/type)",
        direct_types.len()
    );

    if !failures.is_empty() {
        let fail_summary = failures
            .iter()
            .take(10)
            .cloned()
            .collect::<Vec<_>>()
            .join("\n\n");
        panic!(
            "{failed} round-trip failures (showing first {}):\n\n{fail_summary}",
            failures.len().min(10)
        );
    }
}
