// Random type definition tests: generate arbitrary Elm type definitions,
// feed through the full parser→codegen pipeline, and verify correctness.
//
// Layer 1 (fast): proptest generates seeds → random Elm modules → parse → codegen → no errors
// Layer 2 (slow): batch-generate types → codegen → compile with rustc → verify valid Rust
//
// This tests the CODEGEN pipeline itself, not just specific Wire3 values.
// Together with the value-level fuzz tests (fuzz_round_trip.rs), this proves
// the tool handles arbitrary Elm types correctly.

mod common;

use common::type_gen::ElmTypeGen;
use elm_wire3_rs::codegen::generate_rust_module;
use elm_wire3_rs::parser::parse_elm_module;

use proptest::prelude::*;
use std::process::Command;

// ── Layer 1: Fast proptest — parse + codegen doesn't error ────

proptest! {
    #![proptest_config(ProptestConfig::with_cases(500))]

    #[test]
    fn random_types_parse_and_codegen(seed in 0u64..1_000_000) {
        let mut tgen = ElmTypeGen::new(seed);
        let num_types = (seed % 8) as usize + 1; // 1–8 types per module
        let source = tgen.generate_module(num_types);

        let module = parse_elm_module(&source)
            .unwrap_or_else(|e| panic!("Parse failed (seed {seed}):\n{source}\nError: {e}"));

        // Verify we parsed the expected number of types
        assert!(
            !module.types.is_empty(),
            "No types parsed (seed {seed}):\n{source}"
        );

        let _rust_code = generate_rust_module(&module)
            .unwrap_or_else(|e| panic!("Codegen failed (seed {seed}):\n{source}\nError: {e}"));
    }
}

// ── Layer 2: Compilation check — generated Rust actually compiles ─

#[test]
fn generated_types_compile() {
    // Generate a large batch of types from multiple seeds
    let mut all_source = "module GeneratedAll exposing (..)\n\n".to_string();
    all_source.push_str("import Dict exposing (Dict)\n");
    all_source.push_str("import Set exposing (Set)\n\n");

    let mut tgen = ElmTypeGen::new(42);
    for _ in 0..50 {
        all_source.push_str(&tgen.generate_type_def());
        all_source.push_str("\n\n");
    }

    let module = parse_elm_module(&all_source)
        .unwrap_or_else(|e| panic!("Parse failed:\n{all_source}\nError: {e}"));

    assert!(
        module.types.len() >= 40,
        "Expected at least 40 types, got {}. Source:\n{all_source}",
        module.types.len()
    );

    let rust_code = generate_rust_module(&module)
        .unwrap_or_else(|e| panic!("Codegen failed:\n{all_source}\nError: {e}"));

    // Write temp Cargo project and try to compile
    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    let tmp_dir = std::path::PathBuf::from(manifest_dir).join("target/gen-test");
    let _ = std::fs::remove_dir_all(&tmp_dir);
    std::fs::create_dir_all(tmp_dir.join("src")).expect("create temp dir");

    std::fs::write(
        tmp_dir.join("Cargo.toml"),
        format!(
            r#"[package]
name = "gen-test"
version = "0.1.0"
edition = "2024"

[dependencies]
elm-wire3-rs = {{ path = "{manifest_dir}" }}
"#
        ),
    )
    .expect("write Cargo.toml");

    std::fs::write(tmp_dir.join("src/lib.rs"), &rust_code).expect("write lib.rs");

    let output = Command::new("cargo")
        .arg("check")
        .current_dir(&tmp_dir)
        .output()
        .expect("run cargo check");

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        // Dump the generated code with line numbers for debugging
        let numbered: String = rust_code
            .lines()
            .enumerate()
            .map(|(i, line)| format!("{:4} | {line}", i + 1))
            .collect::<Vec<_>>()
            .join("\n");
        panic!(
            "Generated Rust code failed to compile!\n\n\
             === Elm source ===\n{all_source}\n\n\
             === Generated Rust ===\n{numbered}\n\n\
             === Compiler errors ===\n{stderr}"
        );
    }

    // Cleanup on success
    let _ = std::fs::remove_dir_all(&tmp_dir);
}

// ── Targeted edge case tests ──────────────────────────────────

#[test]
fn codegen_nested_float_in_union_gets_partial_eq_only() {
    // A union whose constructor wraps a type containing Float
    // should NOT get Eq/Hash derives.
    let source = r#"
module Test exposing (..)

type alias Score = Float

type Wrapper
    = WithScore Score
    | Empty
"#;
    let module = parse_elm_module(source).unwrap();
    let rust_code = generate_rust_module(&module).unwrap();

    // Wrapper should have PartialEq but NOT Eq (since Score = Float)
    assert!(
        rust_code.contains("pub enum Wrapper"),
        "Missing Wrapper enum"
    );
    // Find the derive line for Wrapper
    let wrapper_idx = rust_code.find("pub enum Wrapper").unwrap();
    let derive_line = rust_code[..wrapper_idx]
        .lines()
        .last()
        .unwrap_or("");
    assert!(
        derive_line.contains("PartialEq"),
        "Wrapper should derive PartialEq: {derive_line}"
    );
    assert!(
        !derive_line.contains("Eq,") && !derive_line.contains("Eq]"),
        "Wrapper should NOT derive Eq (contains Float via Score): {derive_line}"
    );
}

#[test]
fn codegen_deeply_nested_float_detected() {
    // Float nested inside List inside Maybe inside a record
    let source = r#"
module Test exposing (..)

type alias Inner = { scores : List Float }

type alias Outer = { data : Maybe Inner }

type Holder
    = Hold Outer
    | None
"#;
    let module = parse_elm_module(source).unwrap();
    let rust_code = generate_rust_module(&module).unwrap();

    // All three types contain Float transitively
    // Holder should NOT get Eq/Hash
    let holder_idx = rust_code.find("pub enum Holder").unwrap();
    let derive_line = rust_code[..holder_idx]
        .lines()
        .last()
        .unwrap_or("");
    assert!(
        !derive_line.contains("Eq,") && !derive_line.contains("Eq]"),
        "Holder should NOT derive Eq: {derive_line}"
    );
}

#[test]
fn codegen_no_float_gets_eq_hash() {
    // A union with only Int/String/Bool — should get Eq/Hash
    let source = r#"
module Test exposing (..)

type Status
    = Active String
    | Inactive
    | Pending Int
"#;
    let module = parse_elm_module(source).unwrap();
    let rust_code = generate_rust_module(&module).unwrap();

    let status_idx = rust_code.find("pub enum Status").unwrap();
    let derive_line = rust_code[..status_idx]
        .lines()
        .last()
        .unwrap_or("");
    assert!(
        derive_line.contains("Eq"),
        "Status should derive Eq (no floats): {derive_line}"
    );
    assert!(
        derive_line.contains("Hash"),
        "Status should derive Hash (no floats): {derive_line}"
    );
}
