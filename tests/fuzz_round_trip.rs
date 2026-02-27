// Fuzz round-trip tests: proptest generates random values, Rust encodes them,
// Elm decodes+re-encodes, and we verify byte-for-byte equality.
//
// Uses proptest strategies to explore the full value space for each type.

use std::collections::{BTreeMap, BTreeSet};
use std::io::{BufRead, BufReader, Write};
use std::process::{Command, Stdio};

use elm_wire3_rs::wire3::encode::{Wire3Encoder, elm_str_cmp};
use elm_wire3_rs::wire3::types::*;

use proptest::prelude::*;
use proptest::strategy::ValueTree;
use proptest::test_runner::{Config, TestRunner};

// ── Elm harness (same as round_trip.rs) ──────────────────────────

struct ElmHarness {
    child: std::process::Child,
    stdin: std::process::ChildStdin,
    reader: BufReader<std::process::ChildStdout>,
}

impl ElmHarness {
    fn new() -> Self {
        let harness_dir = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("elm-harness");
        let mut child = Command::new("node")
            .arg("runner.js")
            .current_dir(&harness_dir)
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .expect("Failed to spawn Elm harness");
        let stdin = child.stdin.take().unwrap();
        let stdout = child.stdout.take().unwrap();
        ElmHarness {
            child,
            stdin,
            reader: BufReader::new(stdout),
        }
    }

    fn round_trip(&mut self, type_name: &str, encoded_bytes: &[u8]) -> RoundTripResult {
        let hex = bytes_to_hex(encoded_bytes);
        let request = format!("{type_name} {hex}\n");
        self.stdin.write_all(request.as_bytes()).expect("write");
        self.stdin.flush().expect("flush");

        let mut response = String::new();
        self.reader.read_line(&mut response).expect("read");
        let response = response.trim();

        if let Some(hex_result) = response.strip_prefix("OK ") {
            let result_bytes = hex_to_bytes(hex_result);
            if result_bytes == encoded_bytes {
                RoundTripResult::Match
            } else {
                RoundTripResult::Mismatch {
                    sent: encoded_bytes.to_vec(),
                    received: result_bytes,
                }
            }
        } else if let Some(err) = response.strip_prefix("ERR ") {
            RoundTripResult::ElmError(err.to_string())
        } else {
            RoundTripResult::ElmError(format!("unexpected: {response}"))
        }
    }
}

impl Drop for ElmHarness {
    fn drop(&mut self) {
        let _ = self.child.kill();
    }
}

#[derive(Debug)]
enum RoundTripResult {
    Match,
    Mismatch { sent: Vec<u8>, received: Vec<u8> },
    ElmError(String),
}

impl RoundTripResult {
    fn assert_match(&self, context: &str) {
        match self {
            RoundTripResult::Match => {}
            RoundTripResult::Mismatch { sent, received } => {
                panic!(
                    "Wire3 mismatch for {context}!\n  Sent:     {}\n  Received: {}",
                    bytes_to_hex(sent),
                    bytes_to_hex(received)
                );
            }
            RoundTripResult::ElmError(err) => {
                panic!("Elm error for {context}: {err}");
            }
        }
    }
}

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

// ── Value generation helper ──────────────────────────────────────

fn generate_values<T: std::fmt::Debug>(strategy: impl Strategy<Value = T>, count: usize) -> Vec<T> {
    let mut runner = TestRunner::new(Config::with_cases(count as u32));
    (0..count)
        .map(|_| {
            let tree = strategy.new_tree(&mut runner).unwrap();
            tree.current()
        })
        .collect()
}

// ── Proptest strategies ──────────────────────────────────────────

fn elm_int_strategy() -> impl Strategy<Value = i64> {
    prop_oneof![
        // Small values (most common in practice, hit 1-2 byte encoding)
        -1000i64..1000,
        // Medium values (2-3 byte varint)
        -70_000i64..70_000,
        // Large values (4-5 byte varint)
        -3_000_000_000i64..3_000_000_000,
        // Full Elm range (may hit 9-byte float64 escape)
        (ElmInt::MIN..=ElmInt::MAX),
        // Exact boundary values
        prop_oneof![
            Just(0i64),
            Just(107), Just(-108),       // 1-byte boundary
            Just(108), Just(-109),       // 2-byte start
            Just(4715), Just(-4716),     // 2-byte boundary
            Just(4716), Just(-4717),     // 3-byte start
            Just(65535), Just(-65536),   // near u16 max
            Just(8_388_607),             // 3-byte boundary
            Just(8_388_608),             // 4-byte start
            Just(2_147_483_647i64),      // near i32 max
            Just(2_147_483_648i64),      // 5-byte start
            Just(4_294_967_295i64),      // u32 max boundary
            Just(4_294_967_296i64),      // 9-byte start
            Just(ElmInt::MAX),
            Just(ElmInt::MIN),
        ],
    ]
}

fn elm_float_strategy() -> impl Strategy<Value = f64> {
    prop_oneof![
        // Normal finite floats (filter NaN/Infinity for deterministic comparison)
        any::<f64>().prop_filter("finite", |f| f.is_finite()),
        // Common values
        prop_oneof![
            Just(0.0f64),
            Just(-0.0),
            Just(1.0),
            Just(-1.0),
            Just(0.5),
            Just(std::f64::consts::PI),
            Just(f64::EPSILON),
            Just(f64::MAX),
            Just(f64::MIN),
            Just(f64::MIN_POSITIVE),
        ],
    ]
}

fn short_string_strategy() -> impl Strategy<Value = String> {
    prop_oneof![
        // ASCII strings (most common)
        "[a-zA-Z0-9 ]{0,50}",
        // Unicode strings
        "\\PC{0,20}",
        // Empty
        Just(String::new()),
    ]
}


// ── Fuzz tests ───────────────────────────────────────────────────

const FUZZ_CASES: usize = 200;

#[test]
fn fuzz_int() {
    let mut harness = ElmHarness::new();
    let values = generate_values(elm_int_strategy(), FUZZ_CASES);
    for v in values {
        let elm_v = ElmInt::new(v).unwrap();
        let mut enc = Wire3Encoder::new();
        enc.encode_int(&elm_v);
        harness
            .round_trip("Int", &enc.into_bytes())
            .assert_match(&format!("Int({v})"));
    }
}

#[test]
fn fuzz_float() {
    let mut harness = ElmHarness::new();
    let values = generate_values(elm_float_strategy(), FUZZ_CASES);
    for v in values {
        let mut enc = Wire3Encoder::new();
        enc.encode_float(&ElmFloat::new(v));
        harness
            .round_trip("Float", &enc.into_bytes())
            .assert_match(&format!("Float({v})"));
    }
}

#[test]
fn fuzz_string() {
    let mut harness = ElmHarness::new();
    let values = generate_values(short_string_strategy(), FUZZ_CASES);
    for v in &values {
        let mut enc = Wire3Encoder::new();
        enc.encode_string(v);
        harness
            .round_trip("String", &enc.into_bytes())
            .assert_match(&format!("String({v:?})"));
    }
}

#[test]
fn fuzz_bool() {
    let mut harness = ElmHarness::new();
    let values = generate_values(any::<bool>(), FUZZ_CASES);
    for v in values {
        let mut enc = Wire3Encoder::new();
        enc.encode_bool(v);
        harness
            .round_trip("Bool", &enc.into_bytes())
            .assert_match(&format!("Bool({v})"));
    }
}

#[test]
fn fuzz_color() {
    let mut harness = ElmHarness::new();
    // Color: Blue=0, Green=1, Red=2 (alphabetical)
    let values = generate_values(0u8..3, FUZZ_CASES);
    for tag in values {
        let mut enc = Wire3Encoder::new();
        enc.encode_tag8(tag);
        let name = match tag {
            0 => "Blue",
            1 => "Green",
            _ => "Red",
        };
        harness
            .round_trip("Color", &enc.into_bytes())
            .assert_match(&format!("Color({name})"));
    }
}

#[test]
fn fuzz_shape() {
    let mut harness = ElmHarness::new();
    // Shape: Circle=0, Point=1, Rectangle=2 (alphabetical)
    let tags = generate_values(0u8..3, FUZZ_CASES);
    let floats = generate_values(elm_float_strategy(), FUZZ_CASES * 2);
    let mut fi = 0;

    for tag in tags {
        let mut enc = Wire3Encoder::new();
        match tag {
            0 => {
                // Circle Float
                enc.encode_tag8(0);
                enc.encode_float(&ElmFloat::new(floats[fi % floats.len()]));
                fi += 1;
            }
            1 => {
                // Point
                enc.encode_tag8(1);
            }
            _ => {
                // Rectangle Float Float
                enc.encode_tag8(2);
                enc.encode_float(&ElmFloat::new(floats[fi % floats.len()]));
                fi += 1;
                enc.encode_float(&ElmFloat::new(floats[fi % floats.len()]));
                fi += 1;
            }
        }
        harness
            .round_trip("Shape", &enc.into_bytes())
            .assert_match(&format!("Shape(tag={tag})"));
    }
}

#[test]
fn fuzz_person() {
    let mut harness = ElmHarness::new();
    // Person: active(Bool), age(Int), name(String), score(Float) — alphabetical
    let bools = generate_values(any::<bool>(), FUZZ_CASES);
    let ints = generate_values(elm_int_strategy(), FUZZ_CASES);
    let strings = generate_values(short_string_strategy(), FUZZ_CASES);
    let floats = generate_values(elm_float_strategy(), FUZZ_CASES);

    for i in 0..FUZZ_CASES {
        let mut enc = Wire3Encoder::new();
        enc.encode_bool(bools[i]);                                      // active
        enc.encode_int(&ElmInt::new(ints[i]).unwrap());                 // age
        enc.encode_string(&strings[i]);                                  // name
        enc.encode_float(&ElmFloat::new(floats[i]));                    // score
        harness
            .round_trip("Person", &enc.into_bytes())
            .assert_match(&format!("Person(i={i})"));
    }
}

#[test]
fn fuzz_inventory() {
    let mut harness = ElmHarness::new();
    // Inventory: counts(Dict String Int), items(List String), selected(Maybe String), tags(Set String)
    // — alphabetical field order

    let list_lens = generate_values(0usize..5, FUZZ_CASES);
    let dict_lens = generate_values(0usize..4, FUZZ_CASES);
    let set_lens = generate_values(0usize..4, FUZZ_CASES);
    let use_just = generate_values(any::<bool>(), FUZZ_CASES);
    let strings = generate_values(short_string_strategy(), FUZZ_CASES * 10);
    let ints = generate_values(elm_int_strategy(), FUZZ_CASES * 5);
    let mut si = 0;
    let mut ii = 0;

    for i in 0..FUZZ_CASES {
        let mut enc = Wire3Encoder::new();

        // counts: Dict String Int — sorted by Elm's UTF-16 code unit ordering
        let dict_len = dict_lens[i];
        let mut dict: BTreeMap<String, i64> = BTreeMap::new();
        for _ in 0..dict_len {
            let k = strings[si % strings.len()].clone();
            si += 1;
            let v = ints[ii % ints.len()];
            ii += 1;
            dict.insert(k, v);
        }
        // Re-sort by Elm's string ordering (UTF-16 code units)
        let mut dict_sorted: Vec<(&String, &i64)> = dict.iter().collect();
        dict_sorted.sort_by(|a, b| elm_str_cmp(a.0, b.0));
        enc.encode_int_raw(dict_sorted.len() as i64);
        for (k, v) in &dict_sorted {
            enc.encode_string(k);
            enc.encode_int(&ElmInt::new(**v).unwrap());
        }

        // items: List String
        let list_len = list_lens[i];
        enc.encode_int_raw(list_len as i64);
        for _ in 0..list_len {
            enc.encode_string(&strings[si % strings.len()]);
            si += 1;
        }

        // selected: Maybe String
        if use_just[i] {
            enc.encode_tag8(1); // Just
            enc.encode_string(&strings[si % strings.len()]);
            si += 1;
        } else {
            enc.encode_tag8(0); // Nothing
        }

        // tags: Set String — sorted by Elm's UTF-16 code unit ordering
        let set_len = set_lens[i];
        let mut set: BTreeSet<String> = BTreeSet::new();
        for _ in 0..set_len {
            set.insert(strings[si % strings.len()].clone());
            si += 1;
        }
        // Re-sort by Elm's string ordering (UTF-16 code units)
        let mut set_sorted: Vec<&String> = set.iter().collect();
        set_sorted.sort_by(|a, b| elm_str_cmp(a, b));
        enc.encode_int_raw(set_sorted.len() as i64);
        for s in &set_sorted {
            enc.encode_string(s);
        }

        harness
            .round_trip("Inventory", &enc.into_bytes())
            .assert_match(&format!("Inventory(i={i})"));
    }
}

#[test]
fn fuzz_api_response() {
    let mut harness = ElmHarness::new();
    // ApiResponse: message(String), result(Result String Int) — alphabetical
    // Result tags: Ok=0, Err=1 (Wire3.elm hardcoded)

    let strings = generate_values(short_string_strategy(), FUZZ_CASES * 2);
    let ints = generate_values(elm_int_strategy(), FUZZ_CASES);
    let is_ok = generate_values(any::<bool>(), FUZZ_CASES);
    let mut si = 0;

    for i in 0..FUZZ_CASES {
        let mut enc = Wire3Encoder::new();

        // message
        enc.encode_string(&strings[si % strings.len()]);
        si += 1;

        // result
        if is_ok[i] {
            enc.encode_tag8(0); // Ok
            enc.encode_int(&ElmInt::new(ints[i]).unwrap());
        } else {
            enc.encode_tag8(1); // Err
            enc.encode_string(&strings[si % strings.len()]);
            si += 1;
        }

        harness
            .round_trip("ApiResponse", &enc.into_bytes())
            .assert_match(&format!("ApiResponse(i={i})"));
    }
}

#[test]
fn fuzz_tree() {
    let mut harness = ElmHarness::new();
    // Tree: Branch=0, Leaf=1 (alphabetical)
    // Use limited depth to avoid huge encodings

    let ints = generate_values(elm_int_strategy(), FUZZ_CASES * 7);
    // Generate tree shapes as vectors of choices (0=branch, 1=leaf)
    // Max depth 4 to keep things reasonable
    let depths = generate_values(0u8..5, FUZZ_CASES);
    let mut ii = 0;

    for i in 0..FUZZ_CASES {
        let mut enc = Wire3Encoder::new();
        let depth = depths[i] as usize;
        encode_random_tree(&mut enc, depth, &ints, &mut ii);
        harness
            .round_trip("Tree", &enc.into_bytes())
            .assert_match(&format!("Tree(i={i}, depth={depth})"));
    }
}

fn encode_random_tree(enc: &mut Wire3Encoder, depth: usize, ints: &[i64], ii: &mut usize) {
    if depth == 0 {
        // Leaf
        enc.encode_tag8(1);
        enc.encode_int(&ElmInt::new(ints[*ii % ints.len()]).unwrap());
        *ii += 1;
    } else {
        // Branch
        enc.encode_tag8(0);
        encode_random_tree(enc, depth - 1, ints, ii);
        encode_random_tree(enc, depth - 1, ints, ii);
    }
}

#[test]
fn fuzz_coordinate() {
    let mut harness = ElmHarness::new();
    // Coordinate: label(String), point(Float, Float) — alphabetical
    let strings = generate_values(short_string_strategy(), FUZZ_CASES);
    let floats = generate_values(elm_float_strategy(), FUZZ_CASES * 2);

    for i in 0..FUZZ_CASES {
        let mut enc = Wire3Encoder::new();
        enc.encode_string(&strings[i]);                       // label
        enc.encode_float(&ElmFloat::new(floats[i * 2]));      // point.0
        enc.encode_float(&ElmFloat::new(floats[i * 2 + 1]));  // point.1
        harness
            .round_trip("Coordinate", &enc.into_bytes())
            .assert_match(&format!("Coordinate(i={i})"));
    }
}

#[test]
fn fuzz_dashboard() {
    let mut harness = ElmHarness::new();
    // Dashboard: nested(Result String (Maybe Int)), optionalData(Maybe (List String)),
    //            userScores(Dict String (List Int)) — alphabetical

    let strings = generate_values(short_string_strategy(), FUZZ_CASES * 10);
    let ints = generate_values(elm_int_strategy(), FUZZ_CASES * 10);
    let bools = generate_values(any::<bool>(), FUZZ_CASES * 5);
    let small_lens = generate_values(0usize..4, FUZZ_CASES * 5);
    let mut si = 0;
    let mut ii = 0;
    let mut bi = 0;
    let mut li = 0;

    for idx in 0..FUZZ_CASES {
        let mut enc = Wire3Encoder::new();

        // nested: Result String (Maybe Int) — Ok=0, Err=1
        let is_ok = bools[bi % bools.len()];
        bi += 1;
        if is_ok {
            enc.encode_tag8(0); // Ok
            let is_just = bools[bi % bools.len()];
            bi += 1;
            if is_just {
                enc.encode_tag8(1); // Just
                enc.encode_int(&ElmInt::new(ints[ii % ints.len()]).unwrap());
                ii += 1;
            } else {
                enc.encode_tag8(0); // Nothing
            }
        } else {
            enc.encode_tag8(1); // Err
            enc.encode_string(&strings[si % strings.len()]);
            si += 1;
        }

        // optionalData: Maybe (List String)
        let has_data = bools[bi % bools.len()];
        bi += 1;
        if has_data {
            enc.encode_tag8(1); // Just
            let len = small_lens[li % small_lens.len()];
            li += 1;
            enc.encode_int_raw(len as i64);
            for _ in 0..len {
                enc.encode_string(&strings[si % strings.len()]);
                si += 1;
            }
        } else {
            enc.encode_tag8(0); // Nothing
        }

        // userScores: Dict String (List Int) — sorted by Elm's UTF-16 code unit ordering
        let dict_len = small_lens[li % small_lens.len()];
        li += 1;
        let mut dict: BTreeMap<String, Vec<i64>> = BTreeMap::new();
        for _ in 0..dict_len {
            let k = strings[si % strings.len()].clone();
            si += 1;
            let vlen = small_lens[li % small_lens.len()];
            li += 1;
            let mut vals = Vec::new();
            for _ in 0..vlen {
                vals.push(ints[ii % ints.len()]);
                ii += 1;
            }
            dict.insert(k, vals);
        }
        // Re-sort by Elm's string ordering (UTF-16 code units)
        let mut dict_sorted: Vec<(&String, &Vec<i64>)> = dict.iter().collect();
        dict_sorted.sort_by(|a, b| elm_str_cmp(a.0, b.0));
        enc.encode_int_raw(dict_sorted.len() as i64);
        for (k, vals) in &dict_sorted {
            enc.encode_string(k);
            enc.encode_int_raw(vals.len() as i64);
            for v in *vals {
                enc.encode_int(&ElmInt::new(*v).unwrap());
            }
        }

        harness
            .round_trip("Dashboard", &enc.into_bytes())
            .assert_match(&format!("Dashboard(i={idx})"));
    }
}
