// Round-trip integration tests: Rust encodes → Elm decodes+re-encodes → Rust verifies.
//
// These tests spawn the Elm harness (Node.js process) and communicate via
// stdin/stdout using the hex-encoded Wire3 protocol.

use std::io::{BufRead, BufReader, Write};
use std::process::{Command, Stdio};

use elm_wire3_rs::wire3::encode::Wire3Encoder;
use elm_wire3_rs::wire3::types::*;

/// Spawn the Elm harness process and return a handle for communication.
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
            .expect("Failed to spawn Elm harness (is Node.js installed?)");

        let stdin = child.stdin.take().unwrap();
        let stdout = child.stdout.take().unwrap();
        let reader = BufReader::new(stdout);

        ElmHarness {
            child,
            stdin,
            reader,
        }
    }

    /// Send a Wire3-encoded value to the Elm harness and get back the re-encoded bytes.
    fn round_trip(&mut self, type_name: &str, encoded_bytes: &[u8]) -> RoundTripResult {
        let hex = bytes_to_hex(encoded_bytes);
        let request = format!("{type_name} {hex}\n");

        self.stdin
            .write_all(request.as_bytes())
            .expect("Failed to write to harness");
        self.stdin.flush().expect("Failed to flush");

        let mut response = String::new();
        self.reader
            .read_line(&mut response)
            .expect("Failed to read from harness");
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
            RoundTripResult::ElmError(format!("unexpected response: {response}"))
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

// ── Helper to encode and round-trip ─────────────────────────────

fn encode_and_test(harness: &mut ElmHarness, type_name: &str, encode_fn: impl FnOnce(&mut Wire3Encoder)) {
    let mut enc = Wire3Encoder::new();
    encode_fn(&mut enc);
    let bytes = enc.into_bytes();
    harness
        .round_trip(type_name, &bytes)
        .assert_match(type_name);
}

// ── Primitive round-trip tests ──────────────────────────────────

#[test]
fn round_trip_int_values() {
    let mut harness = ElmHarness::new();

    let test_values: Vec<i64> = vec![
        0, 1, -1, 2, -2, 42, -42, 100, -100,
        107, 108, -108, -109,          // 1-byte / 2-byte boundary
        4715, 4716, -4716, -4717,      // 2-byte / 3-byte boundary
        32767, 32768, -32768, -32769,  // near i16 boundary
        65535, 65536,                   // u16 max
        8_388_607, 8_388_608,          // 3-byte / 4-byte boundary
        2_147_483_647, 2_147_483_648,  // near i32 / 5-byte boundary
        ElmInt::MAX,                    // 2^53 - 1
        ElmInt::MIN,                    // -(2^53 - 1)
    ];

    for v in test_values {
        let elm_v = ElmInt::new(v).unwrap();
        encode_and_test(&mut harness, "Int", |enc| enc.encode_int(&elm_v));
    }
}

#[test]
fn round_trip_float_values() {
    let mut harness = ElmHarness::new();

    let test_values: Vec<f64> = vec![
        0.0, 1.0, -1.0, 0.5, -0.5, 3.14159265358979,
        f64::MAX, f64::MIN, f64::EPSILON,
        1e10, -1e10, 1e-10,
    ];

    for v in test_values {
        let elm_v = ElmFloat::new(v);
        encode_and_test(&mut harness, "Float", |enc| enc.encode_float(&elm_v));
    }
}

#[test]
fn round_trip_bool_values() {
    let mut harness = ElmHarness::new();
    encode_and_test(&mut harness, "Bool", |enc| enc.encode_bool(false));
    encode_and_test(&mut harness, "Bool", |enc| enc.encode_bool(true));
}

#[test]
fn round_trip_string_values() {
    let mut harness = ElmHarness::new();

    let test_strings = vec![
        "",
        "hello",
        "Hello, World!",
        "héllo wörld",
        "日本語テスト",
        "🎉🚀🌍",
        "line1\nline2\ttab",
    ];

    for s in test_strings {
        encode_and_test(&mut harness, "String", |enc| enc.encode_string(s));
    }
}

// ── Complex type round-trip tests ───────────────────────────────

#[test]
fn round_trip_color() {
    let mut harness = ElmHarness::new();

    // Color constructors sorted alphabetically: Blue=0, Green=1, Red=2
    encode_and_test(&mut harness, "Color", |enc| enc.encode_tag8(0)); // Blue
    encode_and_test(&mut harness, "Color", |enc| enc.encode_tag8(1)); // Green
    encode_and_test(&mut harness, "Color", |enc| enc.encode_tag8(2)); // Red
}

#[test]
fn round_trip_shape() {
    let mut harness = ElmHarness::new();

    // Shape constructors sorted alphabetically: Circle=0, Point=1, Rectangle=2
    // Circle Float
    encode_and_test(&mut harness, "Shape", |enc| {
        enc.encode_tag8(0);
        enc.encode_float(&ElmFloat::new(5.0));
    });

    // Point (no args)
    encode_and_test(&mut harness, "Shape", |enc| {
        enc.encode_tag8(1);
    });

    // Rectangle Float Float
    encode_and_test(&mut harness, "Shape", |enc| {
        enc.encode_tag8(2);
        enc.encode_float(&ElmFloat::new(3.0));
        enc.encode_float(&ElmFloat::new(4.0));
    });
}

#[test]
fn round_trip_person() {
    let mut harness = ElmHarness::new();

    // Person { name : String, age : Int, score : Float, active : Bool }
    // Fields in ALPHABETICAL order: active, age, name, score
    encode_and_test(&mut harness, "Person", |enc| {
        enc.encode_bool(true);                        // active
        enc.encode_int(&ElmInt::new(30).unwrap());    // age
        enc.encode_string("Alice");                   // name
        enc.encode_float(&ElmFloat::new(95.5));       // score
    });

    // Another person
    encode_and_test(&mut harness, "Person", |enc| {
        enc.encode_bool(false);                       // active
        enc.encode_int(&ElmInt::new(0).unwrap());     // age
        enc.encode_string("");                        // name
        enc.encode_float(&ElmFloat::new(0.0));        // score
    });
}

#[test]
fn round_trip_inventory() {
    let mut harness = ElmHarness::new();

    // Inventory { items : List String, counts : Dict String Int, tags : Set String, selected : Maybe String }
    // Fields alphabetically: counts, items, selected, tags
    encode_and_test(&mut harness, "Inventory", |enc| {
        // counts: Dict String Int — empty
        enc.encode_int_raw(0); // count = 0

        // items: List String — ["apple", "banana"]
        enc.encode_int_raw(2); // count = 2
        enc.encode_string("apple");
        enc.encode_string("banana");

        // selected: Maybe String — Nothing
        enc.encode_tag8(0);

        // tags: Set String — empty set
        enc.encode_int_raw(0);
    });
}

#[test]
fn round_trip_api_response() {
    let mut harness = ElmHarness::new();

    // ApiResponse { result : Result String Int, message : String }
    // Fields alphabetically: message, result
    // Result tags: Ok=0, Err=1 (Wire3.elm hardcoded)

    // Ok case
    encode_and_test(&mut harness, "ApiResponse", |enc| {
        enc.encode_string("success");                 // message
        enc.encode_tag8(0);                           // Ok
        enc.encode_int(&ElmInt::new(42).unwrap());    // value
    });

    // Err case
    encode_and_test(&mut harness, "ApiResponse", |enc| {
        enc.encode_string("failed");                  // message
        enc.encode_tag8(1);                           // Err
        enc.encode_string("not found");               // error
    });
}

#[test]
fn round_trip_tree() {
    let mut harness = ElmHarness::new();

    // Tree: Branch=0, Leaf=1 (alphabetically)

    // Simple leaf
    encode_and_test(&mut harness, "Tree", |enc| {
        enc.encode_tag8(1); // Leaf
        enc.encode_int(&ElmInt::new(42).unwrap());
    });

    // Branch with two leaves
    encode_and_test(&mut harness, "Tree", |enc| {
        enc.encode_tag8(0); // Branch
        enc.encode_tag8(1); // Leaf (left)
        enc.encode_int(&ElmInt::new(1).unwrap());
        enc.encode_tag8(1); // Leaf (right)
        enc.encode_int(&ElmInt::new(2).unwrap());
    });

    // Nested: Branch(Branch(Leaf 1, Leaf 2), Leaf 3)
    encode_and_test(&mut harness, "Tree", |enc| {
        enc.encode_tag8(0); // Branch
        enc.encode_tag8(0); //   Branch (left)
        enc.encode_tag8(1); //     Leaf
        enc.encode_int(&ElmInt::new(1).unwrap());
        enc.encode_tag8(1); //     Leaf
        enc.encode_int(&ElmInt::new(2).unwrap());
        enc.encode_tag8(1); //   Leaf (right)
        enc.encode_int(&ElmInt::new(3).unwrap());
    });
}

#[test]
fn round_trip_coordinate() {
    let mut harness = ElmHarness::new();

    // Coordinate { point : ( Float, Float ), label : String }
    // Fields alphabetically: label, point
    encode_and_test(&mut harness, "Coordinate", |enc| {
        enc.encode_string("origin");              // label
        enc.encode_float(&ElmFloat::new(0.0));    // point.0
        enc.encode_float(&ElmFloat::new(0.0));    // point.1
    });

    encode_and_test(&mut harness, "Coordinate", |enc| {
        enc.encode_string("top-right");           // label
        enc.encode_float(&ElmFloat::new(100.5));  // point.0
        enc.encode_float(&ElmFloat::new(200.3));  // point.1
    });
}

#[test]
fn round_trip_dashboard() {
    let mut harness = ElmHarness::new();

    // Dashboard { userScores : Dict String (List Int), optionalData : Maybe (List String), nested : Result String (Maybe Int) }
    // Fields alphabetically: nested, optionalData, userScores

    encode_and_test(&mut harness, "Dashboard", |enc| {
        // nested: Result String (Maybe Int) — Ok (Just 42)
        enc.encode_tag8(0); // Ok
        enc.encode_tag8(1); // Just
        enc.encode_int(&ElmInt::new(42).unwrap());

        // optionalData: Maybe (List String) — Just ["hello"]
        enc.encode_tag8(1); // Just
        enc.encode_int_raw(1); // list count
        enc.encode_string("hello");

        // userScores: Dict String (List Int) — { "alice": [10, 20] }
        enc.encode_int_raw(1); // dict count
        enc.encode_string("alice"); // key
        enc.encode_int_raw(2); // list count
        enc.encode_int(&ElmInt::new(10).unwrap());
        enc.encode_int(&ElmInt::new(20).unwrap());
    });
}
