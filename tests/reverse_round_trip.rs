// Reverse round-trip tests: Elm encodes → Rust decodes + re-encodes → verify bytes match.
//
// This tests the OPPOSITE direction from round_trip.rs:
//   round_trip.rs:         Rust encodes → Elm decodes+re-encodes → verify
//   reverse_round_trip.rs: Elm encodes  → Rust decodes+re-encodes → verify
//
// Together they prove full bidirectional Wire3 compatibility.

use std::collections::BTreeMap;
use std::io::{BufRead, BufReader, Write};
use std::process::{Command, Stdio};

use elm_wire3_rs::wire3::decode::Wire3Decoder;
use elm_wire3_rs::wire3::encode::Wire3Encoder;
use elm_wire3_rs::wire3::types::*;

// ── Elm harness ──────────────────────────────────────────────────

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

    /// Ask Elm to encode a predefined fixture value.
    fn elm_encode(&mut self, type_name: &str, case_id: &str) -> Vec<u8> {
        let request = format!("ENCODE {type_name} {case_id}\n");
        self.stdin.write_all(request.as_bytes()).expect("write");
        self.stdin.flush().expect("flush");

        let mut response = String::new();
        self.reader.read_line(&mut response).expect("read");
        let response = response.trim();

        if let Some(hex) = response.strip_prefix("OK ") {
            hex_to_bytes(hex)
        } else {
            panic!("Elm encode failed for {type_name}/{case_id}: {response}");
        }
    }
}

impl Drop for ElmHarness {
    fn drop(&mut self) {
        let _ = self.child.kill();
    }
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

fn bytes_to_hex(bytes: &[u8]) -> String {
    bytes.iter().map(|b| format!("{b:02x}")).collect()
}

/// Decode Elm-encoded bytes in Rust, then re-encode and verify byte equality.
fn assert_rust_roundtrip(
    elm_bytes: &[u8],
    decode_and_reencode: impl FnOnce(&[u8]) -> Vec<u8>,
    context: &str,
) {
    let reencoded = decode_and_reencode(elm_bytes);
    assert_eq!(
        elm_bytes,
        reencoded.as_slice(),
        "Reverse round-trip mismatch for {context}!\n  Elm encoded:  {}\n  Rust encoded: {}",
        bytes_to_hex(elm_bytes),
        bytes_to_hex(&reencoded),
    );
}

// ── Primitive reverse round-trips ────────────────────────────────

#[test]
fn reverse_int() {
    let mut h = ElmHarness::new();
    for case in ["0", "1", "neg1", "42", "boundary1", "boundary2", "boundary3", "boundary4", "large", "max", "min", "neg_large"] {
        let elm_bytes = h.elm_encode("Int", case);
        assert_rust_roundtrip(&elm_bytes, |bytes| {
            let mut dec = Wire3Decoder::new(bytes);
            let val = dec.decode_int().unwrap();
            assert!(dec.is_empty(), "leftover bytes for Int/{case}");
            let mut enc = Wire3Encoder::new();
            enc.encode_int(&val);
            enc.into_bytes()
        }, &format!("Int/{case}"));
    }
}

#[test]
fn reverse_float() {
    let mut h = ElmHarness::new();
    for case in ["0", "1", "neg1", "pi", "tiny", "big"] {
        let elm_bytes = h.elm_encode("Float", case);
        assert_rust_roundtrip(&elm_bytes, |bytes| {
            let mut dec = Wire3Decoder::new(bytes);
            let val = dec.decode_float().unwrap();
            assert!(dec.is_empty());
            let mut enc = Wire3Encoder::new();
            enc.encode_float(&val);
            enc.into_bytes()
        }, &format!("Float/{case}"));
    }
}

#[test]
fn reverse_bool() {
    let mut h = ElmHarness::new();
    for case in ["true", "false"] {
        let elm_bytes = h.elm_encode("Bool", case);
        assert_rust_roundtrip(&elm_bytes, |bytes| {
            let mut dec = Wire3Decoder::new(bytes);
            let val = dec.decode_bool().unwrap();
            assert!(dec.is_empty());
            let mut enc = Wire3Encoder::new();
            enc.encode_bool(val);
            enc.into_bytes()
        }, &format!("Bool/{case}"));
    }
}

#[test]
fn reverse_string() {
    let mut h = ElmHarness::new();
    for case in ["empty", "hello", "unicode", "cjk", "emoji", "newlines"] {
        let elm_bytes = h.elm_encode("String", case);
        assert_rust_roundtrip(&elm_bytes, |bytes| {
            let mut dec = Wire3Decoder::new(bytes);
            let val = dec.decode_string().unwrap();
            assert!(dec.is_empty());
            let mut enc = Wire3Encoder::new();
            enc.encode_string(&val);
            enc.into_bytes()
        }, &format!("String/{case}"));
    }
}

// ── Complex type reverse round-trips ─────────────────────────────

#[test]
fn reverse_color() {
    let mut h = ElmHarness::new();
    // Color: Blue=0, Green=1, Red=2 (alphabetical)
    for case in ["red", "green", "blue"] {
        let elm_bytes = h.elm_encode("Color", case);
        assert_rust_roundtrip(&elm_bytes, |bytes| {
            let mut dec = Wire3Decoder::new(bytes);
            let tag = dec.decode_tag8().unwrap();
            assert!(dec.is_empty());
            let mut enc = Wire3Encoder::new();
            enc.encode_tag8(tag);
            enc.into_bytes()
        }, &format!("Color/{case}"));
    }
}

#[test]
fn reverse_shape() {
    let mut h = ElmHarness::new();
    // Shape: Circle=0, Point=1, Rectangle=2
    for case in ["circle", "point", "rect"] {
        let elm_bytes = h.elm_encode("Shape", case);
        assert_rust_roundtrip(&elm_bytes, |bytes| {
            let mut dec = Wire3Decoder::new(bytes);
            let tag = dec.decode_tag8().unwrap();
            let mut enc = Wire3Encoder::new();
            enc.encode_tag8(tag);
            match tag {
                0 => {
                    // Circle Float
                    let f = dec.decode_float().unwrap();
                    enc.encode_float(&f);
                }
                1 => {
                    // Point — no args
                }
                2 => {
                    // Rectangle Float Float
                    let w = dec.decode_float().unwrap();
                    let h = dec.decode_float().unwrap();
                    enc.encode_float(&w);
                    enc.encode_float(&h);
                }
                _ => panic!("unexpected tag {tag}"),
            }
            assert!(dec.is_empty());
            enc.into_bytes()
        }, &format!("Shape/{case}"));
    }
}

#[test]
fn reverse_person() {
    let mut h = ElmHarness::new();
    // Person fields alphabetical: active(Bool), age(Int), name(String), score(Float)
    for case in ["alice", "empty", "unicode"] {
        let elm_bytes = h.elm_encode("Person", case);
        assert_rust_roundtrip(&elm_bytes, |bytes| {
            let mut dec = Wire3Decoder::new(bytes);
            let active = dec.decode_bool().unwrap();
            let age = dec.decode_int().unwrap();
            let name = dec.decode_string().unwrap();
            let score = dec.decode_float().unwrap();
            assert!(dec.is_empty(), "leftover bytes for Person/{case}");

            let mut enc = Wire3Encoder::new();
            enc.encode_bool(active);
            enc.encode_int(&age);
            enc.encode_string(&name);
            enc.encode_float(&score);
            enc.into_bytes()
        }, &format!("Person/{case}"));
    }
}

#[test]
fn reverse_inventory() {
    let mut h = ElmHarness::new();
    // Inventory fields alphabetical: counts(Dict), items(List), selected(Maybe), tags(Set)
    for case in ["empty", "full"] {
        let elm_bytes = h.elm_encode("Inventory", case);
        assert_rust_roundtrip(&elm_bytes, |bytes| {
            let mut dec = Wire3Decoder::new(bytes);

            // counts: Dict String Int
            let counts: BTreeMap<String, ElmInt> = dec.decode_dict(
                |d| d.decode_string(),
                |d| d.decode_int(),
            ).unwrap();

            // items: List String
            let items: Vec<String> = dec.decode_list(|d| d.decode_string()).unwrap();

            // selected: Maybe String
            let selected: Option<String> = dec.decode_maybe(|d| d.decode_string()).unwrap();

            // tags: Set String
            let tags: std::collections::BTreeSet<String> = dec.decode_set(|d| d.decode_string()).unwrap();

            assert!(dec.is_empty(), "leftover bytes for Inventory/{case}");

            let mut enc = Wire3Encoder::new();
            // Re-encode with Elm string ordering for Dict and Set
            enc.encode_string_key_dict(&counts, |e, v| e.encode_int(v));
            enc.encode_list(&items, |e, v| e.encode_string(v));
            enc.encode_maybe(&selected, |e, v| e.encode_string(v));
            enc.encode_string_set(&tags);
            enc.into_bytes()
        }, &format!("Inventory/{case}"));
    }
}

#[test]
fn reverse_api_response() {
    let mut h = ElmHarness::new();
    // ApiResponse fields alphabetical: message(String), result(Result String Int)
    for case in ["ok", "err"] {
        let elm_bytes = h.elm_encode("ApiResponse", case);
        assert_rust_roundtrip(&elm_bytes, |bytes| {
            let mut dec = Wire3Decoder::new(bytes);

            let message = dec.decode_string().unwrap();
            let result: Result<ElmInt, String> = dec.decode_result(
                |d| d.decode_string(),
                |d| d.decode_int(),
            ).unwrap();
            assert!(dec.is_empty());

            let mut enc = Wire3Encoder::new();
            enc.encode_string(&message);
            enc.encode_result(&result, |e, s| e.encode_string(s), |e, v| e.encode_int(v));
            enc.into_bytes()
        }, &format!("ApiResponse/{case}"));
    }
}

#[test]
fn reverse_tree() {
    let mut h = ElmHarness::new();

    for case in ["leaf", "branch", "nested"] {
        let elm_bytes = h.elm_encode("Tree", case);
        assert_rust_roundtrip(&elm_bytes, |bytes| {
            let mut dec = Wire3Decoder::new(bytes);
            let mut enc = Wire3Encoder::new();
            decode_and_reencode_tree(&mut dec, &mut enc);
            assert!(dec.is_empty(), "leftover bytes for Tree/{case}");
            enc.into_bytes()
        }, &format!("Tree/{case}"));
    }
}

fn decode_and_reencode_tree(dec: &mut Wire3Decoder, enc: &mut Wire3Encoder) {
    let tag = dec.decode_tag8().unwrap();
    enc.encode_tag8(tag);
    match tag {
        0 => {
            // Branch left right
            decode_and_reencode_tree(dec, enc);
            decode_and_reencode_tree(dec, enc);
        }
        1 => {
            // Leaf Int
            let val = dec.decode_int().unwrap();
            enc.encode_int(&val);
        }
        _ => panic!("unexpected tree tag {tag}"),
    }
}

#[test]
fn reverse_coordinate() {
    let mut h = ElmHarness::new();
    // Coordinate fields alphabetical: label(String), point(Float, Float)
    for case in ["origin", "point"] {
        let elm_bytes = h.elm_encode("Coordinate", case);
        assert_rust_roundtrip(&elm_bytes, |bytes| {
            let mut dec = Wire3Decoder::new(bytes);
            let label = dec.decode_string().unwrap();
            let x = dec.decode_float().unwrap();
            let y = dec.decode_float().unwrap();
            assert!(dec.is_empty());

            let mut enc = Wire3Encoder::new();
            enc.encode_string(&label);
            enc.encode_float(&x);
            enc.encode_float(&y);
            enc.into_bytes()
        }, &format!("Coordinate/{case}"));
    }
}

#[test]
fn reverse_dashboard() {
    let mut h = ElmHarness::new();
    // Dashboard fields alphabetical: nested(Result String (Maybe Int)), optionalData(Maybe (List String)), userScores(Dict String (List Int))
    for case in ["empty", "full", "error"] {
        let elm_bytes = h.elm_encode("Dashboard", case);
        assert_rust_roundtrip(&elm_bytes, |bytes| {
            let mut dec = Wire3Decoder::new(bytes);

            // nested: Result String (Maybe Int)
            let nested: Result<Option<ElmInt>, String> = dec.decode_result(
                |d| d.decode_string(),
                |d| d.decode_maybe(|d2| d2.decode_int()),
            ).unwrap();

            // optionalData: Maybe (List String)
            let optional_data: Option<Vec<String>> = dec.decode_maybe(
                |d| d.decode_list(|d2| d2.decode_string()),
            ).unwrap();

            // userScores: Dict String (List Int)
            let user_scores: BTreeMap<String, Vec<ElmInt>> = dec.decode_dict(
                |d| d.decode_string(),
                |d| d.decode_list(|d2| d2.decode_int()),
            ).unwrap();

            assert!(dec.is_empty(), "leftover bytes for Dashboard/{case}");

            let mut enc = Wire3Encoder::new();
            enc.encode_result(&nested, |e, s| e.encode_string(s), |e, v| {
                e.encode_maybe(v, |e2, i| e2.encode_int(i));
            });
            enc.encode_maybe(&optional_data, |e, list| {
                e.encode_list(list, |e2, s| e2.encode_string(s));
            });
            enc.encode_string_key_dict(&user_scores, |e, list| {
                e.encode_list(list, |e2, v| e2.encode_int(v));
            });
            enc.into_bytes()
        }, &format!("Dashboard/{case}"));
    }
}

#[test]
fn reverse_team() {
    let mut h = ElmHarness::new();
    // Team fields alphabetical: color(Color), leader(Person), members(List Person)
    for case in ["small", "empty"] {
        let elm_bytes = h.elm_encode("Team", case);
        assert_rust_roundtrip(&elm_bytes, |bytes| {
            let mut dec = Wire3Decoder::new(bytes);

            // color: Color (tag)
            let color_tag = dec.decode_tag8().unwrap();

            // leader: Person (active, age, name, score)
            let l_active = dec.decode_bool().unwrap();
            let l_age = dec.decode_int().unwrap();
            let l_name = dec.decode_string().unwrap();
            let l_score = dec.decode_float().unwrap();

            // members: List Person
            let member_count = dec.decode_int_raw().unwrap() as usize;
            let mut members = Vec::new();
            for _ in 0..member_count {
                let active = dec.decode_bool().unwrap();
                let age = dec.decode_int().unwrap();
                let name = dec.decode_string().unwrap();
                let score = dec.decode_float().unwrap();
                members.push((active, age, name, score));
            }

            assert!(dec.is_empty(), "leftover bytes for Team/{case}");

            let mut enc = Wire3Encoder::new();
            enc.encode_tag8(color_tag);
            enc.encode_bool(l_active);
            enc.encode_int(&l_age);
            enc.encode_string(&l_name);
            enc.encode_float(&l_score);
            enc.encode_int_raw(members.len() as i64);
            for (active, age, name, score) in &members {
                enc.encode_bool(*active);
                enc.encode_int(age);
                enc.encode_string(name);
                enc.encode_float(score);
            }
            enc.into_bytes()
        }, &format!("Team/{case}"));
    }
}
