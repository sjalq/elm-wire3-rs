// Wire3 Encoder — exact match to Lamdera/Wire3.elm encoding format.
//
// Integer encoding uses zigzag + variable-length:
//   signed → unsigned: positive i → 2*i, negative i → -2*i - 1
//   unsigned n is then encoded in 1–9 bytes (see encode_varint_unsigned)
//
// All multi-byte values use little-endian byte order.

use std::collections::{BTreeMap, BTreeSet};

use super::types::*;

/// Wire3 binary encoder. Accumulates bytes into an internal buffer.
pub struct Wire3Encoder {
    buf: Vec<u8>,
}

impl Wire3Encoder {
    pub fn new() -> Self {
        Wire3Encoder { buf: Vec::new() }
    }

    pub fn with_capacity(cap: usize) -> Self {
        Wire3Encoder {
            buf: Vec::with_capacity(cap),
        }
    }

    /// Consume the encoder and return the encoded bytes.
    pub fn into_bytes(self) -> Vec<u8> {
        self.buf
    }

    /// Current encoded byte count.
    pub fn len(&self) -> usize {
        self.buf.len()
    }

    pub fn is_empty(&self) -> bool {
        self.buf.is_empty()
    }

    // ── Primitive encoders ──────────────────────────────────────

    /// Encode an Elm Int using zigzag varint.
    pub fn encode_int(&mut self, v: &ElmInt) {
        self.encode_int_raw(v.value());
    }

    /// Encode a raw i64 as zigzag varint (for internal use / generated code).
    pub fn encode_int_raw(&mut self, v: i64) {
        let unsigned = zigzag_encode(v);
        self.encode_varint_unsigned(unsigned, v);
    }

    /// Encode an Elm Float as 8 bytes IEEE 754 LE.
    pub fn encode_float(&mut self, v: &ElmFloat) {
        self.buf.extend_from_slice(&v.value().to_le_bytes());
    }

    /// Encode a bool as a single byte.
    pub fn encode_bool(&mut self, v: bool) {
        self.buf.push(if v { 1 } else { 0 });
    }

    /// Encode an Elm Char as its code point via zigzag varint.
    pub fn encode_char(&mut self, v: &ElmChar) {
        self.encode_int_raw(v.code_point() as i64);
    }

    /// Encode a string: [varint utf8_byte_len][utf8 bytes].
    pub fn encode_string(&mut self, v: &str) {
        let bytes = v.as_bytes();
        self.encode_int_raw(bytes.len() as i64);
        self.buf.extend_from_slice(bytes);
    }

    /// Encode raw bytes: [varint len][bytes].
    pub fn encode_bytes(&mut self, v: &[u8]) {
        self.encode_int_raw(v.len() as i64);
        self.buf.extend_from_slice(v);
    }

    /// Encode a u8 tag (for custom type constructors with ≤255 variants).
    pub fn encode_tag8(&mut self, tag: u8) {
        self.buf.push(tag);
    }

    /// Encode a u16 tag LE (for custom types with 256–65535 variants).
    pub fn encode_tag16(&mut self, tag: u16) {
        self.buf.extend_from_slice(&tag.to_le_bytes());
    }

    /// Encode unit: zero bytes.
    pub fn encode_unit(&mut self) {
        // Nothing to write
    }

    /// Encode Elm Order: LT=0, EQ=1, GT=2.
    pub fn encode_order(&mut self, v: ElmOrder) {
        self.buf.push(match v {
            ElmOrder::LT => 0,
            ElmOrder::EQ => 1,
            ElmOrder::GT => 2,
        });
    }

    // ── Container encoders ──────────────────────────────────────

    /// Encode a list: [varint count][elements...].
    pub fn encode_list<T>(&mut self, items: &[T], encode_elem: impl Fn(&mut Self, &T)) {
        self.encode_int_raw(items.len() as i64);
        for item in items {
            encode_elem(self, item);
        }
    }

    /// Encode a set: [varint count][elements...] (BTreeSet is already sorted).
    pub fn encode_set<T: Ord>(
        &mut self,
        items: &ElmSet<T>,
        encode_elem: impl Fn(&mut Self, &T),
    ) {
        self.encode_int_raw(items.len() as i64);
        for item in items {
            encode_elem(self, item);
        }
    }

    /// Encode a Set String, sorted by Elm's string ordering (UTF-16 code units).
    /// Elm (via JavaScript) compares strings by UTF-16 code units, which differs
    /// from Rust's UTF-8 byte ordering for characters >= U+10000.
    pub fn encode_string_set(&mut self, items: &ElmSet<String>) {
        let mut sorted: Vec<&String> = items.iter().collect();
        sorted.sort_by(|a, b| elm_str_cmp(a, b));
        self.encode_int_raw(sorted.len() as i64);
        for s in sorted {
            self.encode_string(s);
        }
    }

    /// Encode a dict: [varint count][k0,v0][k1,v1]...
    /// BTreeMap iterates in sorted key order, matching Elm's Dict.toList.
    pub fn encode_dict<K: Ord, V>(
        &mut self,
        items: &ElmDict<K, V>,
        encode_key: impl Fn(&mut Self, &K),
        encode_val: impl Fn(&mut Self, &V),
    ) {
        self.encode_int_raw(items.len() as i64);
        for (k, v) in items {
            encode_key(self, k);
            encode_val(self, v);
        }
    }

    /// Encode a Dict with String keys, sorted by Elm's string ordering (UTF-16 code units).
    pub fn encode_string_key_dict<V>(
        &mut self,
        items: &ElmDict<String, V>,
        encode_val: impl Fn(&mut Self, &V),
    ) {
        let mut sorted: Vec<(&String, &V)> = items.iter().collect();
        sorted.sort_by(|a, b| elm_str_cmp(a.0, b.0));
        self.encode_int_raw(sorted.len() as i64);
        for (k, v) in sorted {
            self.encode_string(k);
            encode_val(self, v);
        }
    }

    /// Encode Maybe: Nothing=[0x00], Just v=[0x01][v].
    pub fn encode_maybe<T>(
        &mut self,
        v: &ElmMaybe<T>,
        encode_inner: impl Fn(&mut Self, &T),
    ) {
        match v {
            None => self.buf.push(0),
            Some(inner) => {
                self.buf.push(1);
                encode_inner(self, inner);
            }
        }
    }

    /// Encode Result: Ok v=[0x00][v], Err e=[0x01][e].
    /// Wire3 sorts constructors alphabetically: Err=0, Ok=1.
    pub fn encode_result<E, V>(
        &mut self,
        v: &ElmResult<E, V>,
        encode_err: impl Fn(&mut Self, &E),
        encode_ok: impl Fn(&mut Self, &V),
    ) {
        match v {
            Ok(val) => {
                self.buf.push(0);
                encode_ok(self, val);
            }
            Err(err) => {
                self.buf.push(1);
                encode_err(self, err);
            }
        }
    }

    /// Encode a pair: [a][b] (no length prefix).
    pub fn encode_pair<A, B>(
        &mut self,
        v: &ElmPair<A, B>,
        encode_a: impl Fn(&mut Self, &A),
        encode_b: impl Fn(&mut Self, &B),
    ) {
        encode_a(self, &v.0);
        encode_b(self, &v.1);
    }

    /// Encode a triple: [a][b][c] (no length prefix).
    pub fn encode_triple<A, B, C>(
        &mut self,
        v: &ElmTriple<A, B, C>,
        encode_a: impl Fn(&mut Self, &A),
        encode_b: impl Fn(&mut Self, &B),
        encode_c: impl Fn(&mut Self, &C),
    ) {
        encode_a(self, &v.0);
        encode_b(self, &v.1);
        encode_c(self, &v.2);
    }

    // ── Zigzag varint internals ─────────────────────────────────

    /// Encode an unsigned value (post-zigzag) into 1–9 bytes.
    ///
    /// Format (matching Lamdera/Wire3.elm exactly):
    ///   n <= 215          → [n]                             (1 byte)
    ///   216..9431         → [216 + (n-216)/256, (n-216)%256] (2 bytes)
    ///   9432..65535       → [252, hi, lo]                   (3 bytes)
    ///   65536..16777215   → [253, b2, b1, b0]              (4 bytes)
    ///   16777216..2^32-1  → [254, b3, b2, b1, b0]          (5 bytes)
    ///   >= 2^32           → [255, float64_LE(original_i64)] (9 bytes)
    fn encode_varint_unsigned(&mut self, n: u64, original_signed: i64) {
        if n <= 215 {
            self.buf.push(n as u8);
        } else if n <= 9431 {
            let adjusted = n - 216;
            self.buf.push((216 + adjusted / 256) as u8);
            self.buf.push((adjusted % 256) as u8);
        } else if n <= 65535 {
            self.buf.push(252);
            self.buf.push((n >> 8) as u8);
            self.buf.push((n & 0xFF) as u8);
        } else if n <= 16_777_215 {
            self.buf.push(253);
            self.buf.push((n >> 16) as u8);
            self.buf.push(((n >> 8) & 0xFF) as u8);
            self.buf.push((n & 0xFF) as u8);
        } else if n <= 4_294_967_295 {
            self.buf.push(254);
            self.buf.push((n >> 24) as u8);
            self.buf.push(((n >> 16) & 0xFF) as u8);
            self.buf.push(((n >> 8) & 0xFF) as u8);
            self.buf.push((n & 0xFF) as u8);
        } else {
            // >= 2^32: escape to float64 LE of the ORIGINAL signed value
            self.buf.push(255);
            let float_val = original_signed as f64;
            self.buf.extend_from_slice(&float_val.to_le_bytes());
        }
    }
}

impl Default for Wire3Encoder {
    fn default() -> Self {
        Self::new()
    }
}

/// Compare two strings using Elm's ordering (UTF-16 code unit lexicographic order).
///
/// Elm runs on JavaScript, where string comparison uses UTF-16 code units.
/// Rust's native string ordering uses UTF-8 bytes, which is equivalent to
/// Unicode code point order. These two orderings diverge for characters
/// >= U+10000 (emoji, CJK extensions, etc.) because UTF-16 encodes them as
/// surrogate pairs (0xD800-0xDBFF lead + 0xDC00-0xDFFF trail) which sort
/// lower than BMP characters in the 0xE000-0xFFFF range.
pub fn elm_str_cmp(a: &str, b: &str) -> std::cmp::Ordering {
    a.encode_utf16().cmp(b.encode_utf16())
}

/// Zigzag encode: map signed → unsigned.
/// positive i → 2*i (even), negative i → -2*i - 1 (odd).
pub(crate) fn zigzag_encode(v: i64) -> u64 {
    if v >= 0 {
        (v as u64) * 2
    } else {
        ((-v) as u64) * 2 - 1
    }
}

// ── Wire3Encode impls for Elm wrapper types ─────────────────────

impl Wire3Encode for ElmInt {
    fn wire3_encode(&self, encoder: &mut Wire3Encoder) {
        encoder.encode_int(self);
    }
}

impl Wire3Encode for ElmFloat {
    fn wire3_encode(&self, encoder: &mut Wire3Encoder) {
        encoder.encode_float(self);
    }
}

impl Wire3Encode for bool {
    fn wire3_encode(&self, encoder: &mut Wire3Encoder) {
        encoder.encode_bool(*self);
    }
}

impl Wire3Encode for ElmChar {
    fn wire3_encode(&self, encoder: &mut Wire3Encoder) {
        encoder.encode_char(self);
    }
}

impl Wire3Encode for String {
    fn wire3_encode(&self, encoder: &mut Wire3Encoder) {
        encoder.encode_string(self);
    }
}

impl Wire3Encode for ElmOrder {
    fn wire3_encode(&self, encoder: &mut Wire3Encoder) {
        encoder.encode_order(*self);
    }
}

impl Wire3Encode for () {
    fn wire3_encode(&self, encoder: &mut Wire3Encoder) {
        encoder.encode_unit();
    }
}

impl<T: Wire3Encode> Wire3Encode for Vec<T> {
    fn wire3_encode(&self, encoder: &mut Wire3Encoder) {
        encoder.encode_list(self, |enc, item| item.wire3_encode(enc));
    }
}

impl<T: Wire3Encode + Ord> Wire3Encode for BTreeSet<T> {
    fn wire3_encode(&self, encoder: &mut Wire3Encoder) {
        encoder.encode_set(self, |enc, item: &T| item.wire3_encode(enc));
    }
}

impl<K: Wire3Encode + Ord, V: Wire3Encode> Wire3Encode for BTreeMap<K, V> {
    fn wire3_encode(&self, encoder: &mut Wire3Encoder) {
        encoder.encode_dict(self, |enc, k: &K| k.wire3_encode(enc), |enc, v: &V| v.wire3_encode(enc));
    }
}

impl<T: Wire3Encode> Wire3Encode for Option<T> {
    fn wire3_encode(&self, encoder: &mut Wire3Encoder) {
        encoder.encode_maybe(self, |enc, v| v.wire3_encode(enc));
    }
}

impl<T: Wire3Encode, E: Wire3Encode> Wire3Encode for Result<T, E> {
    fn wire3_encode(&self, encoder: &mut Wire3Encoder) {
        encoder.encode_result(self, |enc, e| e.wire3_encode(enc), |enc, v| v.wire3_encode(enc));
    }
}

impl<A: Wire3Encode, B: Wire3Encode> Wire3Encode for (A, B) {
    fn wire3_encode(&self, encoder: &mut Wire3Encoder) {
        self.0.wire3_encode(encoder);
        self.1.wire3_encode(encoder);
    }
}

impl<A: Wire3Encode, B: Wire3Encode, C: Wire3Encode> Wire3Encode for (A, B, C) {
    fn wire3_encode(&self, encoder: &mut Wire3Encoder) {
        self.0.wire3_encode(encoder);
        self.1.wire3_encode(encoder);
        self.2.wire3_encode(encoder);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn zigzag_values() {
        // From Elm's Wire3.elm comments:
        // 0 → 0, -1 → 1, 1 → 2, -2 → 3, 2 → 4, ...
        assert_eq!(zigzag_encode(0), 0);
        assert_eq!(zigzag_encode(-1), 1);
        assert_eq!(zigzag_encode(1), 2);
        assert_eq!(zigzag_encode(-2), 3);
        assert_eq!(zigzag_encode(2), 4);
        assert_eq!(zigzag_encode(100), 200);
        assert_eq!(zigzag_encode(-100), 199);
    }

    #[test]
    fn encode_small_ints() {
        // 0 → zigzag 0 → [0x00]
        let mut enc = Wire3Encoder::new();
        enc.encode_int_raw(0);
        assert_eq!(enc.into_bytes(), vec![0]);

        // 1 → zigzag 2 → [0x02]
        let mut enc = Wire3Encoder::new();
        enc.encode_int_raw(1);
        assert_eq!(enc.into_bytes(), vec![2]);

        // 107 → zigzag 214 → [214] (still 1 byte, 214 <= 215)
        let mut enc = Wire3Encoder::new();
        enc.encode_int_raw(107);
        assert_eq!(enc.into_bytes(), vec![214]);

        // -1 → zigzag 1 → [0x01]
        let mut enc = Wire3Encoder::new();
        enc.encode_int_raw(-1);
        assert_eq!(enc.into_bytes(), vec![1]);
    }

    #[test]
    fn encode_two_byte_ints() {
        // 108 → zigzag 216 → 2-byte: [216 + 0, 0] = [216, 0]
        let mut enc = Wire3Encoder::new();
        enc.encode_int_raw(108);
        assert_eq!(enc.into_bytes(), vec![216, 0]);
    }

    #[test]
    fn encode_bool() {
        let mut enc = Wire3Encoder::new();
        enc.encode_bool(false);
        enc.encode_bool(true);
        assert_eq!(enc.into_bytes(), vec![0, 1]);
    }

    #[test]
    fn encode_float() {
        let mut enc = Wire3Encoder::new();
        enc.encode_float(&ElmFloat::new(1.0));
        let bytes = enc.into_bytes();
        assert_eq!(bytes.len(), 8);
        assert_eq!(f64::from_le_bytes(bytes.try_into().unwrap()), 1.0);
    }

    #[test]
    fn encode_string() {
        let mut enc = Wire3Encoder::new();
        enc.encode_string("hi");
        let bytes = enc.into_bytes();
        // "hi" is 2 bytes → varint(2) = zigzag(2) = 4 → [4]
        // then b"hi" → [104, 105]
        assert_eq!(bytes, vec![4, b'h', b'i']);
    }

    #[test]
    fn encode_maybe() {
        let mut enc = Wire3Encoder::new();
        enc.encode_maybe(&None::<ElmInt>, |e, v| e.encode_int(v));
        assert_eq!(enc.into_bytes(), vec![0]);

        let mut enc = Wire3Encoder::new();
        let val: Option<ElmInt> = Some(ElmInt::new(0).unwrap());
        enc.encode_maybe(&val, |e, v| e.encode_int(v));
        assert_eq!(enc.into_bytes(), vec![1, 0]); // Just 0 → [1] [zigzag(0)=0]
    }

    #[test]
    fn encode_result() {
        // Wire3.elm hardcodes Ok=0, Err=1 (special-cased, not alphabetical)
        let mut enc = Wire3Encoder::new();
        let val: Result<ElmInt, ElmInt> = Ok(ElmInt::new(0).unwrap());
        enc.encode_result(&val, |e, v| e.encode_int(v), |e, v| e.encode_int(v));
        assert_eq!(enc.into_bytes(), vec![0, 0]); // Ok=0, value=zigzag(0)
    }
}
