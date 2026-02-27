// Wire3 Decoder — exact match to Lamdera/Wire3.elm decoding format.
//
// Reads from a byte slice, advancing a position cursor.
// All integer values use zigzag varint (1–9 bytes).
// All multi-byte values are little-endian.

use std::collections::{BTreeMap, BTreeSet};

use super::types::*;

/// Wire3 binary decoder. Reads from a byte slice.
pub struct Wire3Decoder<'a> {
    data: &'a [u8],
    pos: usize,
}

impl<'a> Wire3Decoder<'a> {
    pub fn new(data: &'a [u8]) -> Self {
        Wire3Decoder { data, pos: 0 }
    }

    /// Number of bytes consumed so far.
    pub fn position(&self) -> usize {
        self.pos
    }

    /// Number of bytes remaining.
    pub fn remaining(&self) -> usize {
        self.data.len() - self.pos
    }

    /// True if all bytes have been consumed.
    pub fn is_empty(&self) -> bool {
        self.pos >= self.data.len()
    }

    // ── Raw byte reading ────────────────────────────────────────

    fn read_u8(&mut self) -> Result<u8, Wire3DecodeError> {
        if self.pos >= self.data.len() {
            return Err(Wire3DecodeError::UnexpectedEof {
                needed: 1,
                available: 0,
            });
        }
        let b = self.data[self.pos];
        self.pos += 1;
        Ok(b)
    }

    fn read_bytes(&mut self, n: usize) -> Result<&'a [u8], Wire3DecodeError> {
        if self.pos + n > self.data.len() {
            return Err(Wire3DecodeError::UnexpectedEof {
                needed: n,
                available: self.data.len() - self.pos,
            });
        }
        let slice = &self.data[self.pos..self.pos + n];
        self.pos += n;
        Ok(slice)
    }

    fn read_f64_le(&mut self) -> Result<f64, Wire3DecodeError> {
        let bytes = self.read_bytes(8)?;
        Ok(f64::from_le_bytes(bytes.try_into().unwrap()))
    }

    // ── Primitive decoders ──────────────────────────────────────

    /// Decode an Elm Int from zigzag varint.
    pub fn decode_int(&mut self) -> Result<ElmInt, Wire3DecodeError> {
        let raw = self.decode_int_raw()?;
        ElmInt::new(raw).map_err(Wire3DecodeError::from)
    }

    /// Decode a raw i64 from zigzag varint (for internal use / generated code).
    pub fn decode_int_raw(&mut self) -> Result<i64, Wire3DecodeError> {
        let n0 = self.read_u8()?;

        if n0 <= 215 {
            Ok(zigzag_decode(n0 as u64))
        } else if n0 <= 251 {
            // 2-byte encoding
            let b1 = self.read_u8()?;
            let unsigned = ((n0 as u64) - 216) * 256 + (b1 as u64) + 216;
            Ok(zigzag_decode(unsigned))
        } else if n0 == 252 {
            // 3-byte: [252, hi, lo]
            let hi = self.read_u8()? as u64;
            let lo = self.read_u8()? as u64;
            let unsigned = hi * 256 + lo;
            Ok(zigzag_decode(unsigned))
        } else if n0 == 253 {
            // 4-byte: [253, b2, b1, b0]
            let b2 = self.read_u8()? as u64;
            let b1 = self.read_u8()? as u64;
            let b0 = self.read_u8()? as u64;
            let unsigned = (b2 * 256 + b1) * 256 + b0;
            Ok(zigzag_decode(unsigned))
        } else if n0 == 254 {
            // 5-byte: [254, b3, b2, b1, b0]
            let b3 = self.read_u8()? as u64;
            let b2 = self.read_u8()? as u64;
            let b1 = self.read_u8()? as u64;
            let b0 = self.read_u8()? as u64;
            let unsigned = ((b3 * 256 + b2) * 256 + b1) * 256 + b0;
            Ok(zigzag_decode(unsigned))
        } else {
            // n0 == 255: 9-byte float64 LE escape
            let float_val = self.read_f64_le()?;
            Ok(float_val.floor() as i64)
        }
    }

    /// Decode an Elm Float: 8 bytes IEEE 754 LE.
    pub fn decode_float(&mut self) -> Result<ElmFloat, Wire3DecodeError> {
        let val = self.read_f64_le()?;
        Ok(ElmFloat::new(val))
    }

    /// Decode a bool: 1 byte (0=false, 1=true).
    pub fn decode_bool(&mut self) -> Result<bool, Wire3DecodeError> {
        let b = self.read_u8()?;
        match b {
            0 => Ok(false),
            1 => Ok(true),
            _ => Err(Wire3DecodeError::InvalidTag {
                tag: b,
                type_name: "Bool",
            }),
        }
    }

    /// Decode an Elm Char: code point as zigzag varint.
    pub fn decode_char(&mut self) -> Result<ElmChar, Wire3DecodeError> {
        let code = self.decode_int_raw()?;
        ElmChar::from_code_point(code as u32).map_err(Wire3DecodeError::from)
    }

    /// Decode a string: [varint utf8_byte_len][utf8 bytes].
    pub fn decode_string(&mut self) -> Result<String, Wire3DecodeError> {
        let len = self.decode_int_raw()? as usize;
        let bytes = self.read_bytes(len)?;
        String::from_utf8(bytes.to_vec()).map_err(|_| Wire3DecodeError::InvalidUtf8)
    }

    /// Decode raw bytes: [varint len][bytes].
    pub fn decode_bytes(&mut self) -> Result<Vec<u8>, Wire3DecodeError> {
        let len = self.decode_int_raw()? as usize;
        let bytes = self.read_bytes(len)?;
        Ok(bytes.to_vec())
    }

    /// Decode a u8 tag.
    pub fn decode_tag8(&mut self) -> Result<u8, Wire3DecodeError> {
        self.read_u8()
    }

    /// Decode a u16 tag (LE).
    pub fn decode_tag16(&mut self) -> Result<u16, Wire3DecodeError> {
        let bytes = self.read_bytes(2)?;
        Ok(u16::from_le_bytes(bytes.try_into().unwrap()))
    }

    /// Decode unit: reads zero bytes, returns ().
    pub fn decode_unit(&mut self) -> Result<(), Wire3DecodeError> {
        Ok(())
    }

    /// Decode Elm Order: LT=0, EQ=1, GT=2.
    pub fn decode_order(&mut self) -> Result<ElmOrder, Wire3DecodeError> {
        let b = self.read_u8()?;
        match b {
            0 => Ok(ElmOrder::LT),
            1 => Ok(ElmOrder::EQ),
            2 => Ok(ElmOrder::GT),
            _ => Err(Wire3DecodeError::InvalidTag {
                tag: b,
                type_name: "Order",
            }),
        }
    }

    // ── Container decoders ──────────────────────────────────────

    /// Decode a list: [varint count][elements...].
    pub fn decode_list<T>(
        &mut self,
        decode_elem: impl Fn(&mut Self) -> Result<T, Wire3DecodeError>,
    ) -> Result<Vec<T>, Wire3DecodeError> {
        let count = self.decode_int_raw()? as usize;
        let mut items = Vec::with_capacity(count);
        for _ in 0..count {
            items.push(decode_elem(self)?);
        }
        Ok(items)
    }

    /// Decode a set: [varint count][elements...].
    pub fn decode_set<T: Ord>(
        &mut self,
        decode_elem: impl Fn(&mut Self) -> Result<T, Wire3DecodeError>,
    ) -> Result<BTreeSet<T>, Wire3DecodeError> {
        let items = self.decode_list(decode_elem)?;
        Ok(items.into_iter().collect())
    }

    /// Decode a dict: [varint count][k0,v0][k1,v1]...
    pub fn decode_dict<K: Ord, V>(
        &mut self,
        decode_key: impl Fn(&mut Self) -> Result<K, Wire3DecodeError>,
        decode_val: impl Fn(&mut Self) -> Result<V, Wire3DecodeError>,
    ) -> Result<BTreeMap<K, V>, Wire3DecodeError> {
        let count = self.decode_int_raw()? as usize;
        let mut map = BTreeMap::new();
        for _ in 0..count {
            let k = decode_key(self)?;
            let v = decode_val(self)?;
            map.insert(k, v);
        }
        Ok(map)
    }

    /// Decode Maybe: 0=Nothing, 1=Just(decode).
    pub fn decode_maybe<T>(
        &mut self,
        decode_inner: impl Fn(&mut Self) -> Result<T, Wire3DecodeError>,
    ) -> Result<Option<T>, Wire3DecodeError> {
        let tag = self.read_u8()?;
        match tag {
            0 => Ok(None),
            1 => Ok(Some(decode_inner(self)?)),
            _ => Err(Wire3DecodeError::InvalidTag {
                tag,
                type_name: "Maybe",
            }),
        }
    }

    /// Decode Result: 0=Ok(decode_ok), 1=Err(decode_err).
    /// Wire3.elm hardcodes Ok=0, Err=1.
    pub fn decode_result<E, V>(
        &mut self,
        decode_err: impl Fn(&mut Self) -> Result<E, Wire3DecodeError>,
        decode_ok: impl Fn(&mut Self) -> Result<V, Wire3DecodeError>,
    ) -> Result<Result<V, E>, Wire3DecodeError> {
        let tag = self.read_u8()?;
        match tag {
            0 => Ok(Ok(decode_ok(self)?)),
            1 => Ok(Err(decode_err(self)?)),
            _ => Err(Wire3DecodeError::InvalidTag {
                tag,
                type_name: "Result",
            }),
        }
    }

    /// Decode a pair: [a][b].
    pub fn decode_pair<A, B>(
        &mut self,
        decode_a: impl Fn(&mut Self) -> Result<A, Wire3DecodeError>,
        decode_b: impl Fn(&mut Self) -> Result<B, Wire3DecodeError>,
    ) -> Result<(A, B), Wire3DecodeError> {
        let a = decode_a(self)?;
        let b = decode_b(self)?;
        Ok((a, b))
    }

    /// Decode a triple: [a][b][c].
    pub fn decode_triple<A, B, C>(
        &mut self,
        decode_a: impl Fn(&mut Self) -> Result<A, Wire3DecodeError>,
        decode_b: impl Fn(&mut Self) -> Result<B, Wire3DecodeError>,
        decode_c: impl Fn(&mut Self) -> Result<C, Wire3DecodeError>,
    ) -> Result<(A, B, C), Wire3DecodeError> {
        let a = decode_a(self)?;
        let b = decode_b(self)?;
        let c = decode_c(self)?;
        Ok((a, b, c))
    }
}

/// Zigzag decode: map unsigned → signed.
/// even i → i/2 (positive), odd i → -(i+1)/2 (negative).
fn zigzag_decode(n: u64) -> i64 {
    if n % 2 == 0 {
        (n / 2) as i64
    } else {
        -(((n + 1) / 2) as i64)
    }
}

// ── Wire3Decode impls for Elm wrapper types ─────────────────────

impl Wire3Decode for ElmInt {
    fn wire3_decode(decoder: &mut Wire3Decoder) -> Result<Self, Wire3DecodeError> {
        decoder.decode_int()
    }
}

impl Wire3Decode for ElmFloat {
    fn wire3_decode(decoder: &mut Wire3Decoder) -> Result<Self, Wire3DecodeError> {
        decoder.decode_float()
    }
}

impl Wire3Decode for bool {
    fn wire3_decode(decoder: &mut Wire3Decoder) -> Result<Self, Wire3DecodeError> {
        decoder.decode_bool()
    }
}

impl Wire3Decode for ElmChar {
    fn wire3_decode(decoder: &mut Wire3Decoder) -> Result<Self, Wire3DecodeError> {
        decoder.decode_char()
    }
}

impl Wire3Decode for String {
    fn wire3_decode(decoder: &mut Wire3Decoder) -> Result<Self, Wire3DecodeError> {
        decoder.decode_string()
    }
}

impl Wire3Decode for ElmOrder {
    fn wire3_decode(decoder: &mut Wire3Decoder) -> Result<Self, Wire3DecodeError> {
        decoder.decode_order()
    }
}

impl Wire3Decode for () {
    fn wire3_decode(decoder: &mut Wire3Decoder) -> Result<Self, Wire3DecodeError> {
        decoder.decode_unit()
    }
}

impl<T: Wire3Decode> Wire3Decode for Vec<T> {
    fn wire3_decode(decoder: &mut Wire3Decoder) -> Result<Self, Wire3DecodeError> {
        decoder.decode_list(T::wire3_decode)
    }
}

impl<T: Wire3Decode + Ord> Wire3Decode for BTreeSet<T> {
    fn wire3_decode(decoder: &mut Wire3Decoder) -> Result<Self, Wire3DecodeError> {
        decoder.decode_set(T::wire3_decode)
    }
}

impl<K: Wire3Decode + Ord, V: Wire3Decode> Wire3Decode for BTreeMap<K, V> {
    fn wire3_decode(decoder: &mut Wire3Decoder) -> Result<Self, Wire3DecodeError> {
        decoder.decode_dict(K::wire3_decode, V::wire3_decode)
    }
}

impl<T: Wire3Decode> Wire3Decode for Option<T> {
    fn wire3_decode(decoder: &mut Wire3Decoder) -> Result<Self, Wire3DecodeError> {
        decoder.decode_maybe(T::wire3_decode)
    }
}

impl<A: Wire3Decode, B: Wire3Decode> Wire3Decode for (A, B) {
    fn wire3_decode(decoder: &mut Wire3Decoder) -> Result<Self, Wire3DecodeError> {
        decoder.decode_pair(A::wire3_decode, B::wire3_decode)
    }
}

impl<A: Wire3Decode, B: Wire3Decode, C: Wire3Decode> Wire3Decode for (A, B, C) {
    fn wire3_decode(decoder: &mut Wire3Decoder) -> Result<Self, Wire3DecodeError> {
        decoder.decode_triple(A::wire3_decode, B::wire3_decode, C::wire3_decode)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::wire3::Wire3Encoder;

    fn round_trip_int(v: i64) {
        let elm_v = ElmInt::new(v).unwrap();
        let mut enc = Wire3Encoder::new();
        enc.encode_int(&elm_v);
        let bytes = enc.into_bytes();
        let mut dec = Wire3Decoder::new(&bytes);
        let result = dec.decode_int().unwrap();
        assert_eq!(elm_v, result, "round-trip failed for {v}");
        assert!(dec.is_empty(), "leftover bytes for {v}");
    }

    #[test]
    fn round_trip_small_ints() {
        for v in -200..=200 {
            round_trip_int(v);
        }
    }

    #[test]
    fn round_trip_boundary_ints() {
        let boundaries = [
            0,
            107,
            108,
            -108,
            -109,
            4715,
            4716,
            -4716,
            -4717,
            32767,
            32768,
            -32768,
            -32769,
            8_388_607,
            8_388_608,
            -8_388_608,
            -8_388_609,
            2_147_483_647,
            2_147_483_648,
            -2_147_483_648,
            -2_147_483_649,
            ElmInt::MAX,
            ElmInt::MIN,
        ];
        for v in boundaries {
            round_trip_int(v);
        }
    }

    #[test]
    fn round_trip_float() {
        let values = [0.0, 1.0, -1.0, 3.14159, f64::MAX, f64::MIN, f64::EPSILON];
        for v in values {
            let elm_v = ElmFloat::new(v);
            let mut enc = Wire3Encoder::new();
            enc.encode_float(&elm_v);
            let bytes = enc.into_bytes();
            let mut dec = Wire3Decoder::new(&bytes);
            let result = dec.decode_float().unwrap();
            assert_eq!(elm_v, result, "round-trip failed for {v}");
        }
    }

    #[test]
    fn round_trip_bool() {
        for v in [false, true] {
            let mut enc = Wire3Encoder::new();
            enc.encode_bool(v);
            let bytes = enc.into_bytes();
            let mut dec = Wire3Decoder::new(&bytes);
            assert_eq!(dec.decode_bool().unwrap(), v);
        }
    }

    #[test]
    fn round_trip_string() {
        let cases = ["", "hello", "héllo wörld", "🎉🚀", "日本語"];
        for s in cases {
            let mut enc = Wire3Encoder::new();
            enc.encode_string(s);
            let bytes = enc.into_bytes();
            let mut dec = Wire3Decoder::new(&bytes);
            assert_eq!(dec.decode_string().unwrap(), s);
            assert!(dec.is_empty());
        }
    }

    #[test]
    fn round_trip_char() {
        let chars = ['a', 'Z', '0', 'é', '🎉', '日'];
        for c in chars {
            let elm_c = ElmChar::new(c);
            let mut enc = Wire3Encoder::new();
            enc.encode_char(&elm_c);
            let bytes = enc.into_bytes();
            let mut dec = Wire3Decoder::new(&bytes);
            assert_eq!(dec.decode_char().unwrap(), elm_c);
        }
    }

    #[test]
    fn round_trip_maybe() {
        // Nothing
        let mut enc = Wire3Encoder::new();
        enc.encode_maybe(&None::<ElmInt>, |e, v| e.encode_int(v));
        let bytes = enc.into_bytes();
        let mut dec = Wire3Decoder::new(&bytes);
        let result: Option<ElmInt> = dec.decode_maybe(|d| d.decode_int()).unwrap();
        assert_eq!(result, None);

        // Just 42
        let val = Some(ElmInt::new(42).unwrap());
        let mut enc = Wire3Encoder::new();
        enc.encode_maybe(&val, |e, v| e.encode_int(v));
        let bytes = enc.into_bytes();
        let mut dec = Wire3Decoder::new(&bytes);
        let result: Option<ElmInt> = dec.decode_maybe(|d| d.decode_int()).unwrap();
        assert_eq!(result, val);
    }

    #[test]
    fn round_trip_list() {
        let items: Vec<ElmInt> = (0..5).map(|i| ElmInt::new(i).unwrap()).collect();
        let mut enc = Wire3Encoder::new();
        enc.encode_list(&items, |e, v| e.encode_int(v));
        let bytes = enc.into_bytes();
        let mut dec = Wire3Decoder::new(&bytes);
        let result = dec.decode_list(|d| d.decode_int()).unwrap();
        assert_eq!(items, result);
    }

    #[test]
    fn round_trip_result() {
        // Ok 10
        let val: Result<ElmInt, String> = Ok(ElmInt::new(10).unwrap());
        let mut enc = Wire3Encoder::new();
        enc.encode_result(
            &val,
            |e, s| e.encode_string(s),
            |e, v| e.encode_int(v),
        );
        let bytes = enc.into_bytes();
        let mut dec = Wire3Decoder::new(&bytes);
        let result = dec
            .decode_result(|d| d.decode_string(), |d| d.decode_int())
            .unwrap();
        assert_eq!(result, Ok(ElmInt::new(10).unwrap()));

        // Err "oops"
        let val: Result<ElmInt, String> = Err("oops".to_string());
        let mut enc = Wire3Encoder::new();
        enc.encode_result(
            &val,
            |e, s| e.encode_string(s),
            |e, v| e.encode_int(v),
        );
        let bytes = enc.into_bytes();
        let mut dec = Wire3Decoder::new(&bytes);
        let result = dec
            .decode_result(|d| d.decode_string(), |d| d.decode_int())
            .unwrap();
        assert_eq!(result, Err("oops".to_string()));
    }

    #[test]
    fn round_trip_order() {
        for v in [ElmOrder::LT, ElmOrder::EQ, ElmOrder::GT] {
            let mut enc = Wire3Encoder::new();
            enc.encode_order(v);
            let bytes = enc.into_bytes();
            let mut dec = Wire3Decoder::new(&bytes);
            assert_eq!(dec.decode_order().unwrap(), v);
        }
    }

    #[test]
    fn zigzag_symmetry() {
        for n in 0..1000u64 {
            let signed = zigzag_decode(n);
            let unsigned = crate::wire3::encode::zigzag_encode(signed);
            assert_eq!(unsigned, n, "zigzag symmetry failed for {n}");
        }
    }
}
