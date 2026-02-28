//! Hex encoding/decoding and convenience functions for Wire3 binary data.

use super::types::{Wire3Decode, Wire3DecodeError, Wire3Encode};
use super::{Wire3Decoder, Wire3Encoder};

/// Encode bytes as a lowercase hex string.
pub fn hex_encode(bytes: &[u8]) -> String {
    bytes.iter().map(|b| format!("{b:02x}")).collect()
}

/// Decode a hex string into bytes. Accepts both uppercase and lowercase.
pub fn hex_decode(hex: &str) -> Result<Vec<u8>, Wire3DecodeError> {
    if hex.len() % 2 != 0 {
        return Err(Wire3DecodeError::InvalidHex {
            reason: "odd number of hex characters",
        });
    }
    (0..hex.len())
        .step_by(2)
        .map(|i| {
            u8::from_str_radix(&hex[i..i + 2], 16).map_err(|_| Wire3DecodeError::InvalidHex {
                reason: "invalid hex character",
            })
        })
        .collect()
}

/// Encode a Wire3 value directly to a hex string.
pub fn wire3_encode_to_hex<T: Wire3Encode>(val: &T) -> String {
    let mut enc = Wire3Encoder::new();
    val.wire3_encode(&mut enc);
    hex_encode(&enc.into_bytes())
}

/// Decode a Wire3 value from a hex string. Errors if there are trailing bytes.
pub fn wire3_decode_from_hex<T: Wire3Decode>(hex: &str) -> Result<T, Wire3DecodeError> {
    let bytes = hex_decode(hex)?;
    let mut dec = Wire3Decoder::new(&bytes);
    let val = T::wire3_decode(&mut dec)?;
    if !dec.is_empty() {
        return Err(Wire3DecodeError::TrailingBytes {
            remaining: dec.remaining(),
        });
    }
    Ok(val)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::wire3::types::ElmInt;

    #[test]
    fn hex_roundtrip() {
        let bytes = vec![0x00, 0x0a, 0xff, 0x42];
        let hex = hex_encode(&bytes);
        assert_eq!(hex, "000aff42");
        assert_eq!(hex_decode(&hex).unwrap(), bytes);
    }

    #[test]
    fn hex_decode_uppercase() {
        assert_eq!(hex_decode("0AFF").unwrap(), vec![0x0a, 0xff]);
    }

    #[test]
    fn hex_decode_odd_length_fails() {
        assert!(hex_decode("0af").is_err());
    }

    #[test]
    fn hex_decode_invalid_char_fails() {
        assert!(hex_decode("0gff").is_err());
    }

    #[test]
    fn wire3_hex_convenience_roundtrip() {
        let val = ElmInt::new(42).unwrap();
        let hex = wire3_encode_to_hex(&val);
        let decoded: ElmInt = wire3_decode_from_hex(&hex).unwrap();
        assert_eq!(val, decoded);
    }

    #[test]
    fn wire3_hex_trailing_bytes_detected() {
        let hex = wire3_encode_to_hex(&ElmInt::new(1).unwrap());
        // Append extra bytes
        let bad_hex = format!("{hex}ff");
        let result: Result<ElmInt, _> = wire3_decode_from_hex(&bad_hex);
        assert!(matches!(result, Err(Wire3DecodeError::TrailingBytes { .. })));
    }
}
