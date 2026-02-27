// Elm wrapper types for Rust.
//
// These exist because Elm's runtime semantics differ from Rust's native types.
// For example, Elm's Int is a JS float64 under the hood, so it only has 53 bits
// of integer precision. These wrappers enforce Elm-compatible constraints and
// make the Elm-origin of these types explicit in generated code.

use std::collections::{BTreeMap, BTreeSet};
use std::fmt;

/// Elm Int: a signed integer with 53-bit precision (JS Number.MAX_SAFE_INTEGER).
/// Range: -(2^53 - 1) to (2^53 - 1), i.e. ±9,007,199,254,740,991.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ElmInt(i64);

impl ElmInt {
    pub const MAX: i64 = 9_007_199_254_740_991; // 2^53 - 1
    pub const MIN: i64 = -9_007_199_254_740_991;

    pub fn new(value: i64) -> Result<Self, ElmTypeError> {
        if value < Self::MIN || value > Self::MAX {
            Err(ElmTypeError::IntOutOfRange(value))
        } else {
            Ok(ElmInt(value))
        }
    }

    /// Create without range check — use only when you know the value is safe.
    pub fn new_unchecked(value: i64) -> Self {
        ElmInt(value)
    }

    pub fn value(self) -> i64 {
        self.0
    }
}

impl fmt::Display for ElmInt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl From<i32> for ElmInt {
    fn from(v: i32) -> Self {
        ElmInt(v as i64)
    }
}

/// Elm Float: IEEE 754 double-precision (f64). Same as Rust's f64.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ElmFloat(f64);

impl ElmFloat {
    pub fn new(value: f64) -> Self {
        ElmFloat(value)
    }

    pub fn value(self) -> f64 {
        self.0
    }
}

impl fmt::Display for ElmFloat {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl From<f64> for ElmFloat {
    fn from(v: f64) -> Self {
        ElmFloat(v)
    }
}

/// Elm Bool: True or False.
pub type ElmBool = bool;

/// Elm Char: a Unicode code point (0..0x10FFFF).
/// Elm uses JS strings (UTF-16) but Char.toCode gives the full code point.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ElmChar(char);

impl ElmChar {
    pub fn new(c: char) -> Self {
        ElmChar(c)
    }

    pub fn code_point(self) -> u32 {
        self.0 as u32
    }

    pub fn from_code_point(cp: u32) -> Result<Self, ElmTypeError> {
        char::from_u32(cp)
            .map(ElmChar)
            .ok_or(ElmTypeError::InvalidCodePoint(cp))
    }

    pub fn value(self) -> char {
        self.0
    }
}

impl fmt::Display for ElmChar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl From<char> for ElmChar {
    fn from(c: char) -> Self {
        ElmChar(c)
    }
}

/// Elm String: UTF-8 string. Wire3 encodes as [varint byte_len][utf8 bytes].
pub type ElmString = String;

/// Elm Bytes: raw byte buffer.
pub type ElmBytes = Vec<u8>;

/// Elm List: ordered collection (Vec in Rust).
pub type ElmList<T> = Vec<T>;

/// Elm Array: indexed collection (also Vec in Rust — same wire format as List).
pub type ElmArray<T> = Vec<T>;

/// Elm Set: ordered unique collection (BTreeSet for deterministic ordering).
pub type ElmSet<T> = BTreeSet<T>;

/// Elm Dict: ordered key-value map (BTreeMap for deterministic ordering).
pub type ElmDict<K, V> = BTreeMap<K, V>;

/// Elm Maybe: Optional value.
pub type ElmMaybe<T> = Option<T>;

/// Elm Result: Err or Ok. Note Elm's type params are Result error value,
/// while Rust's are Result<value, error>. The type alias handles the flip.
pub type ElmResult<E, V> = Result<V, E>;

/// Elm Order: LT | EQ | GT
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ElmOrder {
    LT,
    EQ,
    GT,
}

/// Elm Unit: ()
pub type ElmUnit = ();

/// Elm Pair: (a, b) — no length prefix in Wire3.
pub type ElmPair<A, B> = (A, B);

/// Elm Triple: (a, b, c) — no length prefix in Wire3.
pub type ElmTriple<A, B, C> = (A, B, C);

/// Errors from Elm type operations.
#[derive(Debug, Clone, thiserror::Error)]
pub enum ElmTypeError {
    #[error("Int value {0} is outside Elm's safe range (±2^53 - 1)")]
    IntOutOfRange(i64),

    #[error("Invalid Unicode code point: {0:#x}")]
    InvalidCodePoint(u32),
}

/// Trait for types that can be encoded to Wire3 format.
pub trait Wire3Encode {
    fn wire3_encode(&self, encoder: &mut super::Wire3Encoder);
}

/// Trait for types that can be decoded from Wire3 format.
pub trait Wire3Decode: Sized {
    fn wire3_decode(decoder: &mut super::Wire3Decoder) -> Result<Self, Wire3DecodeError>;
}

/// Errors from Wire3 decoding.
#[derive(Debug, Clone, thiserror::Error)]
pub enum Wire3DecodeError {
    #[error("unexpected end of input: needed {needed} bytes, got {available}")]
    UnexpectedEof { needed: usize, available: usize },

    #[error("invalid tag byte {tag} for {type_name}")]
    InvalidTag { tag: u8, type_name: &'static str },

    #[error("invalid UTF-8 in string")]
    InvalidUtf8,

    #[error("Elm type error: {0}")]
    ElmType(#[from] ElmTypeError),

    #[error("{context}: {source}")]
    Context {
        context: String,
        source: Box<Wire3DecodeError>,
    },
}

impl Wire3DecodeError {
    pub fn context(self, ctx: impl Into<String>) -> Self {
        Wire3DecodeError::Context {
            context: ctx.into(),
            source: Box::new(self),
        }
    }
}
