//@ charon-args=--extract-opaque-bodies
//! Test for pattern types (https://github.com/rust-lang/rust/issues/123646).
//! We use `--extract-opaque-bodies` to expose the pattern types hidden inside
//! standard library types like `NonZero` and `NonNull`.
#![feature(pattern_types)]
#![feature(pattern_type_macro)]

use std::num::NonZero;
use std::ptr::NonNull;

/// A custom pattern type: simple range (Range pattern).
pub type NonZeroU32Custom = core::pattern_type!(u32 is 1..);

pub fn use_custom(x: NonZeroU32Custom) -> u32 {
    unsafe { core::mem::transmute(x) }
}

/// Exercises NonNull (pointer type).
pub fn use_nonnull(x: NonNull<u8>) -> *mut u8 {
    x.as_ptr()
}

/// Exercises NonZero<i64>, whose inner type uses a disjunction (Or pattern):
/// `i64 is ..-1 | 0..` (equivalent to `I64NotAllOnes`).
pub fn use_nonzero_i64(x: NonZero<i64>) -> i64 {
    x.get()
}

/// Exercises NonZero<char>, whose inner type uses a char range:
/// `char is '\u{1}'..='\u{10ffff}'` (equivalent to `NonZeroCharInner`).
pub fn use_nonzero_char(x: NonZero<char>) -> char {
    x.get()
}
