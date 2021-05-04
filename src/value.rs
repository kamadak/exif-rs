//
// Copyright (c) 2016 KAMADA Ken'ichi.
// All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions
// are met:
// 1. Redistributions of source code must retain the above copyright
//    notice, this list of conditions and the following disclaimer.
// 2. Redistributions in binary form must reproduce the above copyright
//    notice, this list of conditions and the following disclaimer in the
//    documentation and/or other materials provided with the distribution.
//
// THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
// ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
// IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
// ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
// FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
// DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
// OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
// HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
// LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
// OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
// SUCH DAMAGE.
//

use std::fmt;
use std::fmt::Write as _;

use crate::endian::Endian;

/// A type and values of a TIFF/Exif field.
#[derive(Clone)]
pub enum Value {
    /// Vector of 8-bit unsigned integers.
    Byte(Vec<u8>),
    /// Vector of slices of 8-bit bytes containing 7-bit ASCII characters.
    /// The trailing null characters are not included.  Note that
    /// the 8th bits may present if a non-conforming data is given.
    Ascii(Vec<Vec<u8>>),
    /// Vector of 16-bit unsigned integers.
    Short(Vec<u16>),
    /// Vector of 32-bit unsigned integers.
    Long(Vec<u32>),
    /// Vector of unsigned rationals.
    /// An unsigned rational number is a pair of 32-bit unsigned integers.
    Rational(Vec<Rational>),
    /// Vector of 8-bit signed integers.  Unused in the Exif specification.
    SByte(Vec<i8>),
    /// Slice of 8-bit bytes.
    ///
    /// The second member keeps the offset of the value in the Exif data.
    /// The interpretation of the value does not generally depend on
    /// the location, but if it does, the offset information helps.
    /// When encoding Exif, it is ignored.
    Undefined(Vec<u8>, u32),
    /// Vector of 16-bit signed integers.  Unused in the Exif specification.
    SShort(Vec<i16>),
    /// Vector of 32-bit signed integers.
    SLong(Vec<i32>),
    /// Vector of signed rationals.
    /// A signed rational number is a pair of 32-bit signed integers.
    SRational(Vec<SRational>),
    /// Vector of 32-bit (single precision) floating-point numbers.
    /// Unused in the Exif specification.
    Float(Vec<f32>),
    /// Vector of 64-bit (double precision) floating-point numbers.
    /// Unused in the Exif specification.
    Double(Vec<f64>),
    /// The type is unknown to this implementation.
    /// The associated values are the type, the count, and the
    /// offset of the "Value Offset" element.
    Unknown(u16, u32, u32),
}

impl Value {
    /// Returns an object that implements `std::fmt::Display` for
    /// printing a value in a tag-specific format.
    /// The tag of the value is specified as the argument.
    ///
    /// If you want to display with the unit, use `Field::display_value`.
    ///
    /// # Examples
    ///
    /// ```
    /// use exif::{Value, Tag};
    /// let val = Value::Undefined(b"0231".to_vec(), 0);
    /// assert_eq!(val.display_as(Tag::ExifVersion).to_string(), "2.31");
    /// let val = Value::Short(vec![2]);
    /// assert_eq!(val.display_as(Tag::ResolutionUnit).to_string(), "inch");
    /// ```
    #[inline]
    pub fn display_as(&self, tag: crate::tag::Tag) -> Display {
        crate::tag::display_value_as(self, tag)
    }

    /// Returns the value as a slice if the type is BYTE.
    #[inline]
    pub(crate) fn byte(&self) -> Option<&[u8]> {
        match *self {
            Value::Byte(ref v) => Some(v),
            _ => None,
        }
    }

    /// Returns the value as `AsciiValues` if the type is ASCII.
    #[inline]
    pub(crate) fn ascii(&self) -> Option<AsciiValues> {
        match *self {
            Value::Ascii(ref v) => Some(AsciiValues(v)),
            _ => None,
        }
    }

    /// Returns the value as a slice if the type is RATIONAL.
    #[inline]
    pub(crate) fn rational(&self) -> Option<&[Rational]> {
        match *self {
            Value::Rational(ref v) => Some(v),
            _ => None,
        }
    }

    /// Returns the value as a slice if the type is UNDEFINED.
    #[inline]
    pub(crate) fn undefined(&self) -> Option<&[u8]> {
        match *self {
            Value::Undefined(ref v, _) => Some(v),
            _ => None,
        }
    }

    /// Returns the unsigned integer at the given position.
    /// None is returned if the value type is not unsigned integer
    /// (BYTE, SHORT, or LONG) or the position is out of bounds.
    pub fn get_uint(&self, index: usize) -> Option<u32> {
        match *self {
            Value::Byte(ref v) if v.len() > index => Some(v[index] as u32),
            Value::Short(ref v) if v.len() > index => Some(v[index] as u32),
            Value::Long(ref v) if v.len() > index => Some(v[index]),
            _ => None,
        }
    }

    /// Returns an iterator over the unsigned integers (BYTE, SHORT, or LONG).
    /// The iterator yields `u32` regardless of the underlying integer size.
    /// The returned iterator implements `Iterator` and `ExactSizeIterator`
    /// traits.
    /// `None` is returned if the value is not an unsigned integer type.
    #[inline]
    pub fn iter_uint(&self) -> Option<UIntIter> {
        match *self {
            Value::Byte(ref v) =>
                Some(UIntIter { iter: Box::new(v.iter().map(|&x| x as u32)) }),
            Value::Short(ref v) =>
                Some(UIntIter { iter: Box::new(v.iter().map(|&x| x as u32)) }),
            Value::Long(ref v) =>
                Some(UIntIter { iter: Box::new(v.iter().map(|&x| x)) }),
            _ => None,
        }
    }
}

pub struct AsciiValues<'a>(&'a [Vec<u8>]);

impl<'a> AsciiValues<'a> {
    pub fn first(&self) -> Option<&'a [u8]> {
        self.0.first().map(|x| &x[..])
    }
}

// A struct that wraps std::slice::Iter<'a, u8/u16/u32>.
pub struct UIntIter<'a> {
    iter: Box<dyn ExactSizeIterator<Item=u32> + 'a>
}

impl<'a> Iterator for UIntIter<'a> {
    type Item = u32;

    #[inline]
    fn next(&mut self) -> Option<u32> {
        self.iter.next()
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.iter.size_hint()
    }
}

impl<'a> ExactSizeIterator for UIntIter<'a> {}

/// Helper struct for printing a value in a tag-specific format.
#[derive(Copy, Clone)]
pub struct Display<'a> {
    pub fmt: fn(&mut dyn fmt::Write, &Value) -> fmt::Result,
    pub value: &'a Value,
}

impl<'a> fmt::Display for Display<'a> {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        (self.fmt)(f, self.value)
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Byte(v) => f.debug_tuple("Byte").field(v).finish(),
            Self::Ascii(v) => f.debug_tuple("Ascii")
                .field(&IterDebugAdapter(
                    || v.iter().map(|x| AsciiDebugAdapter(x)))).finish(),
            Self::Short(v) => f.debug_tuple("Short").field(v).finish(),
            Self::Long(v) => f.debug_tuple("Long").field(v).finish(),
            Self::Rational(v) => f.debug_tuple("Rational").field(v).finish(),
            Self::SByte(v) => f.debug_tuple("SByte").field(v).finish(),
            Self::Undefined(v, o) => f.debug_tuple("Undefined")
                .field(&HexDebugAdapter(v))
                .field(&format_args!("ofs={:#x}", o)).finish(),
            Self::SShort(v) => f.debug_tuple("SShort").field(v).finish(),
            Self::SLong(v) => f.debug_tuple("SLong").field(v).finish(),
            Self::SRational(v) => f.debug_tuple("SRational").field(v).finish(),
            Self::Float(v) => f.debug_tuple("Float").field(v).finish(),
            Self::Double(v) => f.debug_tuple("Double").field(v).finish(),
            Self::Unknown(t, c, oo) => f.debug_tuple("Unknown")
                .field(&format_args!("typ={}", t))
                .field(&format_args!("cnt={}", c))
                .field(&format_args!("ofs={:#x}", oo)).finish(),
        }
    }
}

struct IterDebugAdapter<F>(F);

impl<F, T, I> fmt::Debug for IterDebugAdapter<F>
where F: Fn() -> T, T: Iterator<Item = I>, I: fmt::Debug {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_list().entries(self.0()).finish()
    }
}

struct AsciiDebugAdapter<'a>(&'a [u8]);

impl<'a> fmt::Debug for AsciiDebugAdapter<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_char('"')?;
        self.0.iter().try_for_each(|&c| match c {
            b'\\' | b'"' => write!(f, "\\{}", c as char),
            0x20..=0x7e => f.write_char(c as char),
            _ => write!(f, "\\x{:02x}", c),
        })?;
        f.write_char('"')
    }
}

struct HexDebugAdapter<'a>(&'a [u8]);

impl<'a> fmt::Debug for HexDebugAdapter<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str("0x")?;
        self.0.iter().try_for_each(|x| write!(f, "{:02x}", x))
    }
}

// Static default values.
pub enum DefaultValue {
    None,
    Byte(&'static [u8]),
    Ascii(&'static [&'static [u8]]),
    Short(&'static [u16]),
    Rational(&'static [(u32, u32)]),
    Undefined(&'static [u8]),
    // Depends on other tags, JPEG markers, etc.
    ContextDependent,
    // Unspecified in the Exif standard.
    Unspecified,
}

impl From<&DefaultValue> for Option<Value> {
    fn from(defval: &DefaultValue) -> Option<Value> {
        match *defval {
            DefaultValue::None => None,
            DefaultValue::Byte(s) => Some(Value::Byte(s.to_vec())),
            DefaultValue::Ascii(s) => Some(Value::Ascii(
                s.iter().map(|&x| x.to_vec()).collect())),
            DefaultValue::Short(s) => Some(Value::Short(s.to_vec())),
            DefaultValue::Rational(s) => Some(Value::Rational(
                s.iter().map(|&x| x.into()).collect())),
            DefaultValue::Undefined(s) => Some(Value::Undefined(
                s.to_vec(), 0)),
            DefaultValue::ContextDependent => None,
            DefaultValue::Unspecified => None,
        }
    }
}

/// An unsigned rational number, which is a pair of 32-bit unsigned integers.
#[derive(Copy, Clone)]
pub struct Rational { pub num: u32, pub denom: u32 }

impl Rational {
    /// Converts the value to an f32.
    #[inline]
    pub fn to_f32(&self) -> f32 {
        self.to_f64() as f32
    }

    /// Converts the value to an f64.
    #[inline]
    pub fn to_f64(&self) -> f64 {
        self.num as f64 / self.denom as f64
    }
}

impl From<(u32, u32)> for Rational {
    fn from(t: (u32, u32)) -> Rational {
        Rational { num: t.0, denom: t.1 }
    }
}

impl fmt::Debug for Rational {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Rational({}/{})", self.num, self.denom)
    }
}

impl fmt::Display for Rational {
    /// Formatting parameters other than width are not supported.
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let buf = fmt_rational_sub(f, self.num, self.denom);
        f.pad_integral(true, "", &buf)
    }
}

// This implementation has been deprecated.  Use Rational::to_f64 instead.
impl From<Rational> for f64 {
    #[inline]
    fn from(r: Rational) -> f64 { r.to_f64() }
}

// This implementation has been deprecated.  Use Rational::to_f32 instead.
impl From<Rational> for f32 {
    #[inline]
    fn from(r: Rational) -> f32 { r.to_f32() }
}

/// A signed rational number, which is a pair of 32-bit signed integers.
#[derive(Copy, Clone)]
pub struct SRational { pub num: i32, pub denom: i32 }

impl SRational {
    /// Converts the value to an f32.
    #[inline]
    pub fn to_f32(&self) -> f32 {
        self.to_f64() as f32
    }

    /// Converts the value to an f64.
    #[inline]
    pub fn to_f64(&self) -> f64 {
        self.num as f64 / self.denom as f64
    }
}

impl From<(i32, i32)> for SRational {
    fn from(t: (i32, i32)) -> SRational {
        SRational { num: t.0, denom: t.1 }
    }
}

impl fmt::Debug for SRational {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "SRational({}/{})", self.num, self.denom)
    }
}

impl fmt::Display for SRational {
    /// Formatting parameters other than width are not supported.
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let buf = fmt_rational_sub(
            f, self.num.wrapping_abs() as u32, self.denom);
        f.pad_integral(self.num >= 0, "", &buf)
    }
}

// This implementation has been deprecated.  Use SRational::to_f64 instead.
impl From<SRational> for f64 {
    #[inline]
    fn from(r: SRational) -> f64 { r.to_f64() }
}

// This implementation has been deprecated.  Use SRational::to_f32 instead.
impl From<SRational> for f32 {
    #[inline]
    fn from(r: SRational) -> f32 { r.to_f32() }
}

// Only u32 or i32 are expected for T.
fn fmt_rational_sub<T>(f: &mut fmt::Formatter, num: u32, denom: T)
                       -> String where T: fmt::Display {
    // The API to get the alignment is not yet stable as of Rust 1.16,
    // so it is not fully supported.
    match (f.sign_plus(), f.precision(), f.sign_aware_zero_pad()) {
        (true, Some(prec), true) =>
            format!("{}/{:+0w$}", num, denom, w = prec),
        (true, Some(prec), false) =>
            format!("{}/{:+w$}", num, denom, w = prec),
        (true, None, _) =>
            format!("{}/{:+}", num, denom),
        (false, Some(prec), true) =>
            format!("{}/{:0w$}", num, denom, w = prec),
        (false, Some(prec), false) =>
            format!("{}/{:w$}", num, denom, w = prec),
        (false, None, _) =>
            format!("{}/{}", num, denom),
    }
}

type Parser = fn(&[u8], usize, usize) -> Value;

// Return the length of a single value and the parser of the type.
pub fn get_type_info<E>(typecode: u16) -> (usize, Parser) where E: Endian {
    match typecode {
        1 => (1, parse_byte),
        2 => (1, parse_ascii),
        3 => (2, parse_short::<E>),
        4 => (4, parse_long::<E>),
        5 => (8, parse_rational::<E>),
        6 => (1, parse_sbyte),
        7 => (1, parse_undefined),
        8 => (2, parse_sshort::<E>),
        9 => (4, parse_slong::<E>),
        10 => (8, parse_srational::<E>),
        11 => (4, parse_float::<E>),
        12 => (8, parse_double::<E>),
        _ => (0, parse_unknown),
    }
}

fn parse_byte(data: &[u8], offset: usize, count: usize) -> Value {
    Value::Byte(data[offset .. offset + count].to_vec())
}

fn parse_ascii(data: &[u8], offset: usize, count: usize) -> Value {
    // Any ASCII field can contain multiple strings [TIFF6 Image File
    // Directory].
    let iter = (&data[offset .. offset + count]).split(|&b| b == b'\0');
    let mut v: Vec<Vec<u8>> = iter.map(|x| x.to_vec()).collect();
    if v.last().map_or(false, |x| x.len() == 0) {
        v.pop();
    }
    Value::Ascii(v)
}

fn parse_short<E>(data: &[u8], offset: usize, count: usize)
                  -> Value where E: Endian {
    let mut val = Vec::with_capacity(count);
    for i in 0..count {
        val.push(E::loadu16(data, offset + i * 2));
    }
    Value::Short(val)
}

fn parse_long<E>(data: &[u8], offset: usize, count: usize)
                 -> Value where E: Endian {
    let mut val = Vec::with_capacity(count);
    for i in 0..count {
        val.push(E::loadu32(data, offset + i * 4));
    }
    Value::Long(val)
}

fn parse_rational<E>(data: &[u8], offset: usize, count: usize)
                     -> Value where E: Endian {
    let mut val = Vec::with_capacity(count);
    for i in 0..count {
        val.push(Rational {
            num: E::loadu32(data, offset + i * 8),
            denom: E::loadu32(data, offset + i * 8 + 4),
        });
    }
    Value::Rational(val)
}

fn parse_sbyte(data: &[u8], offset: usize, count: usize) -> Value {
    let bytes = data[offset .. offset + count].iter()
        .map(|x| *x as i8).collect();
    Value::SByte(bytes)
}

fn parse_undefined(data: &[u8], offset: usize, count: usize) -> Value {
    Value::Undefined(data[offset .. offset + count].to_vec(), offset as u32)
}

fn parse_sshort<E>(data: &[u8], offset: usize, count: usize)
                   -> Value where E: Endian {
    let mut val = Vec::with_capacity(count);
    for i in 0..count {
        val.push(E::loadu16(data, offset + i * 2) as i16);
    }
    Value::SShort(val)
}

fn parse_slong<E>(data: &[u8], offset: usize, count: usize)
                  -> Value where E: Endian {
    let mut val = Vec::with_capacity(count);
    for i in 0..count {
        val.push(E::loadu32(data, offset + i * 4) as i32);
    }
    Value::SLong(val)
}

fn parse_srational<E>(data: &[u8], offset: usize, count: usize)
                      -> Value where E: Endian {
    let mut val = Vec::with_capacity(count);
    for i in 0..count {
        val.push(SRational {
            num: E::loadu32(data, offset + i * 8) as i32,
            denom: E::loadu32(data, offset + i * 8 + 4) as i32,
        });
    }
    Value::SRational(val)
}

// TIFF and Rust use IEEE 754 format, so no conversion is required.
fn parse_float<E>(data: &[u8], offset: usize, count: usize)
                  -> Value where E: Endian {
    let mut val = Vec::with_capacity(count);
    for i in 0..count {
        val.push(f32::from_bits(E::loadu32(data, offset + i * 4)));
    }
    Value::Float(val)
}

// TIFF and Rust use IEEE 754 format, so no conversion is required.
fn parse_double<E>(data: &[u8], offset: usize, count: usize)
                   -> Value where E: Endian {
    let mut val = Vec::with_capacity(count);
    for i in 0..count {
        val.push(f64::from_bits(E::loadu64(data, offset + i * 8)));
    }
    Value::Double(val)
}

// This is a dummy function and will never be called.
#[allow(unused_variables)]
fn parse_unknown(data: &[u8], offset: usize, count: usize) -> Value {
    unreachable!()
}

#[cfg(test)]
mod tests {
    use crate::endian::BigEndian;
    use super::*;

    #[test]
    fn byte() {
        let sets: &[(&[u8], &[u8])] = &[
            (b"x", b""),
            (b"x\xbe\xad", b"\xbe\xad"),
        ];
        let (unitlen, parser) = get_type_info::<BigEndian>(1);
        for &(data, ans) in sets {
            assert!((data.len() - 1) % unitlen == 0);
            match parser(data, 1, (data.len() - 1) / unitlen) {
                Value::Byte(v) => assert_eq!(v, ans),
                v => panic!("wrong variant {:?}", v),
            }
        }
    }

    #[test]
    fn ascii() {
        let sets: &[(&[u8], Vec<&[u8]>)] = &[
            (b"x", vec![]),				// malformed
            (b"x\0", vec![b""]),
            (b"x\0\0", vec![b"", b""]),
            (b"xA", vec![b"A"]),			// malformed
            (b"xA\0", vec![b"A"]),
            (b"xA\0B", vec![b"A", b"B"]),		// malformed
            (b"xA\0B\0", vec![b"A", b"B"]),
            (b"xA\0\xbe\0", vec![b"A", b"\xbe"]),	// not ASCII
        ];
        let (unitlen, parser) = get_type_info::<BigEndian>(2);
        for &(data, ref ans) in sets {
            match parser(data, 1, (data.len() - 1) / unitlen) {
                Value::Ascii(v) => assert_eq!(v, *ans),
                v => panic!("wrong variant {:?}", v),
            }
        }
    }

    #[test]
    fn short() {
        let sets: &[(&[u8], Vec<u16>)] = &[
            (b"x", vec![]),
            (b"x\x01\x02\x03\x04", vec![0x0102, 0x0304]),
        ];
        let (unitlen, parser) = get_type_info::<BigEndian>(3);
        for &(data, ref ans) in sets {
            assert!((data.len() - 1) % unitlen == 0);
            match parser(data, 1, (data.len() - 1) / unitlen) {
                Value::Short(v) => assert_eq!(v, *ans),
                v => panic!("wrong variant {:?}", v),
            }
        }
    }

    #[test]
    fn long() {
        let sets: &[(&[u8], Vec<u32>)] = &[
            (b"x", vec![]),
            (b"x\x01\x02\x03\x04\x05\x06\x07\x08",
             vec![0x01020304, 0x05060708]),
        ];
        let (unitlen, parser) = get_type_info::<BigEndian>(4);
        for &(data, ref ans) in sets {
            assert!((data.len() - 1) % unitlen == 0);
            match parser(data, 1, (data.len() - 1) / unitlen) {
                Value::Long(v) => assert_eq!(v, *ans),
                v => panic!("wrong variant {:?}", v),
            }
        }
    }

    #[test]
    fn rational() {
        let sets: &[(&[u8], Vec<Rational>)] = &[
            (b"x", vec![]),
            (b"x\xa1\x02\x03\x04\x05\x06\x07\x08\
               \x09\x0a\x0b\x0c\xbd\x0e\x0f\x10",
             vec![(0xa1020304, 0x05060708).into(),
                  (0x090a0b0c, 0xbd0e0f10).into()]),
        ];
        let (unitlen, parser) = get_type_info::<BigEndian>(5);
        for &(data, ref ans) in sets {
            assert!((data.len() - 1) % unitlen == 0);
            match parser(data, 1, (data.len() - 1) / unitlen) {
                Value::Rational(v) => {
                    assert_eq!(v.len(), ans.len());
                    for (x, y) in v.iter().zip(ans.iter()) {
                        assert!(x.num == y.num && x.denom == y.denom);
                    }
                },
                v => panic!("wrong variant {:?}", v),
            }
        }
    }

    #[test]
    fn sbyte() {
        let sets: &[(&[u8], &[i8])] = &[
            (b"x", &[]),
            (b"x\xbe\x7d", &[-0x42, 0x7d]),
        ];
        let (unitlen, parser) = get_type_info::<BigEndian>(6);
        for &(data, ans) in sets {
            assert!((data.len() - 1) % unitlen == 0);
            match parser(data, 1, (data.len() - 1) / unitlen) {
                Value::SByte(v) => assert_eq!(v, ans),
                v => panic!("wrong variant {:?}", v),
            }
        }
    }

    #[test]
    fn undefined() {
        let sets: &[(&[u8], &[u8])] = &[
            (b"x", b""),
            (b"x\xbe\xad", b"\xbe\xad"),
        ];
        let (unitlen, parser) = get_type_info::<BigEndian>(7);
        for &(data, ans) in sets {
            assert!((data.len() - 1) % unitlen == 0);
            match parser(data, 1, (data.len() - 1) / unitlen) {
                Value::Undefined(v, o) => {
                    assert_eq!(v, ans);
                    assert_eq!(o, 1);
                },
                v => panic!("wrong variant {:?}", v),
            }
        }
    }

    #[test]
    fn sshort() {
        let sets: &[(&[u8], Vec<i16>)] = &[
            (b"x", vec![]),
            (b"x\x01\x02\xf3\x04", vec![0x0102, -0x0cfc]),
        ];
        let (unitlen, parser) = get_type_info::<BigEndian>(8);
        for &(data, ref ans) in sets {
            assert!((data.len() - 1) % unitlen == 0);
            match parser(data, 1, (data.len() - 1) / unitlen) {
                Value::SShort(v) => assert_eq!(v, *ans),
                v => panic!("wrong variant {:?}", v),
            }
        }
    }

    #[test]
    fn slong() {
        let sets: &[(&[u8], Vec<i32>)] = &[
            (b"x", vec![]),
            (b"x\x01\x02\x03\x04\x85\x06\x07\x08",
             vec![0x01020304, -0x7af9f8f8]),
        ];
        let (unitlen, parser) = get_type_info::<BigEndian>(9);
        for &(data, ref ans) in sets {
            assert!((data.len() - 1) % unitlen == 0);
            match parser(data, 1, (data.len() - 1) / unitlen) {
                Value::SLong(v) => assert_eq!(v, *ans),
                v => panic!("wrong variant {:?}", v),
            }
        }
    }

    #[test]
    fn srational() {
        let sets: &[(&[u8], Vec<SRational>)] = &[
            (b"x", vec![]),
            (b"x\xa1\x02\x03\x04\x05\x06\x07\x08\
               \x09\x0a\x0b\x0c\xbd\x0e\x0f\x10",
             vec![(-0x5efdfcfc, 0x05060708).into(),
                  (0x090a0b0c, -0x42f1f0f0).into()]),
        ];
        let (unitlen, parser) = get_type_info::<BigEndian>(10);
        for &(data, ref ans) in sets {
            assert!((data.len() - 1) % unitlen == 0);
            match parser(data, 1, (data.len() - 1) / unitlen) {
                Value::SRational(v) => {
                    assert_eq!(v.len(), ans.len());
                    for (x, y) in v.iter().zip(ans.iter()) {
                        assert!(x.num == y.num && x.denom == y.denom);
                    }
                },
                v => panic!("wrong variant {:?}", v),
            }
        }
    }

    #[test]
    fn float() {
        let sets: &[(&[u8], Vec<f32>)] = &[
            (b"x", vec![]),
            (b"x\x7f\x7f\xff\xff\x80\x80\x00\x00\x40\x00\x00\x00",
             vec![std::f32::MAX, -std::f32::MIN_POSITIVE, 2.0]),
        ];
        let (unitlen, parser) = get_type_info::<BigEndian>(11);
        for &(data, ref ans) in sets {
            assert!((data.len() - 1) % unitlen == 0);
            match parser(data, 1, (data.len() - 1) / unitlen) {
                Value::Float(v) => assert_eq!(v, *ans),
                v => panic!("wrong variant {:?}", v),
            }
        }
    }

    #[test]
    fn double() {
        let sets: &[(&[u8], Vec<f64>)] = &[
            (b"x", vec![]),
            (b"x\x7f\xef\xff\xff\xff\xff\xff\xff\
               \x80\x10\x00\x00\x00\x00\x00\x00\
               \x40\x00\x00\x00\x00\x00\x00\x00",
             vec![std::f64::MAX, -std::f64::MIN_POSITIVE, 2.0]),
        ];
        let (unitlen, parser) = get_type_info::<BigEndian>(12);
        for &(data, ref ans) in sets {
            assert!((data.len() - 1) % unitlen == 0);
            match parser(data, 1, (data.len() - 1) / unitlen) {
                Value::Double(v) => assert_eq!(v, *ans),
                v => panic!("wrong variant {:?}", v),
            }
        }
    }

    // These functions are never called in a way that an out-of-range access
    // could happen, so this test is hypothetical but just for safety.
    #[test]
    #[should_panic(expected = "index 5 out of range for slice of length 4")]
    fn short_oor() {
        parse_short::<BigEndian>(b"\x01\x02\x03\x04", 1, 2);
    }

    #[test]
    fn unknown() {
        let (unitlen, _parser) = get_type_info::<BigEndian>(0xffff);
        assert_eq!(unitlen, 0);
    }

    #[test]
    fn get_uint() {
        let v = Value::Byte(vec![1, 2]);
        assert_eq!(v.get_uint(0), Some(1));
        assert_eq!(v.get_uint(1), Some(2));
        assert_eq!(v.get_uint(2), None);
        let v = Value::Short(vec![1, 2]);
        assert_eq!(v.get_uint(0), Some(1));
        assert_eq!(v.get_uint(1), Some(2));
        assert_eq!(v.get_uint(2), None);
        let v = Value::Long(vec![1, 2]);
        assert_eq!(v.get_uint(0), Some(1));
        assert_eq!(v.get_uint(1), Some(2));
        assert_eq!(v.get_uint(2), None);
        let v = Value::SLong(vec![1, 2]);
        assert_eq!(v.get_uint(0), None);
        assert_eq!(v.get_uint(1), None);
        assert_eq!(v.get_uint(2), None);
    }

    #[test]
    fn iter_uint() {
        let vlist = &[
            Value::Byte(vec![1, 2]),
            Value::Short(vec![1, 2]),
            Value::Long(vec![1, 2]),
        ];
        for v in vlist {
            let mut it = v.iter_uint().unwrap();
            assert_eq!(it.next(), Some(1));
            assert_eq!(it.next(), Some(2));
            assert_eq!(it.next(), None);
        }

        let v = Value::SLong(vec![1, 2]);
        assert!(v.iter_uint().is_none());
    }

    #[test]
    fn iter_uint_is_exact_size_iter() {
        let v = Value::Byte(vec![1, 2, 3]);
        let mut it = v.iter_uint().unwrap();
        assert_eq!(it.len(), 3);
        assert_eq!(it.next(), Some(1));
        assert_eq!(it.len(), 2);
    }

    #[test]
    fn value_fmt_debug() {
        let v = Value::Byte(b"b\0y".to_vec());
        assert_eq!(format!("{:?}", v), "Byte([98, 0, 121])");
        let v = Value::Ascii(vec![]);
        assert_eq!(format!("{:?}", v), "Ascii([])");
        let v = Value::Ascii(vec![b"abc\"\\\n\x7f".to_vec(), b"".to_vec()]);
        assert_eq!(format!("{:?}", v), r#"Ascii(["abc\"\\\x0a\x7f", ""])"#);
        let v = Value::Short(vec![]);
        assert_eq!(format!("{:?}", v), "Short([])");
        let v = Value::Long(vec![1, 2]);
        assert_eq!(format!("{:?}", v), "Long([1, 2])");
        let v = Value::Rational(vec![(0, 0).into()]);
        assert_eq!(format!("{:?}", v), "Rational([Rational(0/0)])");
        let v = Value::SByte(vec![-3, 4, 5]);
        assert_eq!(format!("{:?}", v), "SByte([-3, 4, 5])");
        let v = Value::Undefined(vec![0, 0xff], 0);
        assert_eq!(format!("{:?}", v), "Undefined(0x00ff, ofs=0x0)");
        let v = Value::SShort(vec![6, -7]);
        assert_eq!(format!("{:?}", v), "SShort([6, -7])");
        let v = Value::SLong(vec![-9]);
        assert_eq!(format!("{:?}", v), "SLong([-9])");
        let v = Value::SRational(vec![(-2, -1).into()]);
        assert_eq!(format!("{:?}", v), "SRational([SRational(-2/-1)])");
        let v = Value::Float(vec![1.5, 0.0]);
        assert_eq!(format!("{:?}", v), "Float([1.5, 0.0])");
        let v = Value::Double(vec![-0.5, 1.0]);
        assert_eq!(format!("{:?}", v), "Double([-0.5, 1.0])");
        let v = Value::Unknown(1, 2, 10);
        assert_eq!(format!("{:?}", v), "Unknown(typ=1, cnt=2, ofs=0xa)");
    }

    #[test]
    fn rational_fmt_display() {
        let r = Rational::from((u32::max_value(), u32::max_value()));
        assert_eq!(format!("{}", r), "4294967295/4294967295");

        let r = Rational::from((10, 20));
        assert_eq!(format!("{}", r),         "10/20");
        assert_eq!(format!("{:11}", r),      "      10/20");
        assert_eq!(format!("{:3}", r),       "10/20");
    }

    #[test]
    fn srational_fmt_display() {
        let r = SRational::from((i32::min_value(), i32::min_value()));
        assert_eq!(format!("{}", r), "-2147483648/-2147483648");
        let r = SRational::from((i32::max_value(), i32::max_value()));
        assert_eq!(format!("{}", r), "2147483647/2147483647");

        let r = SRational::from((-10, 20));
        assert_eq!(format!("{}", r),         "-10/20");
        assert_eq!(format!("{:11}", r),      "     -10/20");
        assert_eq!(format!("{:3}", r),       "-10/20");

        let r = SRational::from((10, -20));
        assert_eq!(format!("{}", r),         "10/-20");
        assert_eq!(format!("{:11}", r),      "     10/-20");
        assert_eq!(format!("{:3}", r),       "10/-20");

        let r = SRational::from((-10, -20));
        assert_eq!(format!("{}", r),         "-10/-20");
        assert_eq!(format!("{:11}", r),      "    -10/-20");
        assert_eq!(format!("{:3}", r),       "-10/-20");
    }

    #[test]
    fn ratioanl_f64() {
        use std::{f64, u32};
        assert_eq!(f64::from(Rational::from((1, 2))), 0.5);
        assert_eq!(f64::from(Rational::from((1, u32::MAX))),
                   2.3283064370807974e-10);
        assert_eq!(f64::from(Rational::from((u32::MAX, 1))),
                   u32::MAX as f64);
        assert_eq!(f64::from(Rational::from((u32::MAX - 1, u32::MAX))),
                   0.9999999997671694);
        assert_eq!(f64::from(Rational::from((u32::MAX, u32::MAX - 1))),
                   1.0000000002328306);
        assert_eq!(f64::from(Rational::from((1, 0))), f64::INFINITY);
        assert!(f64::from(Rational::from((0, 0))).is_nan());

        assert_eq!(f64::from(SRational::from((1, 2))), 0.5);
        assert_eq!(f64::from(SRational::from((-1, 2))), -0.5);
        assert_eq!(f64::from(SRational::from((1, -2))), -0.5);
        assert_eq!(f64::from(SRational::from((-1, -2))), 0.5);
        assert_eq!(f64::from(SRational::from((1, 0))), f64::INFINITY);
        assert_eq!(f64::from(SRational::from((-1, 0))), f64::NEG_INFINITY);
    }

    #[test]
    fn rational_f32() {
        // If num and demon are converted to f32 before the division,
        // the precision is lost in this example.
        assert_eq!(f32::from(Rational::from((1, 16777217))), 5.960464e-8);
    }
}
