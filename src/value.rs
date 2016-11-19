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
use std::mem;

use endian::Endian;

/// Types and values of TIFF fields (for Exif attributes).
#[derive(Debug)]
pub enum Value<'a> {
    /// Vector of 8-bit unsigned integers.
    Byte(Vec<u8>),
    /// Slice of 8-bit bytes containing 7-bit ASCII characters.
    /// The trailing null character is not included.  Note that
    /// the absence of the 8th bits is not guaranteed.
    Ascii(Vec<&'a [u8]>),
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
    Undefined(&'a [u8]),
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
    /// The type is unknown to this implementation.
    /// The associated values are the type and the count, and the
    /// offset of the "Value Offset" element.
    Unknown(u16, u32, u32),
}

/// An unsigned rational number, which is a pair of 32-bit unsigned integers.
pub struct Rational { pub num: u32, pub denom: u32 }

impl fmt::Debug for Rational {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Rational({}/{})", self.num, self.denom)
    }
}

/// A signed rational number, which is a pair of 32-bit signed integers.
pub struct SRational { pub num: i32, pub denom: i32 }

impl fmt::Debug for SRational {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "SRational({}/{})", self.num, self.denom)
    }
}

type Parser<'a> = fn(&'a [u8], usize, usize) -> Value<'a>;

// Return the length of a single value and the parser of the type.
pub fn get_type_info<'a, E>(typecode: u16)
                            -> (usize, Parser<'a>) where E: Endian {
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
        _ => (0, parse_unknown),
    }
}

fn parse_byte<'a>(data: &'a [u8], offset: usize, count: usize)
                  -> Value<'a> {
    Value::Byte(data[offset .. offset + count].to_vec())
}

fn parse_ascii<'a>(data: &'a [u8], offset: usize, count: usize)
                   -> Value<'a> {
    // Any ASCII field can contain multiple strings [TIFF6 Image File
    // Directory].
    let iter = (&data[offset .. offset + count]).split(|&b| b == b'\0');
    let mut v: Vec<&[u8]> = iter.collect();
    if v.len() >= 2 && v.last().map_or(false, |&s| s.len() == 0) {
        v.pop();
    }
    Value::Ascii(v)
}

fn parse_short<'a, E>(data: &'a [u8], offset: usize, count: usize)
                      -> Value<'a> where E: Endian {
    let mut val = Vec::with_capacity(count);
    for i in 0..count {
        val.push(E::loadu16(data, offset + i * 2));
    }
    Value::Short(val)
}

fn parse_long<'a, E>(data: &'a [u8], offset: usize, count: usize)
                     -> Value<'a> where E: Endian {
    let mut val = Vec::with_capacity(count);
    for i in 0..count {
        val.push(E::loadu32(data, offset + i * 4));
    }
    Value::Long(val)
}

fn parse_rational<'a, E>(data: &'a [u8], offset: usize, count: usize)
                         -> Value<'a> where E: Endian {
    let mut val = Vec::with_capacity(count);
    for i in 0..count {
        val.push(Rational {
            num: E::loadu32(data, offset + i * 8),
            denom: E::loadu32(data, offset + i * 8 + 4),
        });
    }
    Value::Rational(val)
}

fn parse_sbyte<'a>(data: &'a [u8], offset: usize, count: usize)
                   -> Value<'a> {
    let uslice = &data[offset .. offset + count];
    let islice = unsafe { ::std::slice::from_raw_parts(
        uslice.as_ptr() as *const i8, count) };
    Value::SByte(islice.to_vec())
}

fn parse_undefined<'a>(data: &'a [u8], offset: usize, count: usize)
                       -> Value<'a> {
    Value::Undefined(&data[offset .. offset + count])
}

fn parse_sshort<'a, E>(data: &'a [u8], offset: usize, count: usize)
                       -> Value<'a> where E: Endian {
    let mut val = Vec::with_capacity(count);
    for i in 0..count {
        val.push(E::loadu16(data, offset + i * 2) as i16);
    }
    Value::SShort(val)
}

fn parse_slong<'a, E>(data: &'a [u8], offset: usize, count: usize)
                      -> Value<'a> where E: Endian {
    let mut val = Vec::with_capacity(count);
    for i in 0..count {
        val.push(E::loadu32(data, offset + i * 4) as i32);
    }
    Value::SLong(val)
}

fn parse_srational<'a, E>(data: &'a [u8], offset: usize, count: usize)
                          -> Value<'a> where E: Endian {
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
fn parse_float<'a, E>(data: &'a [u8], offset: usize, count: usize)
                      -> Value<'a> where E: Endian {
    let mut val = Vec::with_capacity(count);
    for i in 0..count {
        val.push(unsafe { mem::transmute(E::loadu32(data, offset + i * 4)) });
    }
    Value::Float(val)
}

// This is a dummy function and will never be called.
#[allow(unused_variables)]
fn parse_unknown<'a>(data: &'a [u8], offset: usize, count: usize)
                     -> Value<'a> {
    unreachable!()
}

#[cfg(test)]
mod tests {
    use endian::BigEndian;
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
            (b"x", vec![b""]),				// malformed
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
             vec![Rational { num: 0xa1020304, denom: 0x05060708 },
                  Rational { num: 0x090a0b0c, denom: 0xbd0e0f10 }]),
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
                Value::Undefined(v) => assert_eq!(v, ans),
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
             vec![SRational { num: -0x5efdfcfc, denom: 0x05060708 },
                  SRational { num: 0x090a0b0c, denom: -0x42f1f0f0 }]),
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
             vec![::std::f32::MAX, -::std::f32::MIN_POSITIVE, 2.0]),
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
}
