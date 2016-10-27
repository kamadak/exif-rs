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

use endian::Endian;

/// Types and values of TIFF fields (for Exif attributes).
#[derive(Debug)]
pub enum Value<'a> {
    /// Slice of 8-bit bytes containing 7-bit ASCII characters.
    /// The trailing null character is not included.  Note that
    /// the absence of the 8th bits is not guaranteed.
    Ascii(Vec<&'a [u8]>),
    /// Vector of 16-bit unsigned integers.
    Short(Vec<u16>),
    /// The type is unknown to this implementation.
    /// The associated values are the type and the count, and the
    /// offset of the "Value Offset" element.
    Unknown(u16, u32, u32),
}

type Parser<'a> = fn(&'a [u8], usize, usize) -> Value<'a>;

// Return the length of a single value and the parser of the type.
pub fn get_type_info<'a, E>(typecode: u16)
                            -> (u32, Parser<'a>) where E: Endian {
    match typecode {
        2 => (1, parse_ascii),
        3 => (2, parse_short::<E>),
        _ => (0, parse_unknown),
    }
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
    let mut val = Vec::new();
    for i in 0..count {
        val.push(E::loadu16(data, offset + i * 2));
    }
    Value::Short(val)
}

// This is a dummy function and will never be called.
#[allow(unused_variables)]
fn parse_unknown<'a>(data: &'a [u8], offset: usize, count: usize)
                     -> Value<'a> {
    unreachable!()
}
