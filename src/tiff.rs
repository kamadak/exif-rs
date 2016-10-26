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

use endian::{Endian, BigEndian, LittleEndian};
use error::Error;
use tag::Tag;
use value::Value;
use value::get_type_info;

// Tiff header magic numbers [EXIF23 4.5.2].
const TIFF_BE: u16 = 0x4d4d;
const TIFF_LE: u16 = 0x4949;
const TIFF_FORTY_TWO: u16 = 0x002a;

/// A TIFF field.
#[derive(Debug)]
pub struct Field<'a> {
    pub tag: Tag,
    pub value: Value<'a>,
}

/// Parse the Exif attributes in the TIFF format.
pub fn parse_exif(data: &[u8]) -> Result<Vec<Field>, Error> {
    // Check the byte order and call the real parser.
    if data.len() < 8 {
        return Err(Error::InvalidFormat("Truncated TIFF header"));
    }
    match BigEndian::loadu16(data, 0) {
        TIFF_BE => parse_exif_sub::<BigEndian>(data),
        TIFF_LE => parse_exif_sub::<LittleEndian>(data),
        _ => Err(Error::InvalidFormat("Invalid TIFF byte order")),
    }
}

fn parse_exif_sub<E>(data: &[u8])
                     -> Result<Vec<Field>, Error> where E: Endian {
    // Parse the rest of the header (42 and the IFD offset).
    if E::loadu16(data, 2) != TIFF_FORTY_TWO {
        return Err(Error::InvalidFormat("Invalid forty two"));
    }
    let ifd_offset = E::loadu32(data, 4) as usize;
    parse_ifd::<E>(data, ifd_offset)
}

// Parse IFD [EXIF23 4.6.2].
fn parse_ifd<E>(data: &[u8], offset: usize)
                -> Result<Vec<Field>, Error> where E: Endian {
    // Count (the number of the entries).
    if data.len() < offset || data.len() - offset < 2 {
        return Err(Error::InvalidFormat("Truncated IFD"));
    }
    let count = E::loadu16(data, offset) as usize;

    // Array of entries.  (count * 12) never overflow.
    if data.len() - offset - 2 < count * 12 {
        return Err(Error::InvalidFormat("Truncated IFD"));
    }
    let mut fields = Vec::with_capacity(count);
    for i in 0..count as usize {
        let tag = E::loadu16(data, offset + 2 + i * 12);
        let typ = E::loadu16(data, offset + 2 + i * 12 + 2);
        let cnt = E::loadu32(data, offset + 2 + i * 12 + 4);
        let ofs = E::loadu32(data, offset + 2 + i * 12 + 8) as usize;
        let (unitlen, parser) = get_type_info::<E>(typ);
        let vallen = try!(unitlen.checked_mul(cnt).ok_or(
            Error::InvalidFormat("Invalid entry count"))) as usize;
        let val;
        if unitlen == 0 {
            val = Value::Unknown(typ, cnt, ofs as u32);
        } else if vallen <= 4 {
            val = parser(data, offset + 2 + i * 12 + 8, cnt as usize);
        } else {
            if data.len() < ofs || data.len() - ofs < vallen {
                return Err(Error::InvalidFormat("Truncated IFD"));
            }
            val = parser(data, ofs, cnt as usize);
        }
        fields.push(Field { tag: Tag(tag), value: val });
    }

    // Offset to the next IFD.
    if data.len() - offset - 2 - count * 12 < 4 {
        return Err(Error::InvalidFormat("Truncated IFD"));
    }
    let next_ifd_offset = E::loadu32(data, offset + 2 + count * 12);
    if next_ifd_offset != 0 {
        unimplemented!();
    }

    Ok(fields)
}
