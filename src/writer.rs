//
// Copyright (c) 2017 KAMADA Ken'ichi.
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

use std::io;
use std::io::{Seek, SeekFrom, Write};

use crate::endian::{Endian, BigEndian, LittleEndian};
use crate::error::Error;
use crate::tag::{Context, Tag};
use crate::tiff::{Field, In, TIFF_BE_SIG, TIFF_LE_SIG};
use crate::value::Value;

/// The `Writer` struct is used to encode and write Exif data.
///
/// # Examples
///
/// ```
/// # fn main() -> Result<(), Box<dyn std::error::Error>> {
/// use exif::{Field, In, Tag, Value};
/// use exif::experimental::Writer;
/// let image_desc = Field {
///     tag: Tag::ImageDescription,
///     ifd_num: In::PRIMARY,
///     value: Value::Ascii(vec![b"Sample".to_vec()]),
/// };
/// let mut writer = Writer::new();
/// let mut buf = std::io::Cursor::new(Vec::new());
/// writer.push_field(&image_desc);
/// writer.write(&mut buf, false)?;
/// static expected: &[u8] =
///     b"\x4d\x4d\x00\x2a\x00\x00\x00\x08\
///       \x00\x01\x01\x0e\x00\x02\x00\x00\x00\x07\x00\x00\x00\x1a\
///       \x00\x00\x00\x00\
///       Sample\0";
/// assert_eq!(buf.into_inner(), expected);
/// # Ok(()) }
/// ```
#[derive(Debug)]
pub struct Writer<'a> {
    ifd_list: Vec<Ifd<'a>>,
}

#[derive(Debug, Default)]
struct Ifd<'a> {
    tiff_fields: Vec<&'a Field>,
    exif_fields: Vec<&'a Field>,
    gps_fields: Vec<&'a Field>,
    interop_fields: Vec<&'a Field>,
    strips: Option<&'a [&'a [u8]]>,
    tiles: Option<&'a [&'a [u8]]>,
    jpeg: Option<&'a [u8]>,
}

impl<'a> Ifd<'a> {
    fn is_empty(&self) -> bool {
        self.tiff_fields.is_empty() &&
            self.exif_fields.is_empty() &&
            self.gps_fields.is_empty() &&
            self.interop_fields.is_empty() &&
            self.strips.is_none() &&
            self.tiles.is_none() &&
            self.jpeg.is_none()
    }
}

struct WriterState<'a> {
    tiff_fields: Vec<&'a Field>,
    exif_fields: Vec<&'a Field>,
    gps_fields: Vec<&'a Field>,
    interop_fields: Vec<&'a Field>,
    tiff_ifd_offset: u32,
    exif_ifd_offset: u32,
    gps_ifd_offset: u32,
    interop_ifd_offset: u32,
}

impl<'a> Writer<'a> {
    /// Constructs an empty `Writer`.
    pub fn new() -> Writer<'a> {
        Writer {
            ifd_list: Vec::new(),
        }
    }

    /// Appends a field to be written.
    ///
    /// The fields can be appended in any order.
    /// Duplicate fields must not be appended.
    ///
    /// The following fields are ignored and synthesized when needed:
    /// ExifIFDPointer, GPSInfoIFDPointer, InteropIFDPointer,
    /// StripOffsets, StripByteCounts, TileOffsets, TileByteCounts,
    /// JPEGInterchangeFormat, and JPEGInterchangeFormatLength.
    pub fn push_field(&mut self, field: &'a Field) {
        match *field {
            // Ignore the tags for the internal data structure.
            Field { tag: Tag::ExifIFDPointer, .. } |
            Field { tag: Tag::GPSInfoIFDPointer, .. } |
            Field { tag: Tag::InteropIFDPointer, .. } => {},
            // These tags are synthesized from the actual strip/tile data.
            Field { tag: Tag::StripOffsets, .. } |
            Field { tag: Tag::StripByteCounts, .. } |
            Field { tag: Tag::TileOffsets, .. } |
            Field { tag: Tag::TileByteCounts, .. } => {},
            // These tags are synthesized from the actual JPEG thumbnail.
            Field { tag: Tag::JPEGInterchangeFormat, .. } |
            Field { tag: Tag::JPEGInterchangeFormatLength, .. } => {},
            // Other normal tags.
            Field { tag: Tag(ctx, _), ifd_num, .. } => {
                let ifd = self.pick_ifd(ifd_num);
                match ctx {
                    Context::Tiff => ifd.tiff_fields.push(field),
                    Context::Exif => ifd.exif_fields.push(field),
                    Context::Gps => ifd.gps_fields.push(field),
                    Context::Interop => ifd.interop_fields.push(field),
                }
            },
        }
    }

    /// Sets TIFF strips for the specified IFD.
    /// If this method is called multiple times, the last one is used.
    pub fn set_strips(&mut self, strips: &'a [&'a [u8]], ifd_num: In) {
        self.pick_ifd(ifd_num).strips = Some(strips);
    }

    /// Sets TIFF tiles for the specified IFD.
    /// If this method is called multiple times, the last one is used.
    pub fn set_tiles(&mut self, tiles: &'a [&'a [u8]], ifd_num: In) {
        self.pick_ifd(ifd_num).tiles = Some(tiles);
    }

    /// Sets JPEG data for the specified IFD.
    /// If this method is called multiple times, the last one is used.
    pub fn set_jpeg(&mut self, jpeg: &'a [u8], ifd_num: In) {
        self.pick_ifd(ifd_num).jpeg = Some(jpeg);
    }

    /// Encodes Exif data and writes it into `w`.
    ///
    /// The write position of `w` must be set to zero before calling
    /// this method.
    pub fn write<W>(&mut self, w: &mut W, little_endian: bool)
                    -> Result<(), Error> where W: Write + Seek {
        // TIFF signature and the offset of the 0th IFD.
        if little_endian {
            w.write_all(&TIFF_LE_SIG)?;
            LittleEndian::writeu32(w, 8)?;
        } else {
            w.write_all(&TIFF_BE_SIG)?;
            BigEndian::writeu32(w, 8)?;
        }

        // There must be at least 1 IFD in a TIFF file [TIFF6, Section 2,
        // Image File Directory].
        if self.ifd_list.is_empty() {
            return Err(Error::InvalidFormat("At least one IFD must exist"));
        }
        let mut ifd_num_ck = Some(0);
        let mut next_ifd_offset_offset = 4;
        for ifd in &self.ifd_list {
            // Each IFD must have at least one entry [TIFF6, Section 2,
            // Image File Directory].
            if ifd.is_empty() {
                return Err(Error::InvalidFormat("IFD must not be empty"));
            }
            let ifd_num =
                ifd_num_ck.ok_or(Error::InvalidFormat("Too many IFDs"))?;
            if ifd_num > 0 {
                let next_ifd_offset = pad_and_get_offset(w)?;
                let origpos = w.seek(SeekFrom::Current(0))?;
                w.seek(SeekFrom::Start(next_ifd_offset_offset as u64))?;
                match little_endian {
                    false => BigEndian::writeu32(w, next_ifd_offset)?,
                    true => LittleEndian::writeu32(w, next_ifd_offset)?,
                }
                w.seek(SeekFrom::Start(origpos))?;
            }
            next_ifd_offset_offset =
                synthesize_fields(w, ifd, In(ifd_num), little_endian)?;
            ifd_num_ck = ifd_num.checked_add(1);
        }
        w.flush()?;
        Ok(())
    }

    fn pick_ifd(&mut self, ifd_num: In) -> &mut Ifd<'a> {
        let ifd_num = ifd_num.index() as usize;
        if self.ifd_list.len() <= ifd_num {
            self.ifd_list.resize_with(ifd_num + 1, Default::default);
        }
        &mut self.ifd_list[ifd_num]
    }
}

// Synthesizes special fields, writes an image, and returns the offset
// of the next IFD offset.
fn synthesize_fields<W>(w: &mut W, ifd: &Ifd, ifd_num: In,
                        little_endian: bool)
                        -> Result<u32, Error> where W: Write + Seek {
    let exif_in_tiff;
    let gps_in_tiff;
    let interop_in_exif;
    let strip_offsets;
    let strip_byte_counts;
    let tile_offsets;
    let tile_byte_counts;
    let jpeg_offset;
    let jpeg_length;
    let mut ws = WriterState {
        tiff_fields: ifd.tiff_fields.clone(),
        exif_fields: ifd.exif_fields.clone(),
        gps_fields: ifd.gps_fields.clone(),
        interop_fields: ifd.interop_fields.clone(),
        tiff_ifd_offset: 0,
        exif_ifd_offset: 0,
        gps_ifd_offset: 0,
        interop_ifd_offset: 0,
    };

    if let Some(strips) = ifd.strips {
        strip_offsets = Field {
            tag: Tag::StripOffsets,
            ifd_num: ifd_num,
            value: Value::Long(vec![0; strips.len()]),
        };
        ws.tiff_fields.push(&strip_offsets);
        strip_byte_counts = Field {
            tag: Tag::StripByteCounts,
            ifd_num: ifd_num,
            value: Value::Long(
                strips.iter().map(|s| s.len() as u32).collect()),
        };
        ws.tiff_fields.push(&strip_byte_counts);
    }
    if let Some(tiles) = ifd.tiles {
        tile_offsets = Field {
            tag: Tag::TileOffsets,
            ifd_num: ifd_num,
            value: Value::Long(vec![0; tiles.len()]),
        };
        ws.tiff_fields.push(&tile_offsets);
        tile_byte_counts = Field {
            tag: Tag::TileByteCounts,
            ifd_num: ifd_num,
            value: Value::Long(
                tiles.iter().map(|s| s.len() as u32).collect()),
        };
        ws.tiff_fields.push(&tile_byte_counts);
    }
    if let Some(jpeg) = ifd.jpeg {
        jpeg_offset = Field {
            tag: Tag::JPEGInterchangeFormat,
            ifd_num: ifd_num,
            value: Value::Long(vec![0]),
        };
        ws.tiff_fields.push(&jpeg_offset);
        jpeg_length = Field {
            tag: Tag::JPEGInterchangeFormatLength,
            ifd_num: ifd_num,
            value: Value::Long(vec![jpeg.len() as u32]),
        };
        ws.tiff_fields.push(&jpeg_length);
    }

    let interop_fields_len = ws.interop_fields.len();
    let gps_fields_len = ws.gps_fields.len();
    let exif_fields_len = ws.exif_fields.len() +
        match interop_fields_len { 0 => 0, _ => 1 };
    let tiff_fields_len = ws.tiff_fields.len() +
        match gps_fields_len { 0 => 0, _ => 1 } +
        match exif_fields_len { 0 => 0, _ => 1 };
    assert_ne!(tiff_fields_len, 0);

    ws.tiff_ifd_offset = reserve_ifd(w, tiff_fields_len)?;
    if exif_fields_len > 0 {
        ws.exif_ifd_offset = reserve_ifd(w, exif_fields_len)?;
        exif_in_tiff = Field {
            tag: Tag::ExifIFDPointer,
            ifd_num: ifd_num,
            value: Value::Long(vec![ws.exif_ifd_offset]),
        };
        ws.tiff_fields.push(&exif_in_tiff);
    }
    if gps_fields_len > 0 {
        ws.gps_ifd_offset = reserve_ifd(w, gps_fields_len)?;
        gps_in_tiff = Field {
            tag: Tag::GPSInfoIFDPointer,
            ifd_num: ifd_num,
            value: Value::Long(vec![ws.gps_ifd_offset]),
        };
        ws.tiff_fields.push(&gps_in_tiff);
    }
    if interop_fields_len > 0 {
        ws.interop_ifd_offset = reserve_ifd(w, interop_fields_len)?;
        interop_in_exif = Field {
            tag: Tag::InteropIFDPointer,
            ifd_num: ifd_num,
            value: Value::Long(vec![ws.interop_ifd_offset]),
        };
        ws.exif_fields.push(&interop_in_exif);
    }

    ws.tiff_fields.sort_by_key(|f| f.tag.number());
    ws.exif_fields.sort_by_key(|f| f.tag.number());
    ws.gps_fields.sort_by_key(|f| f.tag.number());
    ws.interop_fields.sort_by_key(|f| f.tag.number());

    match little_endian {
        false => write_image::<_, BigEndian>(w, &ws, ifd),
        true => write_image::<_, LittleEndian>(w, &ws, ifd),
    }
}

// Writes an image and returns the offset of the next IFD offset.
fn write_image<W, E>(w: &mut W, ws: &WriterState, ifd: &Ifd)
                     -> Result<u32, Error> where W: Write + Seek, E: Endian {
    let (next_ifd_offset_offset,
         strip_offsets_offset, tile_offsets_offset, jpeg_offset) =
        write_ifd_and_fields::<_, E>(
            w, &ws.tiff_fields, ws.tiff_ifd_offset)?;
    if ws.exif_fields.len() > 0 {
        write_ifd_and_fields::<_, E>(
            w, &ws.exif_fields, ws.exif_ifd_offset)?;
    }
    if ws.gps_fields.len() > 0 {
        write_ifd_and_fields::<_, E>(
            w, &ws.gps_fields, ws.gps_ifd_offset)?;
    }
    if ws.interop_fields.len() > 0 {
        write_ifd_and_fields::<_, E>(
            w, &ws.interop_fields, ws.interop_ifd_offset)?;
    }

    if let Some(strips) = ifd.strips {
        let mut strip_offsets = Vec::new();
        for strip in strips {
            strip_offsets.push(get_offset(w)?);
            w.write_all(strip)?;
        }
        let origpos = w.seek(SeekFrom::Current(0))?;
        w.seek(SeekFrom::Start(strip_offsets_offset as u64))?;
        for ofs in strip_offsets {
            E::writeu32(w, ofs)?;
        }
        w.seek(SeekFrom::Start(origpos))?;
    }
    if let Some(tiles) = ifd.tiles {
        let mut tile_offsets = Vec::new();
        for tile in tiles {
            tile_offsets.push(get_offset(w)?);
            w.write_all(tile)?;
        }
        let origpos = w.seek(SeekFrom::Current(0))?;
        w.seek(SeekFrom::Start(tile_offsets_offset as u64))?;
        for ofs in tile_offsets {
            E::writeu32(w, ofs)?;
        }
        w.seek(SeekFrom::Start(origpos))?;
    }
    if let Some(jpeg) = ifd.jpeg {
        let offset = get_offset(w)?;
        w.write_all(jpeg)?;
        let origpos = w.seek(SeekFrom::Current(0))?;
        w.seek(SeekFrom::Start(jpeg_offset as u64))?;
        E::writeu32(w, offset)?;
        w.seek(SeekFrom::Start(origpos))?;
    }

    Ok(next_ifd_offset_offset)
}

// Advances the write position to make a space for a new IFD and
// returns the offset of the IFD.
fn reserve_ifd<W>(w: &mut W, count: usize)
                  -> Result<u32, Error> where W: Write + Seek {
    let ifdpos = get_offset(w)?;
    assert!(ifdpos % 2 == 0);
    // The number of entries (2) + array of entries (12 * n) +
    // the next IFD pointer (4).
    w.seek(SeekFrom::Current(2 + count as i64 * 12 + 4))?;
    Ok(ifdpos)
}

// Writes an IFD and its fields, and
// returns the offsets of the next IFD offset, StripOffsets value,
// TileOffsets value, and JPEGInterchangeFormat value.
fn write_ifd_and_fields<W, E>(
    w: &mut W, fields: &Vec<&Field>, ifd_offset: u32)
    -> Result<(u32, u32, u32, u32), Error> where W: Write + Seek, E: Endian
{
    let mut strip_offsets_offset = 0;
    let mut tile_offsets_offset = 0;
    let mut jpeg_offset = 0;
    let mut ifd = Vec::new();

    // Write the number of entries.
    E::writeu16(&mut ifd, fields.len() as u16)?;
    // Write the fields.
    for f in fields {
        let (typ, cnt, mut valbuf) = compose_value::<E>(&f.value)?;
        if cnt as u32 as usize != cnt {
            return Err(Error::TooBig("Too long array"));
        }
        E::writeu16(&mut ifd, f.tag.number())?;
        E::writeu16(&mut ifd, typ)?;
        E::writeu32(&mut ifd, cnt as u32)?;
        // Embed the value itself into the offset, or
        // encode as an offset and the value.
        if valbuf.len() <= 4 {
            valbuf.resize(4, 0);
            ifd.write_all(&valbuf)?;
        } else {
            // The value must begin on a word boundary. [TIFF6, Section 2:
            // TIFF Structure, Image File Directory, IFD Entry, p. 15]
            let valofs = pad_and_get_offset(w)?;
            E::writeu32(&mut ifd, valofs)?;
            w.write_all(&valbuf)?;
        }
        if f.tag == Tag::StripOffsets {
            strip_offsets_offset = match valbuf.len() {
                0..=4 => ifd_offset + ifd.len() as u32 - 4,
                _ => get_offset(w)? - valbuf.len() as u32,
            };
        }
        if f.tag == Tag::TileOffsets {
            tile_offsets_offset = match valbuf.len() {
                0..=4 => ifd_offset + ifd.len() as u32 - 4,
                _ => get_offset(w)? - valbuf.len() as u32,
            };
        }
        if f.tag == Tag::JPEGInterchangeFormat {
            jpeg_offset = ifd_offset + ifd.len() as u32 - 4;
        }
    }
    // Write the next IFD pointer.
    let next_ifd_offset_offset = ifd_offset + ifd.len() as u32;
    E::writeu32(&mut ifd, 0)?;

    // Write the IFD.
    write_at(w, &ifd, ifd_offset)?;

    Ok((next_ifd_offset_offset,
        strip_offsets_offset, tile_offsets_offset, jpeg_offset))
}

// Returns the type, count, and encoded value.
fn compose_value<E>(value: &Value)
                    -> Result<(u16, usize, Vec<u8>), Error> where E: Endian {
    match *value {
        Value::Byte(ref vec) =>
            Ok((1, vec.len(), vec.clone())),
        Value::Ascii(ref vec) => {
            let mut buf = Vec::new();
            for x in vec {
                buf.extend_from_slice(x);
                buf.push(0);
            }
            Ok((2, buf.len(), buf))
        },
        Value::Short(ref vec) => {
            let mut buf = Vec::new();
            for &v in vec {
                E::writeu16(&mut buf, v)?;
            }
            Ok((3, vec.len(), buf))
        },
        Value::Long(ref vec) => {
            let mut buf = Vec::new();
            for &v in vec {
                E::writeu32(&mut buf, v)?;
            }
            Ok((4, vec.len(), buf))
        },
        Value::Rational(ref vec) => {
            let mut buf = Vec::new();
            for v in vec {
                E::writeu32(&mut buf, v.num)?;
                E::writeu32(&mut buf, v.denom)?;
            }
            Ok((5, vec.len(), buf))
        },
        Value::SByte(ref vec) => {
            let bytes = vec.iter().map(|x| *x as u8).collect();
            Ok((6, vec.len(), bytes))
        },
        Value::Undefined(ref s, _) =>
            Ok((7, s.len(), s.to_vec())),
        Value::SShort(ref vec) => {
            let mut buf = Vec::new();
            for &v in vec {
                E::writeu16(&mut buf, v as u16)?;
            }
            Ok((8, vec.len(), buf))
        },
        Value::SLong(ref vec) => {
            let mut buf = Vec::new();
            for &v in vec {
                E::writeu32(&mut buf, v as u32)?;
            }
            Ok((9, vec.len(), buf))
        },
        Value::SRational(ref vec) => {
            let mut buf = Vec::new();
            for v in vec {
                E::writeu32(&mut buf, v.num as u32)?;
                E::writeu32(&mut buf, v.denom as u32)?;
            }
            Ok((10, vec.len(), buf))
        },
        Value::Float(ref vec) => {
            let mut buf = Vec::new();
            for &v in vec {
                E::writeu32(&mut buf, v.to_bits())?;
            }
            Ok((11, vec.len(), buf))
        },
        Value::Double(ref vec) => {
            let mut buf = Vec::new();
            for &v in vec {
                E::writeu64(&mut buf, v.to_bits())?;
            }
            Ok((12, vec.len(), buf))
        },
        Value::Unknown(_, _, _) =>
            Err(Error::NotSupported("Cannot write unknown field types")),
    }
}

fn write_at<W>(w: &mut W, buf: &[u8], offset: u32)
               -> io::Result<()> where W: Write + Seek {
    let orig = w.seek(SeekFrom::Current(0))?;
    w.seek(SeekFrom::Start(offset as u64))?;
    w.write_all(buf)?;
    w.seek(SeekFrom::Start(orig))?;
    Ok(())
}

// Aligns `w` to the two-byte (word) boundary and returns the new offset.
fn pad_and_get_offset<W>(w: &mut W)
                         -> Result<u32, Error> where W: Write + Seek {
    let mut pos = w.seek(SeekFrom::Current(0))?;
    if pos >= (1 << 32) - 1 {
        return Err(Error::TooBig("Offset too large"));
    }
    if pos % 2 != 0 {
        w.write_all(&[0])?;
        pos += 1;
    }
    Ok(pos as u32)
}

fn get_offset<W>(w: &mut W)
                 -> Result<u32, Error> where W: Write + Seek {
    let pos = w.seek(SeekFrom::Current(0))?;
    if pos as u32 as u64 != pos {
        return Err(Error::TooBig("Offset too large"));
    }
    Ok(pos as u32)
}

#[cfg(test)]
mod tests {
    use std::io::Cursor;
    use super::*;

    #[test]
    fn primary() {
        let image_desc = Field {
            tag: Tag::ImageDescription,
            ifd_num: In::PRIMARY,
            value: Value::Ascii(vec![b"Sample".to_vec()]),
        };
        let mut writer = Writer::new();
        let mut buf = Cursor::new(Vec::new());
        writer.push_field(&image_desc);
        writer.write(&mut buf, false).unwrap();
        let expected: &[u8] =
            b"\x4d\x4d\x00\x2a\x00\x00\x00\x08\
              \x00\x01\x01\x0e\x00\x02\x00\x00\x00\x07\x00\x00\x00\x1a\
              \x00\x00\x00\x00\
              Sample\0";
        assert_eq!(buf.into_inner(), expected);
    }

    #[test]
    fn primary_exif_only() {
        let exif_ver = Field {
            tag: Tag::ExifVersion,
            ifd_num: In::PRIMARY,
            value: Value::Undefined(b"0231".to_vec(), 0),
        };
        let mut writer = Writer::new();
        let mut buf = Cursor::new(Vec::new());
        writer.push_field(&exif_ver);
        writer.write(&mut buf, false).unwrap();
        let expected: &[u8] =
            b"\x4d\x4d\x00\x2a\x00\x00\x00\x08\
              \x00\x01\x87\x69\x00\x04\x00\x00\x00\x01\x00\x00\x00\x1a\
              \x00\x00\x00\x00\
              \x00\x01\x90\x00\x00\x07\x00\x00\x00\x040231\
              \x00\x00\x00\x00";
        assert_eq!(buf.into_inner(), expected);
    }

    #[test]
    fn primary_tiff_tiled() {
        // This is not a valid TIFF tile (only for testing).
        let tiles: &[&[u8]] = &[b"TILE"];
        let mut writer = Writer::new();
        let mut buf = Cursor::new(Vec::new());
        writer.set_tiles(tiles, In::PRIMARY);
        writer.write(&mut buf, false).unwrap();
        let expected: &[u8] =
            b"\x4d\x4d\x00\x2a\x00\x00\x00\x08\
              \x00\x02\x01\x44\x00\x04\x00\x00\x00\x01\x00\x00\x00\x26\
                      \x01\x45\x00\x04\x00\x00\x00\x01\x00\x00\x00\x04\
              \x00\x00\x00\x00\
              TILE";
        assert_eq!(buf.into_inner(), expected);
    }

    #[test]
    fn thumbnail_jpeg() {
        // This is not a valid JPEG data (only for testing).
        let jpeg = b"JPEG";
        let desc = Field {
            tag: Tag::ImageDescription,
            ifd_num: In::PRIMARY,
            value: Value::Ascii(vec![b"jpg".to_vec()]),
        };
        let mut writer = Writer::new();
        let mut buf = Cursor::new(Vec::new());
        writer.push_field(&desc);
        writer.set_jpeg(jpeg, In::THUMBNAIL);
        writer.write(&mut buf, false).unwrap();
        let expected: &[u8] =
            b"\x4d\x4d\x00\x2a\x00\x00\x00\x08\
              \x00\x01\x01\x0e\x00\x02\x00\x00\x00\x04jpg\x00\
              \x00\x00\x00\x1a\
              \x00\x02\x02\x01\x00\x04\x00\x00\x00\x01\x00\x00\x00\x38\
                      \x02\x02\x00\x04\x00\x00\x00\x01\x00\x00\x00\x04\
              \x00\x00\x00\x00\
              JPEG";
        assert_eq!(buf.into_inner(), expected);
    }

    #[test]
    fn thumbnail_tiff() {
        // This is not a valid TIFF strip (only for testing).
        let desc = Field {
            tag: Tag::ImageDescription,
            ifd_num: In::PRIMARY,
            value: Value::Ascii(vec![b"tif".to_vec()]),
        };
        let strips: &[&[u8]] = &[b"STRIP"];
        let mut writer = Writer::new();
        let mut buf = Cursor::new(Vec::new());
        writer.push_field(&desc);
        writer.set_strips(strips, In::THUMBNAIL);
        writer.write(&mut buf, false).unwrap();
        let expected: &[u8] =
            b"\x4d\x4d\x00\x2a\x00\x00\x00\x08\
              \x00\x01\x01\x0e\x00\x02\x00\x00\x00\x04tif\x00\
              \x00\x00\x00\x1a\
              \x00\x02\x01\x11\x00\x04\x00\x00\x00\x01\x00\x00\x00\x38\
                      \x01\x17\x00\x04\x00\x00\x00\x01\x00\x00\x00\x05\
              \x00\x00\x00\x00\
              STRIP";
        assert_eq!(buf.into_inner(), expected);
    }

    #[test]
    fn primary_and_thumbnail() {
        let image_desc = Field {
            tag: Tag::ImageDescription,
            ifd_num: In::PRIMARY,
            value: Value::Ascii(vec![b"Sample".to_vec()]),
        };
        let exif_ver = Field {
            tag: Tag::ExifVersion,
            ifd_num: In::PRIMARY,
            value: Value::Undefined(b"0231".to_vec(), 0),
        };
        let gps_ver = Field {
            tag: Tag::GPSVersionID,
            ifd_num: In::PRIMARY,
            value: Value::Byte(vec![2, 3, 0, 0]),
        };
        let interop_index = Field {
            tag: Tag::InteroperabilityIndex,
            ifd_num: In::PRIMARY,
            value: Value::Ascii(vec![b"ABC".to_vec()]),
        };
        let jpeg = b"JPEG";
        let mut writer = Writer::new();
        let mut buf = Cursor::new(Vec::new());
        writer.push_field(&image_desc);
        writer.push_field(&exif_ver);
        writer.push_field(&gps_ver);
        writer.push_field(&interop_index);
        writer.set_jpeg(jpeg, In::THUMBNAIL);
        writer.write(&mut buf, false).unwrap();
        let expected: &[u8] =
            b"\x4d\x4d\x00\x2a\x00\x00\x00\x08\
              \x00\x03\x01\x0e\x00\x02\x00\x00\x00\x07\x00\x00\x00\x74\
                      \x87\x69\x00\x04\x00\x00\x00\x01\x00\x00\x00\x32\
                      \x88\x25\x00\x04\x00\x00\x00\x01\x00\x00\x00\x50\
              \x00\x00\x00\x7c\
              \x00\x02\x90\x00\x00\x07\x00\x00\x00\x040231\
                      \xa0\x05\x00\x04\x00\x00\x00\x01\x00\x00\x00\x62\
              \x00\x00\x00\x00\
              \x00\x01\x00\x00\x00\x01\x00\x00\x00\x04\x02\x03\x00\x00\
              \x00\x00\x00\x00\
              \x00\x01\x00\x01\x00\x02\x00\x00\x00\x04ABC\0\
              \x00\x00\x00\x00\
              Sample\0\0\
              \x00\x02\x02\x01\x00\x04\x00\x00\x00\x01\x00\x00\x00\x9a\
                      \x02\x02\x00\x04\x00\x00\x00\x01\x00\x00\x00\x04\
              \x00\x00\x00\x00\
              JPEG";
        assert_eq!(buf.into_inner(), expected);
    }

    #[test]
    fn primary_thumbnail_and_2nd() {
        let desc0 = Field {
            tag: Tag::ImageDescription,
            ifd_num: In::PRIMARY,
            value: Value::Ascii(vec![b"p".to_vec()]),
        };
        let desc1 = Field {
            tag: Tag::ImageDescription,
            ifd_num: In::THUMBNAIL,
            value: Value::Ascii(vec![b"t".to_vec()]),
        };
        let desc2 = Field {
            tag: Tag::ImageDescription,
            ifd_num: In(2),
            value: Value::Ascii(vec![b"2".to_vec()]),
        };
        let mut writer = Writer::new();
        let mut buf = Cursor::new(Vec::new());
        writer.push_field(&desc0);
        writer.push_field(&desc1);
        writer.push_field(&desc2);
        writer.write(&mut buf, false).unwrap();
        let expected: &[u8] =
            b"\x4d\x4d\x00\x2a\x00\x00\x00\x08\
              \x00\x01\x01\x0e\x00\x02\x00\x00\x00\x02p\x00\x00\x00\
              \x00\x00\x00\x1a\
              \x00\x01\x01\x0e\x00\x02\x00\x00\x00\x02t\x00\x00\x00\
              \x00\x00\x00\x2c\
              \x00\x01\x01\x0e\x00\x02\x00\x00\x00\x022\x00\x00\x00\
              \x00\x00\x00\x00";
        assert_eq!(buf.into_inner(), expected);
    }

    #[test]
    fn empty_file() {
        let mut writer = Writer::new();
        let mut buf = Cursor::new(Vec::new());
        assert_pat!(writer.write(&mut buf, false),
                    Err(Error::InvalidFormat("At least one IFD must exist")));
    }

    #[test]
    fn missing_primary() {
        let jpeg = b"JPEG";
        let mut writer = Writer::new();
        let mut buf = Cursor::new(Vec::new());
        writer.set_jpeg(jpeg, In::THUMBNAIL);
        assert_pat!(writer.write(&mut buf, false),
                    Err(Error::InvalidFormat("IFD must not be empty")));
    }

    #[test]
    fn write_twice() {
        let image_desc = Field {
            tag: Tag::ImageDescription,
            ifd_num: In::PRIMARY,
            value: Value::Ascii(vec![b"Sample".to_vec()]),
        };
        let mut writer = Writer::new();
        writer.push_field(&image_desc);
        let mut buf1 = Cursor::new(Vec::new());
        writer.write(&mut buf1, false).unwrap();
        let mut buf2 = Cursor::new(Vec::new());
        writer.write(&mut buf2, false).unwrap();
        assert_eq!(buf1.into_inner(), buf2.into_inner());
    }

    #[test]
    fn compose_field_value() {
        let patterns = vec![
            (Value::Byte(vec![1, 2]),
             (1, 2, vec![1, 2]),
             (1, 2, vec![1, 2])),
            (Value::Ascii(vec![b"a".to_vec(), b"b".to_vec()]),
             (2, 4, b"a\0b\0".to_vec()),
             (2, 4, b"a\0b\0".to_vec())),
            (Value::Short(vec![0x0102, 0x0304]),
             (3, 2, b"\x01\x02\x03\x04".to_vec()),
             (3, 2, b"\x02\x01\x04\x03".to_vec())),
            (Value::Long(vec![0x01020304, 0x05060708]),
             (4, 2, b"\x01\x02\x03\x04\x05\x06\x07\x08".to_vec()),
             (4, 2, b"\x04\x03\x02\x01\x08\x07\x06\x05".to_vec())),
            (Value::Rational(vec![(1, 2).into(), (3, 4).into()]),
             (5, 2, b"\0\0\0\x01\0\0\0\x02\0\0\0\x03\0\0\0\x04".to_vec()),
             (5, 2, b"\x01\0\0\0\x02\0\0\0\x03\0\0\0\x04\0\0\0".to_vec())),
            (Value::SByte(vec![-2, -128]),
             (6, 2, b"\xfe\x80".to_vec()),
             (6, 2, b"\xfe\x80".to_vec())),
            (Value::Undefined(b"abc".to_vec(), 0),
             (7, 3, b"abc".to_vec()),
             (7, 3, b"abc".to_vec())),
            (Value::SShort(vec![-2, -0x8000]),
             (8, 2, b"\xff\xfe\x80\x00".to_vec()),
             (8, 2, b"\xfe\xff\x00\x80".to_vec())),
            (Value::SLong(vec![-2, -0x80000000]),
             (9, 2, b"\xff\xff\xff\xfe\x80\x00\x00\x00".to_vec()),
             (9, 2, b"\xfe\xff\xff\xff\x00\x00\x00\x80".to_vec())),
            (Value::SRational(vec![(-1, -2).into(), (-3, -4).into()]),
             (10, 2, b"\xff\xff\xff\xff\xff\xff\xff\xfe\
                       \xff\xff\xff\xfd\xff\xff\xff\xfc".to_vec()),
             (10, 2, b"\xff\xff\xff\xff\xfe\xff\xff\xff\
                       \xfd\xff\xff\xff\xfc\xff\xff\xff".to_vec())),
            (Value::Float(vec![2.5, -0.5]),
             (11, 2, b"\x40\x20\x00\x00\xbf\x00\x00\x00".to_vec()),
             (11, 2, b"\x00\x00\x20\x40\x00\x00\x00\xbf".to_vec())),
            (Value::Double(vec![2.5, -0.5]),
             (12, 2, b"\x40\x04\x00\x00\x00\x00\x00\x00\
                       \xbf\xe0\x00\x00\x00\x00\x00\x00".to_vec()),
             (12, 2, b"\x00\x00\x00\x00\x00\x00\x04\x40\
                       \x00\x00\x00\x00\x00\x00\xe0\xbf".to_vec())),
        ];
        for (val, be, le) in patterns {
            assert_eq!(compose_value::<BigEndian>(&val).unwrap(), be);
            assert_eq!(compose_value::<LittleEndian>(&val).unwrap(), le);
        }
    }
}
