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
use std::mem;
use std::slice;

use endian::{Endian, BigEndian, LittleEndian};
use error::Error;
use tag_priv::{Context, Tag, constants as tag};
use tiff::{Field, TIFF_BE_SIG, TIFF_LE_SIG};
use value::Value;

/// The `Writer` struct is used to encode and write Exif data.
pub struct Writer<'a> {
    tiff_fields: Vec<&'a Field<'a>>,
    exif_fields: Vec<&'a Field<'a>>,
    gps_fields: Vec<&'a Field<'a>>,
    interop_fields: Vec<&'a Field<'a>>,
    tn_tiff_fields: Vec<&'a Field<'a>>,
    tn_exif_fields: Vec<&'a Field<'a>>,
    tn_gps_fields: Vec<&'a Field<'a>>,
    tn_interop_fields: Vec<&'a Field<'a>>,
    strips: Option<&'a [&'a [u8]]>,
    tn_strips: Option<&'a [&'a [u8]]>,
    tiles: Option<&'a [&'a [u8]]>,
    tn_jpeg: Option<&'a [u8]>,
}

struct WriterState<'a> {
    tiff_fields: Vec<&'a Field<'a>>,
    exif_fields: Vec<&'a Field<'a>>,
    gps_fields: Vec<&'a Field<'a>>,
    interop_fields: Vec<&'a Field<'a>>,
    tiff_ifd_offset: u32,
    exif_ifd_offset: u32,
    gps_ifd_offset: u32,
    interop_ifd_offset: u32,
    strips: Option<&'a [&'a [u8]]>,
    tiles: Option<&'a [&'a [u8]]>,
    jpeg: Option<&'a [u8]>,
}

impl<'a> Writer<'a> {
    /// Constructs an empty `Writer`.
    pub fn new() -> Writer<'a> {
        Writer {
            tiff_fields: Vec::new(),
            exif_fields: Vec::new(),
            gps_fields: Vec::new(),
            interop_fields: Vec::new(),
            tn_tiff_fields: Vec::new(),
            tn_exif_fields: Vec::new(),
            tn_gps_fields: Vec::new(),
            tn_interop_fields: Vec::new(),
            strips: None,
            tn_strips: None,
            tiles: None,
            tn_jpeg: None,
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
            Field { tag: tag::ExifIFDPointer, .. } |
            Field { tag: tag::GPSInfoIFDPointer, .. } |
            Field { tag: tag::InteropIFDPointer, .. } => {},
            // These tags are synthesized from the actual strip/tile data.
            Field { tag: tag::StripOffsets, .. } |
            Field { tag: tag::StripByteCounts, .. } |
            Field { tag: tag::TileOffsets, .. } |
            Field { tag: tag::TileByteCounts, .. } => {},
            // These tags are synthesized from the actual JPEG thumbnail.
            Field { tag: tag::JPEGInterchangeFormat, .. } |
            Field { tag: tag::JPEGInterchangeFormatLength, .. } => {},
            // Other normal tags.
            Field { tag: Tag(Context::Tiff, _), thumbnail: false, .. } =>
                self.tiff_fields.push(field),
            Field { tag: Tag(Context::Exif, _), thumbnail: false, .. } =>
                self.exif_fields.push(field),
            Field { tag: Tag(Context::Gps, _), thumbnail: false, .. } =>
                self.gps_fields.push(field),
            Field { tag: Tag(Context::Interop, _), thumbnail: false, .. } =>
                self.interop_fields.push(field),
            Field { tag: Tag(Context::Tiff, _), thumbnail: true, .. } =>
                self.tn_tiff_fields.push(field),
            Field { tag: Tag(Context::Exif, _), thumbnail: true, .. } =>
                self.tn_exif_fields.push(field),
            Field { tag: Tag(Context::Gps, _), thumbnail: true, .. } =>
                self.tn_gps_fields.push(field),
            Field { tag: Tag(Context::Interop, _), thumbnail: true, .. } =>
                self.tn_interop_fields.push(field),
        }
    }

    /// Sets TIFF strips for the primary image.
    /// If this method is called multiple times, the last one is used.
    pub fn set_strips(&mut self, strips: &'a [&'a [u8]]) {
        self.strips = Some(strips);
    }

    /// Sets TIFF strips for the thumbnail image.
    /// If this method is called multiple times, the last one is used.
    pub fn set_thumbnail_strips(&mut self, strips: &'a [&'a [u8]]) {
        self.tn_strips = Some(strips);
    }

    /// Sets TIFF tiles for the primary image.
    /// If this method is called multiple times, the last one is used.
    pub fn set_tiles(&mut self, tiles: &'a [&'a [u8]]) {
        self.tiles = Some(tiles);
    }

    /// Sets JPEG data for the thumbnail image.
    /// If this method is called multiple times, the last one is used.
    pub fn set_thumbnail_jpeg(&mut self, jpeg: &'a [u8]) {
        self.tn_jpeg = Some(jpeg);
    }

    /// Encodes Exif data and writes it into `w`.
    ///
    /// The write position of `w` must be set to zero before calling
    /// this method.
    pub fn write<W>(&mut self, w: &mut W, little_endian: bool)
                    -> Result<(), Error> where W: Write + Seek {
        // TIFF signature and the offset of the 0th IFD.
        if little_endian {
            try!(w.write_all(&TIFF_LE_SIG));
            try!(LittleEndian::writeu32(w, 8));
        } else {
            try!(w.write_all(&TIFF_BE_SIG));
            try!(BigEndian::writeu32(w, 8));
        }

        // Write the primary image.
        let ws = WriterState {
            tiff_fields: self.tiff_fields.clone(),
            exif_fields: self.exif_fields.clone(),
            gps_fields: self.gps_fields.clone(),
            interop_fields: self.interop_fields.clone(),
            tiff_ifd_offset: 0,
            exif_ifd_offset: 0,
            gps_ifd_offset: 0,
            interop_ifd_offset: 0,
            strips: self.strips,
            tiles: self.tiles,
            jpeg: None,
        };
        let next_ifd_offset_offset =
            try!(synthesize_fields(w, ws, false, little_endian));

        // Do not output the thumbnail IFD if there are no data in it.
        let thumbnail_absent =
            self.tn_tiff_fields.len() == 0 &&
            self.tn_exif_fields.len() == 0 &&
            self.tn_gps_fields.len() == 0 &&
            self.tn_interop_fields.len() == 0 &&
            self.tn_strips == None &&
            self.tn_jpeg == None;
        if thumbnail_absent {
            try!(w.flush());
            return Ok(());
        }

        let next_ifd_offset = try!(pad_and_get_offset(w));
        let origpos = try!(w.seek(SeekFrom::Current(0)));
        try!(w.seek(SeekFrom::Start(next_ifd_offset_offset as u64)));
        match little_endian {
            false => try!(BigEndian::writeu32(w, next_ifd_offset)),
            true => try!(LittleEndian::writeu32(w, next_ifd_offset)),
        }
        try!(w.seek(SeekFrom::Start(origpos)));

        // Write the thumbnail image.
        let ws = WriterState {
            tiff_fields: self.tn_tiff_fields.clone(),
            exif_fields: self.tn_exif_fields.clone(),
            gps_fields: self.tn_gps_fields.clone(),
            interop_fields: self.tn_interop_fields.clone(),
            tiff_ifd_offset: 0,
            exif_ifd_offset: 0,
            gps_ifd_offset: 0,
            interop_ifd_offset: 0,
            strips: self.tn_strips,
            tiles: None,
            jpeg: self.tn_jpeg,
        };
        try!(synthesize_fields(w, ws, true, little_endian));

        try!(w.flush());
        Ok(())
    }
}

// Synthesizes special fields, writes an image, and returns the offset
// of the next IFD offset.
fn synthesize_fields<W>(w: &mut W, ws: WriterState, thumbnail: bool,
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
    // Shrink the scope so that referenced fields live longer than ws.
    let mut ws = ws;

    if let Some(strips) = ws.strips {
        strip_offsets = Field {
            tag: tag::StripOffsets,
            thumbnail: thumbnail,
            value: Value::Long(vec![0; strips.len()]),
        };
        ws.tiff_fields.push(&strip_offsets);
        strip_byte_counts = Field {
            tag: tag::StripByteCounts,
            thumbnail: thumbnail,
            value: Value::Long(
                strips.iter().map(|s| s.len() as u32).collect()),
        };
        ws.tiff_fields.push(&strip_byte_counts);
    }
    if let Some(tiles) = ws.tiles {
        tile_offsets = Field {
            tag: tag::TileOffsets,
            thumbnail: thumbnail,
            value: Value::Long(vec![0; tiles.len()]),
        };
        ws.tiff_fields.push(&tile_offsets);
        tile_byte_counts = Field {
            tag: tag::TileByteCounts,
            thumbnail: thumbnail,
            value: Value::Long(
                tiles.iter().map(|s| s.len() as u32).collect()),
        };
        ws.tiff_fields.push(&tile_byte_counts);
    }
    if let Some(jpeg) = ws.jpeg {
        jpeg_offset = Field {
            tag: tag::JPEGInterchangeFormat,
            thumbnail: thumbnail,
            value: Value::Long(vec![0]),
        };
        ws.tiff_fields.push(&jpeg_offset);
        jpeg_length = Field {
            tag: tag::JPEGInterchangeFormatLength,
            thumbnail: thumbnail,
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

    ws.tiff_ifd_offset = try!(reserve_ifd(w, tiff_fields_len));
    if exif_fields_len > 0 {
        ws.exif_ifd_offset = try!(reserve_ifd(w, exif_fields_len));
        exif_in_tiff = Field {
            tag: tag::ExifIFDPointer,
            thumbnail: thumbnail,
            value: Value::Long(vec![ws.exif_ifd_offset]),
        };
        ws.tiff_fields.push(&exif_in_tiff);
    }
    if gps_fields_len > 0 {
        ws.gps_ifd_offset = try!(reserve_ifd(w, gps_fields_len));
        gps_in_tiff = Field {
            tag: tag::GPSInfoIFDPointer,
            thumbnail: thumbnail,
            value: Value::Long(vec![ws.gps_ifd_offset]),
        };
        ws.tiff_fields.push(&gps_in_tiff);
    }
    if interop_fields_len > 0 {
        ws.interop_ifd_offset = try!(reserve_ifd(w, interop_fields_len));
        interop_in_exif = Field {
            tag: tag::InteropIFDPointer,
            thumbnail: thumbnail,
            value: Value::Long(vec![ws.interop_ifd_offset]),
        };
        ws.exif_fields.push(&interop_in_exif);
    }

    ws.tiff_fields.sort_by_key(|f| f.tag.number());
    ws.exif_fields.sort_by_key(|f| f.tag.number());
    ws.gps_fields.sort_by_key(|f| f.tag.number());
    ws.interop_fields.sort_by_key(|f| f.tag.number());

    match little_endian {
        false => write_image::<_, BigEndian>(w, ws),
        true => write_image::<_, LittleEndian>(w, ws),
    }
}

// Writes an image and returns the offset of the next IFD offset.
fn write_image<W, E>(w: &mut W, ws: WriterState)
                     -> Result<u32, Error> where W: Write + Seek, E: Endian {
    let (next_ifd_offset_offset,
         strip_offsets_offset, tile_offsets_offset, jpeg_offset) =
        try!(write_ifd_and_fields::<_, E>(
            w, &ws.tiff_fields, ws.tiff_ifd_offset));
    if ws.exif_fields.len() > 0 {
        try!(write_ifd_and_fields::<_, E>(
            w, &ws.exif_fields, ws.exif_ifd_offset));
    }
    if ws.gps_fields.len() > 0 {
        try!(write_ifd_and_fields::<_, E>(
            w, &ws.gps_fields, ws.gps_ifd_offset));
    }
    if ws.interop_fields.len() > 0 {
        try!(write_ifd_and_fields::<_, E>(
            w, &ws.interop_fields, ws.interop_ifd_offset));
    }

    if let Some(strips) = ws.strips {
        let mut strip_offsets = Vec::new();
        for strip in strips {
            strip_offsets.push(try!(get_offset(w)));
            try!(w.write_all(strip));
        }
        let origpos = try!(w.seek(SeekFrom::Current(0)));
        try!(w.seek(SeekFrom::Start(strip_offsets_offset as u64)));
        for ofs in strip_offsets {
            try!(E::writeu32(w, ofs));
        }
        try!(w.seek(SeekFrom::Start(origpos)));
    }
    if let Some(tiles) = ws.tiles {
        let mut tile_offsets = Vec::new();
        for tile in tiles {
            tile_offsets.push(try!(get_offset(w)));
            try!(w.write_all(tile));
        }
        let origpos = try!(w.seek(SeekFrom::Current(0)));
        try!(w.seek(SeekFrom::Start(tile_offsets_offset as u64)));
        for ofs in tile_offsets {
            try!(E::writeu32(w, ofs));
        }
        try!(w.seek(SeekFrom::Start(origpos)));
    }
    if let Some(jpeg) = ws.jpeg {
        let offset = try!(get_offset(w));
        try!(w.write_all(jpeg));
        let origpos = try!(w.seek(SeekFrom::Current(0)));
        try!(w.seek(SeekFrom::Start(jpeg_offset as u64)));
        try!(E::writeu32(w, offset));
        try!(w.seek(SeekFrom::Start(origpos)));
    }

    Ok(next_ifd_offset_offset)
}

// Advances the write position to make a space for a new IFD and
// returns the offset of the IFD.
fn reserve_ifd<W>(w: &mut W, count: usize)
                  -> Result<u32, Error> where W: Write + Seek {
    let ifdpos = try!(get_offset(w));
    assert!(ifdpos % 2 == 0);
    // The number of entries (2) + array of entries (12 * n) +
    // the next IFD pointer (4).
    try!(w.seek(SeekFrom::Current(2 + count as i64 * 12 + 4)));
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
    try!(E::writeu16(&mut ifd, fields.len() as u16));
    // Write the fields.
    for f in fields {
        let (typ, cnt, mut valbuf) = try!(compose_value::<E>(&f.value));
        if cnt as u32 as usize != cnt {
            return Err(Error::TooBig("Too long array"));
        }
        try!(E::writeu16(&mut ifd, f.tag.number()));
        try!(E::writeu16(&mut ifd, typ));
        try!(E::writeu32(&mut ifd, cnt as u32));
        // Embed the value itself into the offset, or
        // encode as an offset and the value.
        if valbuf.len() <= 4 {
            valbuf.resize(4, 0);
            try!(ifd.write_all(&valbuf));
        } else {
            // The value must begin on a word boundary. [TIFF6, Section 2:
            // TIFF Structure, Image File Directory, IFD Entry, p. 15]
            let valofs = try!(pad_and_get_offset(w));
            try!(E::writeu32(&mut ifd, valofs));
            try!(w.write_all(&valbuf));
        }
        if f.tag == tag::StripOffsets {
            strip_offsets_offset = match valbuf.len() {
                0...4 => ifd_offset + ifd.len() as u32 - 4,
                _ => try!(get_offset(w)) - valbuf.len() as u32,
            };
        }
        if f.tag == tag::TileOffsets {
            tile_offsets_offset = match valbuf.len() {
                0...4 => ifd_offset + ifd.len() as u32 - 4,
                _ => try!(get_offset(w)) - valbuf.len() as u32,
            };
        }
        if f.tag == tag::JPEGInterchangeFormat {
            jpeg_offset = ifd_offset + ifd.len() as u32 - 4;
        }
    }
    // Write the next IFD pointer.
    let next_ifd_offset_offset = ifd_offset + ifd.len() as u32;
    try!(E::writeu32(&mut ifd, 0));

    // Write the IFD.
    try!(write_at(w, &ifd, ifd_offset));

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
            for &s in vec {
                buf.extend_from_slice(s);
                buf.push(0);
            }
            Ok((2, buf.len(), buf))
        },
        Value::Short(ref vec) => {
            let mut buf = Vec::new();
            for &v in vec {
                try!(E::writeu16(&mut buf, v));
            }
            Ok((3, vec.len(), buf))
        },
        Value::Long(ref vec) => {
            let mut buf = Vec::new();
            for &v in vec {
                try!(E::writeu32(&mut buf, v));
            }
            Ok((4, vec.len(), buf))
        },
        Value::Rational(ref vec) => {
            let mut buf = Vec::new();
            for v in vec {
                try!(E::writeu32(&mut buf, v.num));
                try!(E::writeu32(&mut buf, v.denom));
            }
            Ok((5, vec.len(), buf))
        },
        Value::SByte(ref vec) => {
            let uslice = unsafe { slice::from_raw_parts(
                vec.as_ptr() as *const u8, vec.len()) };
            Ok((6, vec.len(), uslice.to_vec()))
        },
        Value::Undefined(ref s) =>
            Ok((7, s.len(), s.to_vec())),
        Value::SShort(ref vec) => {
            let mut buf = Vec::new();
            for &v in vec {
                try!(E::writeu16(&mut buf, v as u16));
            }
            Ok((8, vec.len(), buf))
        },
        Value::SLong(ref vec) => {
            let mut buf = Vec::new();
            for &v in vec {
                try!(E::writeu32(&mut buf, v as u32));
            }
            Ok((9, vec.len(), buf))
        },
        Value::SRational(ref vec) => {
            let mut buf = Vec::new();
            for v in vec {
                try!(E::writeu32(&mut buf, v.num as u32));
                try!(E::writeu32(&mut buf, v.denom as u32));
            }
            Ok((10, vec.len(), buf))
        },
        Value::Float(ref vec) => {
            let mut buf = Vec::new();
            for &v in vec {
                try!(E::writeu32(&mut buf, unsafe { mem::transmute(v) }));
            }
            Ok((11, vec.len(), buf))
        },
        Value::Double(ref vec) => {
            let mut buf = Vec::new();
            for &v in vec {
                try!(E::writeu64(&mut buf, unsafe { mem::transmute(v) }));
            }
            Ok((12, vec.len(), buf))
        },
        Value::Unknown(_, _, _) =>
            Err(Error::NotSupported("Cannot write unknown field types")),
    }
}

fn write_at<W>(w: &mut W, buf: &[u8], offset: u32)
               -> io::Result<()> where W: Write + Seek {
    let orig = try!(w.seek(SeekFrom::Current(0)));
    try!(w.seek(SeekFrom::Start(offset as u64)));
    try!(w.write_all(buf));
    try!(w.seek(SeekFrom::Start(orig)));
    Ok(())
}

// Aligns `w` to the two-byte (word) boundary and returns the new offset.
fn pad_and_get_offset<W>(w: &mut W)
                         -> Result<u32, Error> where W: Write + Seek {
    let mut pos = try!(w.seek(SeekFrom::Current(0)));
    if pos >= (1 << 32) - 1 {
        return Err(Error::TooBig("Offset too large"));
    }
    if pos % 2 != 0 {
        try!(w.write_all(&[0]));
        pos += 1;
    }
    Ok(pos as u32)
}

fn get_offset<W>(w: &mut W)
                 -> Result<u32, Error> where W: Write + Seek {
    let pos = try!(w.seek(SeekFrom::Current(0)));
    if pos as u32 as u64 != pos {
        return Err(Error::TooBig("Offset too large"));
    }
    Ok(pos as u32)
}

#[cfg(test)]
mod tests {
    use std::io::Cursor;
    use value::{Rational, SRational};
    use super::*;

    #[test]
    fn primary() {
        let image_desc = Field {
            tag: tag::ImageDescription,
            thumbnail: false,
            value: Value::Ascii(vec![b"Sample"]),
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
            tag: tag::ExifVersion,
            thumbnail: false,
            value: Value::Undefined(b"0231"),
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
        writer.set_tiles(tiles);
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
        let mut writer = Writer::new();
        let mut buf = Cursor::new(Vec::new());
        writer.set_thumbnail_jpeg(jpeg);
        writer.write(&mut buf, false).unwrap();
        let expected: &[u8] =
            b"\x4d\x4d\x00\x2a\x00\x00\x00\x08\
              \x00\x00\x00\x00\x00\x0e\
              \x00\x02\x02\x01\x00\x04\x00\x00\x00\x01\x00\x00\x00\x2c\
                      \x02\x02\x00\x04\x00\x00\x00\x01\x00\x00\x00\x04\
              \x00\x00\x00\x00\
              JPEG";
        assert_eq!(buf.into_inner(), expected);
    }

    #[test]
    fn thumbnail_tiff() {
        // This is not a valid TIFF strip (only for testing).
        let strips: &[&[u8]] = &[b"STRIP"];
        let mut writer = Writer::new();
        let mut buf = Cursor::new(Vec::new());
        writer.set_thumbnail_strips(strips);
        writer.write(&mut buf, false).unwrap();
        let expected: &[u8] =
            b"\x4d\x4d\x00\x2a\x00\x00\x00\x08\
              \x00\x00\x00\x00\x00\x0e\
              \x00\x02\x01\x11\x00\x04\x00\x00\x00\x01\x00\x00\x00\x2c\
                      \x01\x17\x00\x04\x00\x00\x00\x01\x00\x00\x00\x05\
              \x00\x00\x00\x00\
              STRIP";
        assert_eq!(buf.into_inner(), expected);
    }

    #[test]
    fn primary_and_thumbnail() {
        let image_desc = Field {
            tag: tag::ImageDescription,
            thumbnail: false,
            value: Value::Ascii(vec![b"Sample"]),
        };
        let exif_ver = Field {
            tag: tag::ExifVersion,
            thumbnail: false,
            value: Value::Undefined(b"0231"),
        };
        let gps_ver = Field {
            tag: tag::GPSVersionID,
            thumbnail: false,
            value: Value::Byte(vec![2, 3, 0, 0]),
        };
        let interop_index = Field {
            tag: tag::InteroperabilityIndex,
            thumbnail: false,
            value: Value::Ascii(vec![b"ABC"]),
        };
        let jpeg = b"JPEG";
        let mut writer = Writer::new();
        let mut buf = Cursor::new(Vec::new());
        writer.push_field(&image_desc);
        writer.push_field(&exif_ver);
        writer.push_field(&gps_ver);
        writer.push_field(&interop_index);
        writer.set_thumbnail_jpeg(jpeg);
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
    fn write_twice() {
        let image_desc = Field {
            tag: tag::ImageDescription,
            thumbnail: false,
            value: Value::Ascii(vec![b"Sample"]),
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
            (Value::Ascii(vec![b"a", b"b"]),
             (2, 4, b"a\0b\0".to_vec()),
             (2, 4, b"a\0b\0".to_vec())),
            (Value::Short(vec![0x0102, 0x0304]),
             (3, 2, b"\x01\x02\x03\x04".to_vec()),
             (3, 2, b"\x02\x01\x04\x03".to_vec())),
            (Value::Long(vec![0x01020304, 0x05060708]),
             (4, 2, b"\x01\x02\x03\x04\x05\x06\x07\x08".to_vec()),
             (4, 2, b"\x04\x03\x02\x01\x08\x07\x06\x05".to_vec())),
            (Value::Rational(vec![Rational { num: 1, denom: 2},
                                  Rational { num: 3, denom: 4}]),
             (5, 2, b"\0\0\0\x01\0\0\0\x02\0\0\0\x03\0\0\0\x04".to_vec()),
             (5, 2, b"\x01\0\0\0\x02\0\0\0\x03\0\0\0\x04\0\0\0".to_vec())),
            (Value::SByte(vec![-2, -128]),
             (6, 2, b"\xfe\x80".to_vec()),
             (6, 2, b"\xfe\x80".to_vec())),
            (Value::Undefined(b"abc"),
             (7, 3, b"abc".to_vec()),
             (7, 3, b"abc".to_vec())),
            (Value::SShort(vec![-2, -0x8000]),
             (8, 2, b"\xff\xfe\x80\x00".to_vec()),
             (8, 2, b"\xfe\xff\x00\x80".to_vec())),
            (Value::SLong(vec![-2, -0x80000000]),
             (9, 2, b"\xff\xff\xff\xfe\x80\x00\x00\x00".to_vec()),
             (9, 2, b"\xfe\xff\xff\xff\x00\x00\x00\x80".to_vec())),
            (Value::SRational(vec![SRational { num: -1, denom: -2},
                                   SRational { num: -3, denom: -4}]),
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
        for (val, be, le) in patterns.into_iter() {
            assert_eq!(compose_value::<BigEndian>(&val).unwrap(), be);
            assert_eq!(compose_value::<LittleEndian>(&val).unwrap(), le);
        }
    }
}
