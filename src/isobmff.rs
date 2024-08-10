//
// Copyright (c) 2020 KAMADA Ken'ichi.
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

use std::io::{BufRead, ErrorKind, Seek, SeekFrom};

use crate::endian::{Endian, BigEndian};
use crate::error::Error;
use crate::util::{read64, BufReadExt as _, ReadExt as _};

// Checking "mif1" in the compatible brands should be enough, because
// the "heic", "heix", "heim", and "heis" files shall include "mif1"
// among the compatible brands [ISO23008-12 B.4.1] [ISO23008-12 B.4.3].
// Same for "msf1" [ISO23008-12 B.4.2] [ISO23008-12 B.4.4].
static HEIF_BRANDS: &[[u8; 4]] = &[*b"mif1", *b"msf1"];

pub(crate) const MAX_EXIF_SIZE: usize = 65535;

// Most errors in this file are Error::InvalidFormat.
impl From<&'static str> for Error {
    fn from(err: &'static str) -> Error {
        Error::InvalidFormat(err)
    }
}

pub fn get_exif_attr<R>(reader: &mut R) -> Result<Vec<u8>, Error>
where R: BufRead + Seek {
    let mut parser = Parser::new(reader);
    match parser.parse() {
        Err(Error::Io(ref e)) if e.kind() == ErrorKind::UnexpectedEof =>
            Err("Broken HEIF file".into()),
        Err(e) => Err(e),
        Ok(mut buf) => {
            if buf.len() < 4 {
                return Err("ExifDataBlock too small".into());
            }
            let offset = BigEndian::loadu32(&buf, 0) as usize;
            if buf.len() - 4 < offset {
                return Err("Invalid Exif header offset".into());
            }
            buf.drain(.. 4 + offset);
            Ok(buf)
        },
    }
}

#[derive(Debug)]
struct Parser<R> {
    reader: R,
    // Whether the file type box has been checked.
    ftyp_checked: bool,
    // The item where Exif data is stored.
    item_id: Option<u32>,
    // The location of the item_id.
    item_location: Option<Location>,
}

#[derive(Debug)]
struct Location {
    construction_method: u8,
    // index, offset, length
    extents: Vec<(u64, u64, u64)>,
    base_offset: u64,
}

impl<R> Parser<R> where R: BufRead + Seek {
    fn new(reader: R) -> Self {
        Self {
            reader: reader,
            ftyp_checked: false,
            item_id: None,
            item_location: None,
        }
    }

    fn parse(&mut self) -> Result<Vec<u8>, Error> {
        while let Some((size, boxtype)) = self.read_box_header()? {
            match &boxtype {
                b"ftyp" => {
                    let buf = self.read_file_level_box(size)?;
                    self.parse_ftyp(BoxSplitter::new(&buf))?;
                    self.ftyp_checked = true;
                },
                b"meta" => {
                    if !self.ftyp_checked {
                        return Err("MetaBox found before FileTypeBox".into());
                    }
                    let buf = self.read_file_level_box(size)?;
                    let exif = self.parse_meta(BoxSplitter::new(&buf))?;
                    return Ok(exif);
                },
                _ => self.skip_file_level_box(size)?,
            }
        }
        Err(Error::NotFound("HEIF"))
    }

    // Reads size, type, and largesize,
    // and returns body size and type.
    // If no byte can be read due to EOF, None is returned.
    fn read_box_header(&mut self) -> Result<Option<(u64, [u8; 4])>, Error> {
        if self.reader.is_eof()? {
            return Ok(None);
        }
        let mut buf = [0; 8];
        self.reader.read_exact(&mut buf)?;
        let size = match BigEndian::loadu32(&buf, 0) {
            0 => Some(std::u64::MAX),
            1 => read64(&mut self.reader)?.checked_sub(16),
            x => u64::from(x).checked_sub(8),
        }.ok_or("Invalid box size")?;
        let boxtype = buf[4..8].try_into().expect("never fails");
        Ok(Some((size, boxtype)))
    }

    fn read_file_level_box(&mut self, size: u64) -> Result<Vec<u8>, Error> {
        let mut buf;
        match size {
            std::u64::MAX => {
                buf = Vec::new();
                self.reader.read_to_end(&mut buf)?;
            },
            _ => {
                let size = size.try_into()
                    .or(Err("Box is larger than the address space"))?;
                buf = Vec::new();
                self.reader.read_exact_len(&mut buf, size)?;
            },
        }
        Ok(buf)
    }

    fn skip_file_level_box(&mut self, size: u64) -> Result<(), Error> {
        match size {
            std::u64::MAX => self.reader.seek(SeekFrom::End(0))?,
            _ => self.reader.seek(SeekFrom::Current(
                size.try_into().or(Err("Large seek not supported"))?))?,
        };
        Ok(())
    }

    fn parse_ftyp(&mut self, mut boxp: BoxSplitter) -> Result<(), Error> {
        let head = boxp.slice(8)?;
        let _major_brand = &head[0..4];
        let _minor_version = BigEndian::loadu32(&head, 4);
        while let Ok(compat_brand) = boxp.array4() {
            if HEIF_BRANDS.contains(&compat_brand) {
                return Ok(());
            }
        }
        Err("No compatible brand recognized in ISO base media file".into())
    }

    fn parse_meta(&mut self, mut boxp: BoxSplitter) -> Result<Vec<u8>, Error> {
        let (version, _flags) = boxp.fullbox_header()?;
        if version != 0 {
            return Err("Unsupported MetaBox".into());
        }
        let mut idat = None;
        let mut iloc = None;
        while !boxp.is_empty() {
            let (boxtype, mut body) = boxp.child_box()?;
            match boxtype {
                b"idat" => idat = Some(body.slice(body.len())?),
                b"iinf" => self.parse_iinf(body)?,
                b"iloc" => iloc = Some(body),
                _ => {},
            }
        }

        self.item_id.ok_or(Error::NotFound("HEIF"))?;
        self.parse_iloc(iloc.ok_or("No ItemLocationBox")?)?;
        let location = self.item_location.as_ref()
            .ok_or("No matching item in ItemLocationBox")?;
        let mut buf = Vec::new();
        match location.construction_method {
            0 => {
                for &(_, off, len) in &location.extents {
                    let off = location.base_offset.checked_add(off)
                        .ok_or("Invalid offset")?;
                    // Seeking beyond the EOF is allowed and
                    // implementation-defined, but the subsequent read
                    // should fail.
                    self.reader.seek(SeekFrom::Start(off))?;
                    match len {
                        0 => { self.reader.read_to_end(&mut buf)?; },
                        _ => {
                            let len = len.try_into()
                                .or(Err("Extent too large"))?;
                            self.reader.read_exact_len(&mut buf, len)?;
                        },
                    }
                    if buf.len() > MAX_EXIF_SIZE {
                        return Err("Exif data too large".into());
                    }
                }
            },
            1 => {
                let idat = idat.ok_or("No ItemDataBox")?;
                for &(_, off, len) in &location.extents {
                    let off = location.base_offset.checked_add(off)
                        .ok_or("Invalid offset")?;
                    let end = off.checked_add(len).ok_or("Invalid length")?;
                    let off = off.try_into().or(Err("Offset too large"))?;
                    let end = end.try_into().or(Err("Length too large"))?;
                    buf.extend_from_slice(match len {
                        0 => idat.get(off..),
                        _ => idat.get(off..end),
                    }.ok_or("Out of ItemDataBox")?);
                    if buf.len() > MAX_EXIF_SIZE {
                        return Err("Exif data too large".into());
                    }
                }
            },
            2 => return Err(Error::NotSupported(
                "Construction by item offset is not supported")),
            _ => return Err("Invalid construction_method".into()),
        }
        Ok(buf)
    }

    fn parse_iloc(&mut self, mut boxp: BoxSplitter) -> Result<(), Error> {
        let (version, _flags) = boxp.fullbox_header()?;
        let tmp = boxp.uint16().map(usize::from)?;
        let (offset_size, length_size, base_offset_size) =
            (tmp >> 12, tmp >> 8 & 0xf, tmp >> 4 & 0xf);
        let index_size = match version { 1 | 2 => tmp & 0xf, _ => 0 };
        let item_count = match version {
            0 | 1 => boxp.uint16()?.into(),
            2 => boxp.uint32()?,
            _ => return Err("Unsupported ItemLocationBox".into()),
        };
        for _ in 0..item_count {
            let item_id = match version {
                0 | 1 => boxp.uint16()?.into(),
                2 => boxp.uint32()?,
                _ => unreachable!(),
            };
            let construction_method = match version {
                0 => 0,
                1 | 2 => boxp.slice(2).map(|x| x[1] & 0xf)?,
                _ => unreachable!(),
            };
            let data_ref_index = boxp.uint16()?;
            if construction_method == 0 && data_ref_index != 0 {
                return Err(Error::NotSupported(
                    "External data reference is not supported"));
            }
            let base_offset = boxp.size048(base_offset_size)?
                .ok_or("Invalid base_offset_size")?;
            let extent_count = boxp.uint16()?.into();
            if self.item_id == Some(item_id) {
                let mut extents = Vec::with_capacity(extent_count);
                for _ in 0..extent_count {
                    let index = boxp.size048(index_size)?
                        .ok_or("Invalid index_size")?;
                    let offset = boxp.size048(offset_size)?
                        .ok_or("Invalid offset_size")?;
                    let length = boxp.size048(length_size)?
                        .ok_or("Invalid length_size")?;
                    extents.push((index, offset, length));
                }
                self.item_location = Some(Location {
                    construction_method, extents, base_offset });
            } else {
                // (15 + 15 + 15) * u16::MAX never overflows.
                boxp.slice((index_size + offset_size + length_size) *
                           extent_count)?;
            }
        }
        Ok(())
    }

    fn parse_iinf(&mut self, mut boxp: BoxSplitter) -> Result<(), Error> {
        let (version, _flags) = boxp.fullbox_header()?;
        let entry_count = match version {
            0 => boxp.uint16()?.into(),
            _ => boxp.uint32()?,
        };
        for _ in 0..entry_count {
            let (boxtype, body) = boxp.child_box()?;
            match boxtype {
                b"infe" => self.parse_infe(body)?,
                _ => {},
            }
        }
        Ok(())
    }

    fn parse_infe(&mut self, mut boxp: BoxSplitter) -> Result<(), Error> {
        let (version, _flags) = boxp.fullbox_header()?;
        let item_id = match version {
            2 => boxp.uint16()?.into(),
            3 => boxp.uint32()?,
            _ => return Err("Unsupported ItemInfoEntry".into()),
        };
        let _item_protection_index = boxp.slice(2)?;
        let item_type = boxp.slice(4)?;
        if item_type == b"Exif" {
            self.item_id = Some(item_id);
        }
        Ok(())
    }
}

pub fn is_heif(buf: &[u8]) -> bool {
    let mut boxp = BoxSplitter::new(buf);
    while let Ok((boxtype, mut body)) = boxp.child_box() {
        if boxtype == b"ftyp" {
            let _major_brand_minor_version = if body.slice(8).is_err() {
                return false;
            };
            while let Ok(compat_brand) = body.array4() {
                if HEIF_BRANDS.contains(&compat_brand) {
                    return true;
                }
            }
            return false;
        }
    }
    false
}

pub(crate) struct BoxSplitter<'a> {
    inner: &'a [u8],
}

impl<'a> BoxSplitter<'a> {
    pub(crate) fn new(slice: &'a [u8]) -> BoxSplitter<'a> {
        Self { inner: slice }
    }

    pub(crate) fn is_empty(&self) -> bool {
        self.inner.is_empty()
    }

    pub(crate) fn len(&self) -> usize {
        self.inner.len()
    }

    // Returns type and body.
    pub(crate) fn child_box(&mut self) -> Result<(&'a [u8], BoxSplitter<'a>), Error> {
        let size = self.uint32()? as usize;
        let boxtype = self.slice(4)?;
        let body_len = match size {
            0 => Some(self.len()),
            1 => usize::try_from(self.uint64()?)
                .or(Err("Box is larger than the address space"))?
                .checked_sub(16),
            _ => size.checked_sub(8),
        }.ok_or("Invalid box size")?;
        let body = self.slice(body_len)?;
        Ok((boxtype, BoxSplitter::new(body)))
    }

    // Returns 0-, 4-, or 8-byte unsigned integer.
    pub(crate) fn size048(&mut self, size: usize) -> Result<Option<u64>, Error> {
        match size {
            0 => Ok(Some(0)),
            4 => self.uint32().map(u64::from).map(Some),
            8 => self.uint64().map(Some),
            _ => Ok(None),
        }
    }

    // Returns version and flags.
    pub(crate) fn fullbox_header(&mut self) -> Result<(u32, u32), Error> {
        let tmp = self.uint32()?;
        Ok((tmp >> 24, tmp & 0xffffff))
    }

    pub(crate) fn uint16(&mut self) -> Result<u16, Error> {
        self.slice(2).map(|num| BigEndian::loadu16(num, 0))
    }

    pub(crate) fn uint32(&mut self) -> Result<u32, Error> {
        self.slice(4).map(|num| BigEndian::loadu32(num, 0))
    }

    pub(crate) fn uint64(&mut self) -> Result<u64, Error> {
        self.slice(8).map(|num| BigEndian::loadu64(num, 0))
    }

    pub(crate) fn array4(&mut self) -> Result<[u8; 4], Error> {
        self.slice(4).map(|x| x.try_into().expect("never fails"))
    }

    pub(crate) fn slice(&mut self, at: usize) -> Result<&'a [u8], Error> {
        let slice = self.inner.get(..at).ok_or("Box too small")?;
        self.inner = &self.inner[at..];
        Ok(slice)
    }
}

#[cfg(test)]
mod tests {
    use std::io::Cursor;
    use super::*;

    #[test]
    fn extract() {
        let file = std::fs::File::open("tests/exif.heic").unwrap();
        let buf = get_exif_attr(
            &mut std::io::BufReader::new(&file)).unwrap();
        assert_eq!(buf.len(), 79);
        assert!(buf.starts_with(b"MM\x00\x2a"));
        assert!(buf.ends_with(b"xif\0"));
    }

    #[test]
    fn unknown_before_ftyp() {
        let data =
            b"\0\0\0\x09XXXXx\
              \0\0\0\x14ftypmif1\0\0\0\0mif1\
              \0\0\0\x57meta\0\0\0\0\
                  \0\0\0\x18iloc\x01\0\0\0\0\0\0\x01\x1e\x1d\0\x01\0\0\0\x01\
                  \0\0\0\x22iinf\0\0\0\0\0\x01\
                      \0\0\0\x14infe\x02\0\0\0\x1e\x1d\0\0Exif\
                  \0\0\0\x11idat\0\0\0\x01xabcd";
        assert!(is_heif(data));
        let exif = get_exif_attr(&mut Cursor::new(&data[..])).unwrap();
        assert_eq!(exif, b"abcd");
    }

    #[test]
    fn bad_exif_data_block() {
        let data =
            b"\0\0\0\x14ftypmif1\0\0\0\0mif1\
              \0\0\0\x52meta\0\0\0\0\
                  \0\0\0\x18iloc\x01\0\0\0\0\0\0\x01\x1e\x1d\0\x01\0\0\0\x01\
                  \0\0\0\x22iinf\0\0\0\0\0\x01\
                      \0\0\0\x14infe\x02\0\0\0\x1e\x1d\0\0Exif\
                  \0\0\0\x0cidat\0\0\0\x01";
        assert_err_pat!(get_exif_attr(&mut Cursor::new(&data[..])),
                        Error::InvalidFormat("Invalid Exif header offset"));

        let data =
            b"\0\0\0\x14ftypmif1\0\0\0\0mif1\
              \0\0\0\x51meta\0\0\0\0\
                  \0\0\0\x18iloc\x01\0\0\0\0\0\0\x01\x1e\x1d\0\x01\0\0\0\x01\
                  \0\0\0\x22iinf\0\0\0\0\0\x01\
                      \0\0\0\x14infe\x02\0\0\0\x1e\x1d\0\0Exif\
                  \0\0\0\x0bidat\0\0\0";
        assert_err_pat!(get_exif_attr(&mut Cursor::new(&data[..])),
                        Error::InvalidFormat("ExifDataBlock too small"));
    }

    #[test]
    fn parser_box_header() {
        // size
        let mut p = Parser::new(Cursor::new(b"\0\0\0\x08abcd"));
        assert_eq!(p.read_box_header().unwrap(), Some((0, *b"abcd")));
        let mut p = Parser::new(Cursor::new(b"\0\0\0\x08abc"));
        assert_err_pat!(p.read_box_header(), Error::Io(_));
        let mut p = Parser::new(Cursor::new(b"\0\0\0\x07abcd"));
        assert_err_pat!(p.read_box_header(), Error::InvalidFormat(_));
        // max size
        let mut p = Parser::new(Cursor::new(b"\xff\xff\xff\xffabcd"));
        assert_eq!(p.read_box_header().unwrap(),
                   Some((0xffffffff - 8, *b"abcd")));
        // to the end of the file
        let mut p = Parser::new(Cursor::new(b"\0\0\0\0abcd"));
        assert_eq!(p.read_box_header().unwrap(),
                   Some((std::u64::MAX, *b"abcd")));
        // largesize
        let mut p = Parser::new(Cursor::new(
            b"\0\0\0\x01abcd\0\0\0\0\0\0\0\x10"));
        assert_eq!(p.read_box_header().unwrap(), Some((0, *b"abcd")));
        let mut p = Parser::new(Cursor::new(
            b"\0\0\0\x01abcd\0\0\0\0\0\0\0"));
        assert_err_pat!(p.read_box_header(), Error::Io(_));
        let mut p = Parser::new(Cursor::new(
            b"\0\0\0\x01abcd\0\0\0\0\0\0\0\x0f"));
        assert_err_pat!(p.read_box_header(), Error::InvalidFormat(_));
        // max largesize
        let mut p = Parser::new(Cursor::new(
            b"\0\0\0\x01abcd\xff\xff\xff\xff\xff\xff\xff\xff"));
        assert_eq!(p.read_box_header().unwrap(),
                   Some((std::u64::MAX.wrapping_sub(16), *b"abcd")));
    }

    #[test]
    fn is_heif_test() {
        // HEIF (with any coding format)
        assert!(is_heif(b"\0\0\0\x14ftypmif1\0\0\0\0mif1"));
        // HEIC
        assert!(is_heif(b"\0\0\0\x18ftypheic\0\0\0\0heicmif1"));
        // HEIC image sequence
        assert!(is_heif(b"\0\0\0\x18ftyphevc\0\0\0\0msf1hevc"));
        // unknown major brand but compatible with HEIF
        assert!(is_heif(b"\0\0\0\x18ftypXXXX\0\0\0\0XXXXmif1"));
        // incomplete brand (OK to ignore?)
        assert!(is_heif(b"\0\0\0\x15ftypmif1\0\0\0\0mif1h"));
        assert!(is_heif(b"\0\0\0\x16ftypmif1\0\0\0\0mif1he"));
        assert!(is_heif(b"\0\0\0\x17ftypmif1\0\0\0\0mif1hei"));
        // ISO base media file but not a HEIF
        assert!(!is_heif(b"\0\0\0\x14ftypmp41\0\0\0\0mp41"));
        // missing compatible brands (what should we do?)
        assert!(!is_heif(b"\0\0\0\x10ftypmif1\0\0\0\0"));
        // truncated box
        let mut data: &[u8] = b"\0\0\0\x14ftypmif1\0\0\0\0mif1";
        while let Some((_, rest)) = data.split_last() {
            data = rest;
            assert!(!is_heif(data));
        }
        // short box size
        assert!(!is_heif(b"\0\0\0\x13ftypmif1\0\0\0\0mif1"));
    }

    #[test]
    fn box_splitter() {
        let buf = b"0123456789abcdef";
        let mut boxp = BoxSplitter::new(buf);
        assert_err_pat!(boxp.slice(17), Error::InvalidFormat(_));
        assert_eq!(boxp.slice(16).unwrap(), buf);
        assert_err_pat!(boxp.slice(std::usize::MAX), Error::InvalidFormat(_));

        let mut boxp = BoxSplitter::new(buf);
        assert_eq!(boxp.slice(1).unwrap(), b"0");
        assert_eq!(boxp.uint16().unwrap(), 0x3132);
        assert_eq!(boxp.uint32().unwrap(), 0x33343536);
        assert_eq!(boxp.uint64().unwrap(), 0x3738396162636465);
    }
}
