//
// Copyright (c) 2020 KAMADA Ken'ichi.
// All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions
// are met:
// 1. Redistributions of source code must retain the above copyright notice,
//    this list of conditions and the following disclaimer.
// 2. Redistributions in binary form must reproduce the above copyright notice,
//    this list of conditions and the following disclaimer in the documentation
//    and/or other materials provided with the distribution.
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

use crate::endian::{BigEndian, Endian};
use crate::error::Error;
use crate::isobmff::BoxSplitter;
use crate::util::{read64, BufReadExt as _, ReadExt as _};

// Checking "mif1" in the compatible brands should be enough, because
// the "heic", "heix", "heim", and "heis" files shall include "mif1"
// among the compatible brands [ISO23008-12 B.4.1] [ISO23008-12 B.4.3].
// Same for "msf1" [ISO23008-12 B.4.2] [ISO23008-12 B.4.4].
static CANON_FORMATS: &[[u8; 4]] = &[*b"crx "];

pub fn get_exif_attr<R>(reader: &mut R) -> Result<Vec<u8>, Error>
where
    R: BufRead + Seek,
{
    let mut parser = Parser::new(reader);
    match parser.parse() {
        Err(Error::Io(ref e)) if e.kind() == ErrorKind::UnexpectedEof => Err("Broken CR3 file".into()),
        Err(e) => Err(e),
        Ok(buf) => Ok(buf.first().cloned().unwrap_or_else(|| Vec::new())),
    }
}

#[derive(Debug)]
struct Parser<R> {
    reader: R,
    // Whether the file type box has been checked.
    ftyp_checked: bool,
}

impl<R> Parser<R>
where
    R: BufRead + Seek,
{
    fn new(reader: R) -> Self { Self { reader, ftyp_checked: false } }

    /// Extracts the Exif attributes from raw Exif data for CR3 the result are 4 non-contiguous buffer segments
    /// If an error occurred, `exif::Error` is returned.
    fn parse(&mut self) -> Result<Vec<Vec<u8>>, Error> {
        while let Some((size, boxtype)) = self.read_box_header()? {
            match &boxtype {
                b"ftyp" => {
                    let buf = self.read_file_level_box(size)?;
                    self.parse_ftyp(BoxSplitter::new(&buf))?;
                    self.ftyp_checked = true;
                }
                b"moov" => {
                    if !self.ftyp_checked {
                        return Err("moov found before FileTypeBox".into());
                    }
                    let buf = self.read_file_level_box(size)?;
                    let mut out_buf = Vec::new();
                    self.parse_moov(&mut out_buf, BoxSplitter::new(&buf))?;
                    if !out_buf.is_empty() {
                        return Ok(out_buf);
                    }
                }
                _ => {
                    self.skip_file_level_box(size)?;
                }
            }
        }
        Err(Error::NotFound("CR3"))
    }

    fn parse_moov(&mut self, out_data: &mut Vec<Vec<u8>>, mut split: BoxSplitter) -> Result<(), Error> {
        //let indent = indent + 1;
        while let Ok((boxtype, mut boxbody)) = split.child_box() {
            let size = boxbody.len();
            // println!(
            // 	"{}{}{}{}{} {}",
            // 	" ".repeat(4), boxtype[0] as char, boxtype[1] as char, boxtype[2] as char, boxtype[3] as char, size
            // );
            match boxtype {
                b"uuid" => {
                    let uuid = boxbody.slice(16)?;
                    let canon_uuid = &[0x85, 0xc0, 0xb6, 0x87, 0x82, 0x0f, 0x11, 0xe0, 0x81, 0x11, 0xf4, 0xce, 0x46, 0x2b, 0x6a, 0x48];
                    if uuid == canon_uuid {
                        self.parse_moov(out_data, boxbody)?;
                    }
                }
                b"CMT1" => {
                    // possibly metadata for embedded JPG
                    out_data.push(Vec::from(boxbody.slice(size)?));
                }
                _ => {
                    // b"CMT2" | b"CMT3" | b"CMT4"  also contain useful data but the Tiff tag is incorrect
                    boxbody.slice(size)?;
                }
            }
        }
        return Ok(());
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
        }
            .ok_or("Invalid box size")?;
        let boxtype = buf[4..8].try_into().expect("never fails");
        Ok(Some((size, boxtype)))
    }

    fn read_file_level_box(&mut self, size: u64) -> Result<Vec<u8>, Error> {
        let mut buf;
        match size {
            std::u64::MAX => {
                buf = Vec::new();
                self.reader.read_to_end(&mut buf)?;
            }
            _ => {
                let size = size.try_into().or(Err("Box is larger than the address space"))?;
                buf = Vec::new();
                self.reader.read_exact_len(&mut buf, size)?;
            }
        }
        Ok(buf)
    }

    fn skip_file_level_box(&mut self, size: u64) -> Result<(), Error> {
        match size {
            std::u64::MAX => self.reader.seek(SeekFrom::End(0))?,
            _ => self.reader.seek(SeekFrom::Current(size.try_into().or(Err("Large seek not supported"))?))?,
        };
        Ok(())
    }

    fn parse_ftyp(&mut self, mut boxp: BoxSplitter) -> Result<(), Error> {
        let head = boxp.slice(8)?;
        let _major_brand = &head[0..4];
        let _minor_version = BigEndian::loadu32(&head, 4);
        while let Ok(compat_brand) = boxp.array4() {
            if CANON_FORMATS.contains(&compat_brand) {
                return Ok(());
            }
        }
        Err("No compatible brand recognized in ISO base media file".into())
    }
}

pub fn is_crx(buf: &[u8]) -> bool {
    let mut boxp = BoxSplitter::new(buf);
    while let Ok((boxtype, mut body)) = boxp.child_box() {
        if boxtype == b"ftyp" {
            let _major_brand_minor_version = if body.slice(8).is_err() {
                return false;
            };
            while let Ok(compat_brand) = body.array4() {
                if CANON_FORMATS.contains(&compat_brand) {
                    return true;
                }
            }
            return false;
        }
    }
    false
}