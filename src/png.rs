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

use std::io;
use std::io::Read;

use crate::endian::{Endian, BigEndian};
use crate::error::Error;
use crate::util::BufReadExt;

// PNG file signature [PNG12 12.12].
const PNG_SIG: [u8; 8] = *b"\x89PNG\x0d\x0a\x1a\x0a";
// The four-byte chunk type for Exif data.
const EXIF_CHUNK_TYPE: [u8; 4] = *b"eXIf";

// Get the contents of the eXIf chunk from a PNG file.
pub fn get_exif_attr<R>(reader: &mut R)
                        -> Result<Vec<u8>, Error> where R: io::BufRead {
    match get_exif_attr_sub(reader) {
        Err(Error::Io(ref e)) if e.kind() == io::ErrorKind::UnexpectedEof =>
            Err(Error::InvalidFormat("Broken PNG file")),
        r => r,
    }
}

// The location of the eXIf chunk is restricted [PNGEXT150 3.7], but this
// reader is liberal about it.
fn get_exif_attr_sub<R>(reader: &mut R)
                        -> Result<Vec<u8>, Error> where R: io::BufRead {
    let mut sig = [0u8; 8];
    reader.read_exact(&mut sig)?;
    if sig != PNG_SIG {
        return Err(Error::InvalidFormat("Not a PNG file"));
    }
    // Scan the series of chunks.
    loop {
        let mut lenbuf = Vec::new();
        match reader.by_ref().take(4).read_to_end(&mut lenbuf)? {
            0 => return Err(Error::NotFound("PNG")),
            1..=3 => return Err(io::Error::new(io::ErrorKind::UnexpectedEof,
                                               "truncated chunk").into()),
            _ => {},
        }
        let len = BigEndian::loadu32(&lenbuf, 0) as usize;
        let mut ctype = [0u8; 4];
        reader.read_exact(&mut ctype)?;
        if ctype == EXIF_CHUNK_TYPE {
            let mut data = Vec::new();
            reader.by_ref().take(len as u64).read_to_end(&mut data)?;
            if data.len() != len {
                return Err(io::Error::new(io::ErrorKind::UnexpectedEof,
                                          "truncated chunk").into());
            }
            return Ok(data);
        }
        // Chunk data and CRC.
        reader.discard_exact(len + 4)?;
    }
}

pub fn is_png(buf: &[u8]) -> bool {
    buf.starts_with(&PNG_SIG)
}

#[cfg(test)]
mod tests {
    use std::io::Cursor;
    use super::*;

    #[test]
    fn truncated() {
        let sets: &[&[u8]] = &[
            b"",
            b"\x89",
            b"\x89PNG\x0d\x0a\x1a",
        ];
        for &data in sets {
            assert_err_pat!(get_exif_attr(&mut Cursor::new(data)),
                            Error::InvalidFormat("Broken PNG file"));
        }

        let mut data = b"\x89PNG\x0d\x0a\x1a\x0a\0\0\0\x04eXIfExif".to_vec();
        get_exif_attr(&mut Cursor::new(&data)).unwrap();
        while let Some(_) = data.pop() {
            get_exif_attr(&mut &data[..]).unwrap_err();
        }
    }

    #[test]
    fn no_exif() {
        let data = b"\x89PNG\x0d\x0a\x1a\x0a";
        assert_err_pat!(get_exif_attr(&mut Cursor::new(data)),
                        Error::NotFound(_));
    }

    #[test]
    fn empty() {
        let data = b"\x89PNG\x0d\x0a\x1a\x0a\0\0\0\0eXIfCRC_";
        assert_ok!(get_exif_attr(&mut Cursor::new(data)), []);
    }

    #[test]
    fn non_empty() {
        let data = b"\x89PNG\x0d\x0a\x1a\x0a\0\0\0\x02eXIf\xbe\xadCRC_";
        assert_ok!(get_exif_attr(&mut Cursor::new(data)), [0xbe, 0xad]);
    }
}
