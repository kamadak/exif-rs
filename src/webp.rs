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

use std::io::{BufRead, ErrorKind};

use crate::endian::{Endian, LittleEndian};
use crate::error::Error;
use crate::util::{BufReadExt as _, ReadExt as _};

// Chunk identifiers for RIFF.
const FCC_RIFF: [u8; 4] = *b"RIFF";
const FCC_WEBP: [u8; 4] = *b"WEBP";
const FCC_EXIF: [u8; 4] = *b"EXIF";

// Get the contents of the Exif chunk from a WebP file.
pub fn get_exif_attr<R>(reader: &mut R)
                        -> Result<Vec<u8>, Error> where R: BufRead {
    match get_exif_attr_sub(reader) {
        Err(Error::Io(ref e)) if e.kind() == ErrorKind::UnexpectedEof =>
            Err(Error::InvalidFormat("Broken WebP file")),
        r => r,
    }
}

fn get_exif_attr_sub<R>(reader: &mut R)
                        -> Result<Vec<u8>, Error> where R: BufRead {
    let mut sig = [0; 12];
    reader.read_exact(&mut sig)?;
    if sig[0..4] != FCC_RIFF || sig[8..12] != FCC_WEBP {
        return Err(Error::InvalidFormat("Not a WebP file"));
    }
    let mut file_size = LittleEndian::loadu32(&sig, 4) as usize;
    file_size = file_size.checked_sub(4)
        .ok_or(Error::InvalidFormat("Invalid header file size"))?;

    // Scan the series of chunks.
    while file_size > 0 {
        file_size = file_size.checked_sub(8)
            .ok_or(Error::InvalidFormat("Chunk overflowing parent"))?;
        let mut cheader = [0; 8];
        reader.read_exact(&mut cheader)?;
        let mut size = LittleEndian::loadu32(&cheader, 4) as usize;
        file_size = file_size.checked_sub(size)
            .ok_or(Error::InvalidFormat("Chunk overflowing parent"))?;
        if cheader[0..4] == FCC_EXIF {
            let mut payload = Vec::new();
            reader.read_exact_len(&mut payload, size)?;
            return Ok(payload);
        }
        if size % 2 != 0 && file_size > 0 {
            file_size -= 1;
            size = size.checked_add(1).expect("ex-file_size - size > 0");
        }
        reader.discard_exact(size)?;
    }
    Err(Error::NotFound("WebP"))
}

pub fn is_webp(buf: &[u8]) -> bool {
    buf.len() >= 12 && buf[0..4] == FCC_RIFF && buf[8..12] == FCC_WEBP
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn truncated() {
        let mut data = b"RIFF\x10\0\0\0WEBPEXIF\x04\0\0\0Exif".to_vec();
        assert_eq!(get_exif_attr(&mut &data[..]).unwrap(), b"Exif");
        while let Some(_) = data.pop() {
            get_exif_attr(&mut &data[..]).unwrap_err();
        }
    }

    #[test]
    fn no_exif() {
        let data = b"RIFF\x0c\0\0\0WEBPwhat\0\0\0\0";
        assert_err_pat!(get_exif_attr(&mut &data[..]), Error::NotFound(_));
    }

    #[test]
    fn empty() {
        let data = b"RIFF\x16\0\0\0WEBPodd_\x01\0\0\0X\0EXIF\0\0\0\0";
        assert_ok!(get_exif_attr(&mut &data[..]), b"");
    }

    #[test]
    fn non_empty() {
        let data = b"RIFF\x1a\0\0\0WEBPeven\x02\0\0\0XYEXIF\x03\0\0\0abcd";
        assert_ok!(get_exif_attr(&mut &data[..]), b"abc");
    }

    #[test]
    fn read_first() {
        let data = b"RIFF\x18\0\0\0WEBPEXIF\x02\0\0\0abEXIF\x02\0\0\0cd";
        assert_ok!(get_exif_attr(&mut &data[..]), b"ab");
    }

    #[test]
    fn out_of_toplevel_chunk() {
        let data = b"RIFF\x0e\0\0\0WEBPwhat\x02\0\0\0abEXIF\x02\0\0\0cd";
        assert_err_pat!(get_exif_attr(&mut &data[..]), Error::NotFound(_));
    }

    #[test]
    fn overflowing_parent() {
        let mut data = b"RIFF\x10\0\0\0WEBPEXIF\x04\0\0\0Exif".to_vec();
        assert_eq!(get_exif_attr(&mut &data[..]).unwrap(), b"Exif");
        for x in 0x05..=0x0f {
            data[4] = x;
            assert_err_pat!(get_exif_attr(&mut &data[..]),
                            Error::InvalidFormat(_));
        }
        data[4] = 0x04;
        assert_err_pat!(get_exif_attr(&mut &data[..]), Error::NotFound(_));
    }

    #[test]
    fn odd_at_last_without_padding() {
        let data = b"RIFF\x17\0\0\0WEBPwhat\0\0\0\0EXIF\x03\0\0\0abc";
        assert_ok!(get_exif_attr(&mut &data[..]), b"abc");
    }
}
