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

use std::io::{BufRead, ErrorKind, Read};

use crate::endian::{BigEndian, Endian};
use crate::error::Error;
use crate::util::{BufReadExt as _, ReadExt as _};

/// PNG file signature [PNG12 12.12].
const PNG_SIG: [u8; 8] = *b"\x89PNG\x0d\x0a\x1a\x0a";
/// The four-byte chunk type for Exif data.
const EXIF_CHUNK_TYPE: [u8; 4] = *b"eXIf";
/// The four-byte chunk type for zTXt data, which may embed EXIF.
const ZTXT_CHUNK_TYPE: [u8; 4] = *b"zTXt";

// Get the contents of the EXIF chunk (eXIF or zTXt) from a PNG file.
pub fn get_exif_attr<R>(reader: &mut R) -> Result<Vec<u8>, Error>
where
    R: BufRead,
{
    match get_exif_attr_sub(reader) {
        Err(Error::Io(ref e)) if e.kind() == ErrorKind::UnexpectedEof => {
            Err(Error::InvalidFormat("Broken PNG file"))
        }
        r => r,
    }
}

// The location of the eXIf chunk is restricted [PNGEXT150 3.7], but this
// reader is liberal about it.
fn get_exif_attr_sub<R>(reader: &mut R) -> Result<Vec<u8>, Error>
where
    R: BufRead,
{
    let mut sig = [0u8; 8];
    reader.read_exact(&mut sig)?;
    if sig != PNG_SIG {
        return Err(Error::InvalidFormat("Not a PNG file"));
    }
    // Scan the series of chunks.
    loop {
        if reader.is_eof()? {
            return Err(Error::NotFound("PNG"));
        }
        let mut lenbuf = [0; 4];
        reader.read_exact(&mut lenbuf)?;
        let len = BigEndian::loadu32(&lenbuf, 0) as usize;
        let mut ctype = [0u8; 4];
        reader.read_exact(&mut ctype)?;
        if ctype == EXIF_CHUNK_TYPE {
            let mut data = Vec::with_capacity(len);
            reader.read_exact_len(&mut data, len)?;
            return Ok(data);
        } else if ctype == ZTXT_CHUNK_TYPE {
            // Given a zTXt chunk, see if it decodes as an EXIF blob.
            //
            // As described in [1] for the Exiv2 project, in PNGs,
            // > Exif metadata can [be] either:
            // > a) Stored compressed in a zTXt chunk with the signature `Raw profile type exif\0\0` (This is supported in Exiv2)
            // > b) Stored in a eXIfchunk as a decompressed Tiff file (This is not supported in Exiv2)
            //
            // If the PNG was output via Exiv2 (i.e. in format (a)), we need to check for the chunk signature,
            // then decompress.
            //
            // [1]: https://github.com/Exiv2/exiv2/issues/1343#issuecomment-705568612
            // Read out the header to check:
            const EXIF_SIGNATURE: &[u8] = b"Raw profile type exif\0\0";
            let mut header_buf = Vec::with_capacity(EXIF_SIGNATURE.len());
            reader.read_exact_len(&mut header_buf, EXIF_SIGNATURE.len())?;
            let len = len - EXIF_SIGNATURE.len();
            if header_buf == EXIF_SIGNATURE {
                // Found it! Get the chunk.
                let mut data = Vec::with_capacity(len + 4);
                // "PNG is always bigEndian encoded": https://clanmills.com/exiv2/book/#PNG
                reader.read_exact_len(&mut data, len)?;
                if let Ok(_s) = std::str::from_utf8(&data) {
                    panic!("it's a string!")
                }
                return extract_exif_from_ztxt(&data);
            } else {
                // We've read the header; discard the remaining portion.
                // Chunk data and CRC.
                reader.discard_exact(
                    len.checked_add(4)
                        .ok_or(Error::InvalidFormat("Invalid chunk length"))?,
                )?;
                continue;
            }
        }
        // Chunk data and CRC.
        reader.discard_exact(
            len.checked_add(4)
                .ok_or(Error::InvalidFormat("Invalid chunk length"))?,
        )?;
    }
}

fn extract_exif_from_ztxt(bytes: &[u8]) -> Result<Vec<u8>, Error> {
    // From Exiv2 documentation, https://clanmills.com/exiv2/book/#PNG,
    // in the context of a zTXt block:
    // > The signatures: “Raw profile type iptc” and “Raw profile type exif” introduce a compressed block
    // > which when expanded is an ascii string with the following format.
    // > The number is count of hex code bytes. This is redundant and does not need to be decoded.
    // > ```
    // > \n
    // > exif\n
    // >     number\n
    // > hexEncodedBinary\n
    // > ...
    // > ```
    //
    // More accurately, the hexEncodedBinary lines appear to be repeated.

    // So... let's get started with that.
    //
    // https://www.libpng.org/pub/png/spec/1.2/PNG-Compression.html
    // > Deflate-compressed datastreams within PNG are stored in the "zlib" format, which has the structure:
    let mut dec = flate2::bufread::ZlibDecoder::new(std::io::Cursor::new(bytes));
    let mut decompressed = Vec::new();
    dec.read_to_end(&mut decompressed)?;
    let s = std::str::from_utf8(&decompressed)
        .map_err(|_e| Error::InvalidFormat("PNG zTXt-embedded EXIF is not ASCII"))?;
    let items = s.split("\n");
    // Strip the header lines
    let initial: Vec<_> = items.clone().take(3).collect();
    if initial.len() != 3 || !initial[0].is_empty() || initial[1] != "exif" {
        return Err(Error::InvalidFormat(
            "PNG zTXt-embedded EXIF contains invalid internal structure",
        ));
    }
    let Ok(len) = initial[2].trim().parse::<usize>() else {
        return Err(Error::InvalidFormat(
            "PNG zTXt-embedded EXIF contains an invalid length",
        ));
    };
    let mut data = items
        .skip(3)
        .map(|line| {
            hex::decode(line).map_err(|e| {
                eprintln!("{e}");
                Error::InvalidFormat("PNG zTXt-embedded EXIF contains invalid hex encoding")
            })
        })
        .try_fold(Vec::new(), |mut acc, maybe| -> Result<Vec<u8>, Error> {
            let mut chunk = maybe?;
            acc.append(&mut chunk);
            Ok(acc)
        })?;
    data.truncate(len);
    // The spec for the eXIf PNG segment says [2]:
    //
    // > The data segment of the eXIf chunk contains an Exif profile in the format
    // > specified in "4.7.2 Interoperability Structure of APP1 in Compressed Data"
    // > of [CIPA DC-008-2016] except that the JPEG APP1 marker, length, and the
    // > "Exif ID code" described in 4.7.2(C), i.e., "Exif", NULL, and padding byte,
    // > are not included.
    //
    // [2]: http://www.simplesystems.org/png-group/proposals/eXIf/history/png-proposed-eXIf-chunk-2017-06-08a.html
    //
    // And yes, 4.7.2 (I checked a later version of Exif) says there is an "Exif\0\0" prefix in the
    // marker section; that's what I see in exports from Exiv2.
    // To be compatible with the rest of exif-rs, we strip that marker here.
    if let Some(remainder) = data.strip_prefix(b"Exif\0") {
        Ok(remainder[1..].to_owned())
    } else {
        Ok(data)
    }
}

pub fn is_png(buf: &[u8]) -> bool {
    buf.starts_with(&PNG_SIG)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn truncated() {
        let sets: &[&[u8]] = &[b"", b"\x89", b"\x89PNG\x0d\x0a\x1a"];
        for &data in sets {
            assert_err_pat!(
                get_exif_attr(&mut &data[..]),
                Error::InvalidFormat("Broken PNG file")
            );
        }

        let mut data = b"\x89PNG\x0d\x0a\x1a\x0a\0\0\0\x04eXIfExif".to_vec();
        assert_eq!(get_exif_attr(&mut &data[..]).unwrap(), b"Exif");
        while let Some(_) = data.pop() {
            get_exif_attr(&mut &data[..]).unwrap_err();
        }
    }

    #[test]
    fn no_exif() {
        let data = b"\x89PNG\x0d\x0a\x1a\x0a";
        assert_err_pat!(get_exif_attr(&mut &data[..]), Error::NotFound(_));
    }

    #[test]
    fn empty() {
        let data = b"\x89PNG\x0d\x0a\x1a\x0a\0\0\0\0eXIfCRC_";
        assert_ok!(get_exif_attr(&mut &data[..]), []);
    }

    #[test]
    fn non_empty() {
        let data = b"\x89PNG\x0d\x0a\x1a\x0a\0\0\0\x02eXIf\xbe\xadCRC_";
        assert_ok!(get_exif_attr(&mut &data[..]), [0xbe, 0xad]);
    }
}
