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

use std::io;
use std::io::Read;

use error::Error;
use util::read8;
use util::read16;

mod marker {
    // The first byte of a marker.
    pub const P:    u8 = 0xff;
    // Marker codes.
    pub const Z:    u8 = 0x00;		// Not a marker but a byte stuffing.
    pub const TEM:  u8 = 0x01;
    pub const RST0: u8 = 0xd0;
    pub const RST7: u8 = 0xd7;
    pub const SOI:  u8 = 0xd8;
    pub const EOI:  u8 = 0xd9;
    pub const SOS:  u8 = 0xda;
    pub const APP1: u8 = 0xe1;
}

// SOI marker as the JPEG header.
const JPEG_SIG: [u8; 2] = [marker::P, marker::SOI];

// Exif identifier code "Exif\0\0". [EXIF23 4.7.2]
const EXIF_ID: [u8; 6] = [0x45, 0x78, 0x69, 0x66, 0x00, 0x00];

/// Get the Exif attribute information segment from a JPEG file.
pub fn get_exif_attr<R>(reader: &mut R)
                        -> Result<Vec<u8>, Error> where R: io::BufRead {
    match get_exif_attr_sub(reader) {
        Err(Error::Io(ref e)) if e.kind() == io::ErrorKind::UnexpectedEof =>
            Err(Error::InvalidFormat("Broken JPEG file")),
        r => r,
    }
}

fn get_exif_attr_sub<R>(reader: &mut R)
                        -> Result<Vec<u8>, Error> where R: io::BufRead {
    let mut soi = [0u8; 2];
    try!(reader.read_exact(&mut soi));
    if soi != [marker::P, marker::SOI] {
        return Err(Error::InvalidFormat("Not a JPEG file"));
    }
    loop {
        // Find a marker prefix.  Discard non-ff bytes, which appear if
        // we are in the scan data after SOS or we are out of sync.
        try!(reader.read_until(marker::P, &mut Vec::new()));
        // Get a marker code.
        let mut code;
        loop {
            code = try!(read8(reader));
            if code != marker::P { break; }
        }
        // Continue or return early on stand-alone markers.
        match code {
            marker::Z | marker::TEM | marker::RST0...marker::RST7 => continue,
            marker::SOI => return Err(Error::InvalidFormat("Unexpected SOI")),
            marker::EOI => return Err(Error::NotFound("No Exif data found")),
            _ => {},
        }
        // Read marker segments.
        let seglen = try!(read16(reader));
        if seglen < 2 {
            return Err(Error::InvalidFormat("Invalid segment length"));
        }
        let mut seg = Vec::new();
        try!(reader.by_ref().take(seglen as u64 - 2).read_to_end(&mut seg));
        if code == marker::APP1 && seg.starts_with(&EXIF_ID) {
            return Ok(seg.split_off(EXIF_ID.len()));
        }
        if code == marker::SOS {
            // Skipping the scan data is handled in the main loop,
            // so there is nothing to do here.
        }
    }
}

pub fn is_jpeg(buf: &[u8]) -> bool {
    buf.starts_with(&JPEG_SIG)
}

#[cfg(test)]
mod tests {
    use std::io::Cursor;
    use super::*;

    #[test]
    fn truncated() {
        let sets: &[&[u8]] = &[
            b"",
            b"\xff",
            b"\xff\xd8",
            b"\xff\xd8\x00",
            b"\xff\xd8\xff",
            b"\xff\xd8\xff\xe1\x00\x08\x03\x04",
        ];
        for &data in sets {
            assert_err_pat!(get_exif_attr(&mut Cursor::new(data)),
                            Error::InvalidFormat("Broken JPEG file"));
        }
    }

    #[test]
    fn no_exif() {
        let data = b"\xff\xd8\xff\xd9";
        assert_err_pat!(get_exif_attr(&mut Cursor::new(data)),
                        Error::NotFound(_));
    }

    #[test]
    fn out_of_sync() {
        let data = b"\xff\xd8\x01\x02\x03\xff\x00\xff\xd9";
        assert_err_pat!(get_exif_attr(&mut Cursor::new(data)),
                        Error::NotFound(_));
    }

    #[test]
    fn empty() {
        let data = b"\xff\xd8\xff\xe1\x00\x08Exif\0\0\xff\xd9";
        assert_ok!(get_exif_attr(&mut Cursor::new(data)), []);
    }

    #[test]
    fn non_empty() {
        let data = b"\xff\xd8\xff\xe1\x00\x0aExif\0\0\xbe\xad\xff\xd9";
        assert_ok!(get_exif_attr(&mut Cursor::new(data)), [0xbe, 0xad]);
    }
}
