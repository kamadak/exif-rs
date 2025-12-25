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

use std::io::{BufRead, ErrorKind};

use crate::error::Error;
use crate::util::{read8, read16};

pub(crate) mod marker {
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
    #[cfg(feature = "mpf")]
    pub const APP2: u8 = 0xe2;
}

// SOI marker as the JPEG header.
const JPEG_SIG: [u8; 2] = [marker::P, marker::SOI];

// Exif identifier code "Exif\0\0". [EXIF23 4.7.2]
const EXIF_ID: [u8; 6] = [0x45, 0x78, 0x69, 0x66, 0x00, 0x00];

#[cfg(feature = "mpf")]
use crate::mpf::MPF_ID;

/// Get the Exif attribute information segment from a JPEG file.
///
/// Note: When `mpf` feature is enabled, APP2 (MPF) segment is optional and may not exist.
pub fn get_exif_attr<R>(reader: &mut R)
                        -> Result<Vec<u8>, Error> where R: BufRead {
    match get_exif_attr_sub(reader) {
        Err(Error::Io(ref e)) if e.kind() == ErrorKind::UnexpectedEof =>
            Err(Error::InvalidFormat("Broken JPEG file")),
        r => r,
    }
}

fn get_exif_attr_sub<R>(reader: &mut R)
                        -> Result<Vec<u8>, Error> where R: BufRead {
    let mut soi = [0u8; 2];
    reader.read_exact(&mut soi)?;
    if soi != [marker::P, marker::SOI] {
        return Err(Error::InvalidFormat("Not a JPEG file"));
    }

    #[cfg(feature = "mpf")]
    let mut exif_data: Option<Vec<u8>> = None;

    loop {
        // Find a marker prefix.  Discard non-ff bytes, which appear if
        // we are in the scan data after SOS or we are out of sync.
        #[cfg(feature = "mpf")]
        let read_result = reader.read_until(marker::P, &mut Vec::new());
        #[cfg(not(feature = "mpf"))]
        let read_result = reader.read_until(marker::P, &mut Vec::new());

        match read_result {
            Ok(_) => {},
            Err(e) if e.kind() == ErrorKind::UnexpectedEof => {
                #[cfg(feature = "mpf")]
                {
                    // APP2 (MPF) is optional, so if we found APP1, return it
                    if let Some(exif) = exif_data {
                        return Ok(exif);
                    }
                }
                return Err(Error::Io(e));
            },
            Err(e) => return Err(Error::Io(e)),
        }

        // Get a marker code.
        let mut code;
        loop {
            match read8(reader) {
                Ok(c) => {
                    code = c;
                    if code != marker::P { break; }
                },
                Err(e) if e.kind() == ErrorKind::UnexpectedEof => {
                    #[cfg(feature = "mpf")]
                    {
                        // APP2 (MPF) is optional, so if we found APP1, return it
                        if let Some(exif) = exif_data {
                            return Ok(exif);
                        }
                    }
                    return Err(Error::Io(e));
                },
                Err(e) => return Err(Error::Io(e)),
            }
        }
        // Continue or return early on stand-alone markers.
        match code {
            marker::Z | marker::TEM | marker::RST0..=marker::RST7 => continue,
            marker::SOI => return Err(Error::InvalidFormat("Unexpected SOI")),
            #[cfg(not(feature = "mpf"))]
            marker::EOI => return Err(Error::NotFound("JPEG")),
            #[cfg(feature = "mpf")]
            marker::EOI => {
                if let Some(exif) = exif_data {
                    return Ok(exif);
                }
                return Err(Error::NotFound("JPEG"));
            },
            _ => {},
        }
        // Read marker segments.
        let len = read16(reader)?.checked_sub(2)
            .ok_or(Error::InvalidFormat("Invalid segment length"))?;
        let mut seg = vec![0; len.into()];
        match reader.read_exact(&mut seg) {
            Ok(_) => {},
            Err(e) if e.kind() == ErrorKind::UnexpectedEof => {
                #[cfg(feature = "mpf")]
                {
                    // APP2 (MPF) is optional, so if we found APP1, return it
                    if let Some(exif) = exif_data {
                        return Ok(exif);
                    }
                }
                return Err(Error::Io(e));
            },
            Err(e) => return Err(Error::Io(e)),
        }

        if code == marker::APP1 && seg.starts_with(&EXIF_ID) {
            seg.drain(..EXIF_ID.len());
            #[cfg(not(feature = "mpf"))]
            return Ok(seg);
            #[cfg(feature = "mpf")]
            {
                exif_data = Some(seg);
                // Continue to potentially find APP2 MPF (which is optional)
                continue;
            }
        }
        #[cfg(feature = "mpf")]
        if code == marker::APP2 && exif_data.is_some() && seg.starts_with(&MPF_ID) {
            // Found APP2 MPF right after APP1, return Exif data only
            // (MPF will be handled by get_exif_and_mpf_sub)
            return Ok(exif_data.unwrap());
        }
        if code == marker::SOS {
            // Skipping the scan data is handled in the main loop,
            // so there is nothing to do here.
            #[cfg(feature = "mpf")]
            if let Some(exif) = exif_data {
                return Ok(exif);
            }
        }
    }
}

#[cfg(feature = "mpf")]
/// Container for JPEG APP segments data
pub struct JpegSegments {
    /// Exif data from APP1 segment
    pub exif_data: Vec<u8>,
    /// MPF data from APP2 segment (if present)
    pub mpf_data: Option<Vec<u8>>,
    /// Absolute offset of APP2 segment start in the file (needed for MPF image offset calculation)
    pub mpf_app2_offset: u64,
}

#[cfg(feature = "mpf")]
pub(crate) fn get_exif_and_mpf_sub<R>(reader: &mut R)
                        -> Result<JpegSegments, Error> where R: BufRead {
    let mut soi = [0u8; 2];
    reader.read_exact(&mut soi)?;
    if soi != [marker::P, marker::SOI] {
        return Err(Error::InvalidFormat("Not a JPEG file"));
    }

    let mut exif_data: Option<Vec<u8>> = None;
    let mut mpf_data: Option<Vec<u8>> = None;
    let mut mpf_app2_offset: u64 = 0;
    let mut current_offset: u64 = 2; // After SOI

    loop {
        // Find a marker prefix.  Discard non-ff bytes, which appear if
        // we are in the scan data after SOS or we are out of sync.
        let mut discarded = Vec::new();
        reader.read_until(marker::P, &mut discarded)?;
        current_offset += discarded.len() as u64;

        // Get a marker code.
        let mut code;
        loop {
            code = read8(reader)?;
            current_offset += 1;
            if code != marker::P { break; }
        }
        // Continue or return early on stand-alone markers.
        match code {
            marker::Z | marker::TEM | marker::RST0..=marker::RST7 => continue,
            marker::SOI => return Err(Error::InvalidFormat("Unexpected SOI")),
            marker::EOI => {
                if let Some(exif) = exif_data {
                    return Ok(JpegSegments { exif_data: exif, mpf_data, mpf_app2_offset });
                }
                return Err(Error::NotFound("JPEG"));
            },
            _ => {},
        }
        // Read marker segments.
        let len = read16(reader)?.checked_sub(2)
            .ok_or(Error::InvalidFormat("Invalid segment length"))?;
        current_offset += 2; // length field

        let segment_start = current_offset; // Start of segment data

        let mut seg = vec![0; len.into()];
        reader.read_exact(&mut seg)?;
        current_offset += len as u64;

        if code == marker::APP1 && seg.starts_with(&EXIF_ID) {
            seg.drain(..EXIF_ID.len());
            exif_data = Some(seg);
            // Continue to look for APP2 MPF segment right after APP1
        } else if code == marker::APP2 && exif_data.is_some() && seg.starts_with(&MPF_ID) {
            // APP2 segment start (after marker and length, at MPF ID)
            mpf_app2_offset = segment_start + MPF_ID.len() as u64;
            seg.drain(..MPF_ID.len());
            mpf_data = Some(seg);
            // Found both APP1 and APP2, return immediately
            return Ok(JpegSegments { exif_data: exif_data.unwrap(), mpf_data, mpf_app2_offset });
        } else if code == marker::SOS {
            // Reached scan data, stop searching
            if let Some(exif) = exif_data {
                return Ok(JpegSegments { exif_data: exif, mpf_data, mpf_app2_offset });
            }
            return Err(Error::NotFound("JPEG"));
        }
    }
}

pub fn is_jpeg(buf: &[u8]) -> bool {
    buf.starts_with(&JPEG_SIG)
}

#[cfg(test)]
mod tests {
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
            assert_err_pat!(get_exif_attr(&mut &data[..]),
                            Error::InvalidFormat("Broken JPEG file"));
        }

        let mut data = b"\xff\xd8\xff\xe1\x00\x08Exif\0\0".to_vec();
        assert_eq!(get_exif_attr(&mut &data[..]).unwrap(), b"");
        while let Some(_) = data.pop() {
            get_exif_attr(&mut &data[..]).unwrap_err();
        }
    }

    #[test]
    fn no_exif() {
        let data = b"\xff\xd8\xff\xd9";
        assert_err_pat!(get_exif_attr(&mut &data[..]),
                        Error::NotFound(_));
    }

    #[test]
    fn out_of_sync() {
        let data = b"\xff\xd8\x01\x02\x03\xff\x00\xff\xd9";
        assert_err_pat!(get_exif_attr(&mut &data[..]),
                        Error::NotFound(_));
    }

    #[test]
    fn empty() {
        let data = b"\xff\xd8\xff\xe1\x00\x08Exif\0\0\xff\xd9";
        assert_ok!(get_exif_attr(&mut &data[..]), []);
    }

    #[test]
    fn non_empty() {
        let data = b"\xff\xd8\xff\xe1\x00\x0aExif\0\0\xbe\xad\xff\xd9";
        assert_ok!(get_exif_attr(&mut &data[..]), [0xbe, 0xad]);
    }
}
