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
use std::mem;

use error::Error;
use jpeg;
use tiff;
use tiff::Field;

/// Parse the Exif attributes in a JPEG or TIFF image data.
///
/// Returns a Vec of Exif fields and a bool.
/// The boolean value is true if the data is little endian.
/// If an error occurred, `exif::Error` is returned.
///
/// The `buf` must be an empty `Vec<u8>` when this function is called.
/// The raw Exif data is read into it.
pub fn parse_image<'a, R>(mut reader: &mut R, mut buf: &'a mut Vec<u8>)
                          -> Result<(Vec<Field<'a>>, bool), Error>
    where R: io::BufRead
{
    try!(reader.by_ref().take(4).read_to_end(buf));
    if jpeg::is_jpeg(buf) {
        let exif_buf = try!(jpeg::get_exif_attr(
            &mut buf.as_mut_slice().chain(reader)));
        mem::replace(buf, exif_buf);
    } else if tiff::is_tiff(buf) {
        try!(reader.read_to_end(&mut buf));
    } else {
        return Err(Error::InvalidFormat("Unknown image format"));
    }
    tiff::parse_exif(buf)
}
