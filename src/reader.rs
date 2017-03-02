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
use std::io::Read;
use std::mem;

use error::Error;
use jpeg;
use tiff;
use tiff::Field;

/// The `Reader` struct reads a JPEG or TIFF image,
/// parses the Exif attributes in it, and holds the results.
//
// The struct Reader is self-contained, which means that it does not
// have any external reference.  The `fields` field actually refers to
// the `buf` field.  The type system of the current Rust (as of 1.15)
// cannot represent this, so the static lifetime is used to trick it.
//
// This struct can be moved because the contents of a Vec are allocated
// in the heap and do not change their addresses by the move.
//
// The static lifetime is a lie and it must be kept secret in this struct.
// - This struct must not be destructured by the users.
// - The `fields` must be adjusted to refer the struct itself when
//   returned to the outside world.
pub struct Reader {
    // TIFF data.
    buf: Vec<u8>,
    // Exif fields.
    fields: Vec<Field<'static>>,
    // True if the TIFF data is little endian.
    little_endian: bool,
}

impl Reader {
    /// Reads a JPEG or TIFF image and parses the Exif attributes in it.
    /// If an error occurred, `exif::Error` is returned.
    pub fn new<R>(mut reader: &mut R)
                  -> Result<Reader, Error> where R: io::BufRead {
        let mut buf = Vec::new();
        try!(reader.by_ref().take(4).read_to_end(&mut buf));
        if jpeg::is_jpeg(&buf) {
            let exif_buf = try!(jpeg::get_exif_attr(
                &mut buf.as_mut_slice().chain(reader)));
            buf = exif_buf;
        } else if tiff::is_tiff(&buf) {
            try!(reader.read_to_end(&mut buf));
        } else {
            return Err(Error::InvalidFormat("Unknown image format"));
        }

        // Cheat on the type system and erase the lifetime by transmute().
        // The scope releases the innter `v` to unborrow `buf`.
        let (v, le) = {
            let (v, le) = try!(tiff::parse_exif(&buf));
            (unsafe { mem::transmute(v) }, le) };
        let r = Reader { buf: buf, fields: v, little_endian: le };
        Ok(r)
    }

    /// Returns the slice that contains the TIFF data.
    #[inline]
    pub fn buf(&self) -> &[u8] {
        &self.buf[..]
    }

    /// Returns the reference to the vector of Exif fields.
    #[inline]
    pub fn fields<'a>(&'a self) -> &Vec<Field<'a>> {
        &self.fields
    }

    /// Returns true if the TIFF data is in the little-endian byte order.
    #[inline]
    pub fn little_endian(&self) -> bool {
        self.little_endian
    }
}

#[cfg(test)]
mod tests {
    use std::io::BufReader;
    use tiff::Field;
    use value::Value;
    use super::*;

    static TIFF_ASCII: &'static [u8] =
        b"MM\0\x2a\0\0\0\x08\0\x01\x01\x0e\0\x02\0\0\0\x04ABC\0\0\0\0\0";

    // Test if moving a `Reader` does not invalidate the references in it.
    // The referer is in the heap and does not move, so this test is not
    // so meaningful.
    #[test]
    fn move_reader() {
        let r1 = Reader::new(&mut BufReader::new(TIFF_ASCII)).unwrap();
        let ptr = &r1 as *const _;
        move_in_and_drop(r1, ptr);

        let (r2, ptr) = move_out_and_drop();
        assert!(ptr != &r2 as *const _, "not moved");
        check_abc(r2.fields());

        box_and_drop();
    }

    #[inline(never)]
    fn move_in_and_drop(r1: Reader, ptr: *const Reader) {
        assert!(ptr != &r1 as *const _, "not moved");
        check_abc(r1.fields());
    }

    #[inline(never)]
    fn move_out_and_drop() -> (Reader, *const Reader) {
        let r2 = Reader::new(&mut BufReader::new(TIFF_ASCII)).unwrap();
        let ptr = &r2 as *const _;
        (r2, ptr)
    }

    fn box_and_drop() {
        let r = Reader::new(&mut BufReader::new(TIFF_ASCII)).unwrap();
        let ptr = &r as *const _;
        let b = Box::new(r);
        assert!(ptr != &*b as *const _, "not moved");
        check_abc(b.fields());
    }

    fn check_abc(fields: &Vec<Field>) {
        if let Value::Ascii(ref v) = fields[0].value {
            assert_eq!(*v, vec![b"ABC"]);
        } else {
            panic!("TIFF ASCII field is expected");
        }
    }
}
