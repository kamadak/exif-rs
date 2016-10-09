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

pub fn read8<R>(reader: &mut R) -> Result<u8, io::Error> where R: io::Read {
    let mut buf: [u8; 1] = unsafe { ::std::mem::uninitialized() };
    reader.read_exact(&mut buf).and(Ok(buf[0]))
}

pub fn read16<R>(reader: &mut R) -> Result<u16, io::Error> where R: io::Read {
    let mut buf: [u8; 2] = unsafe { ::std::mem::uninitialized() };
    try!(reader.read_exact(&mut buf));
    Ok(((buf[0] as u16) << 8) + buf[1] as u16)
}

#[cfg(test)]
mod tests {
    use std::io::Cursor;
    use std::io::ErrorKind;
    use std::io::Read;
    use super::*;

    #[test]
    fn read8_len() {
        let mut reader = Cursor::new([]);
        assert_err_kind!(read8(&mut reader), ErrorKind::UnexpectedEof);
        let mut reader = Cursor::new([0x01]);
        assert_ok!(read8(&mut reader), 0x01);
        let mut reader = Cursor::new([0x01, 0x02]);
        let mut buf = Vec::new();
        assert_ok!(read8(&mut reader), 0x01);
        assert_ok!(reader.read_to_end(&mut buf), 1);
        assert_eq!(buf, [0x02]);
    }

    #[test]
    fn read16_len() {
        let mut reader = Cursor::new([]);
        assert_err_kind!(read16(&mut reader), ErrorKind::UnexpectedEof);
        let mut reader = Cursor::new([0x01]);
        assert_err_kind!(read16(&mut reader), ErrorKind::UnexpectedEof);
        let mut reader = Cursor::new([0x01, 0x02]);
        assert_ok!(read16(&mut reader), 0x0102);
        let mut reader = Cursor::new([0x01, 0x02, 0x03]);
        let mut buf = Vec::new();
        assert_ok!(read16(&mut reader), 0x0102);
        assert_ok!(reader.read_to_end(&mut buf), 1);
        assert_eq!(buf, [0x03]);
    }
}
