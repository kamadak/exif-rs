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
use std::mem;

// This is a module to select endianess by using generics
// in order to avoid run-time dispatching penalty at the cost of
// increased object size.

pub trait Endian {
    fn loadu16(buf: &[u8], from: usize) -> u16;
    fn loadu32(buf: &[u8], from: usize) -> u32;
    fn loadu64(buf: &[u8], from: usize) -> u64;
    fn writeu16<W>(w: &mut W, num: u16) -> io::Result<()> where W: io::Write;
    fn writeu32<W>(w: &mut W, num: u32) -> io::Result<()> where W: io::Write;
    fn writeu64<W>(w: &mut W, num: u64) -> io::Result<()> where W: io::Write;
}

pub struct BigEndian;
pub struct LittleEndian;

macro_rules! generate_load {
    ($name:ident, $int_type:ident, $from_func:ident) => (
        fn $name(buf: &[u8], offset: usize) -> $int_type {
            let mut num = [0u8; mem::size_of::<$int_type>()];
            num.copy_from_slice(
                &buf[offset .. offset + mem::size_of::<$int_type>()]);
            $int_type::$from_func(num)
        }
    )
}

macro_rules! generate_write {
    ($name:ident, $int_type:ident, $to_func:ident) => (
        fn $name<W>(w: &mut W, num: $int_type)
                    -> io::Result<()> where W: io::Write {
            let buf = num.$to_func();
            w.write_all(&buf)
        }
    )
}

impl Endian for BigEndian {
    generate_load!(loadu16, u16, from_be_bytes);
    generate_load!(loadu32, u32, from_be_bytes);
    generate_load!(loadu64, u64, from_be_bytes);
    generate_write!(writeu16, u16, to_be_bytes);
    generate_write!(writeu32, u32, to_be_bytes);
    generate_write!(writeu64, u64, to_be_bytes);
}

impl Endian for LittleEndian {
    generate_load!(loadu16, u16, from_le_bytes);
    generate_load!(loadu32, u32, from_le_bytes);
    generate_load!(loadu64, u64, from_le_bytes);
    generate_write!(writeu16, u16, to_le_bytes);
    generate_write!(writeu32, u32, to_le_bytes);
    generate_write!(writeu64, u64, to_le_bytes);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn loadu16() {
        assert_eq!(BigEndian::loadu16(&[0x01, 0x02], 0), 0x0102);
        assert_eq!(BigEndian::loadu16(&[0x01, 0x02, 0x03], 1), 0x0203);
        assert_eq!(LittleEndian::loadu16(&[0x01, 0x02], 0), 0x0201);
        assert_eq!(LittleEndian::loadu16(&[0x01, 0x02, 0x03], 1), 0x0302);
    }

    #[test]
    fn loadu32() {
        assert_eq!(BigEndian::loadu32(&[0x01, 0x02, 0x03, 0x04], 0),
                   0x01020304);
        assert_eq!(BigEndian::loadu32(&[0x01, 0x02, 0x03, 0x04, 0x05], 1),
                   0x02030405);
        assert_eq!(LittleEndian::loadu32(&[0x01, 0x02, 0x03, 0x04], 0),
                   0x04030201);
        assert_eq!(LittleEndian::loadu32(&[0x01, 0x02, 0x03, 0x04, 0x05], 1),
                   0x05040302);
    }

    #[test]
    fn loadu64() {
        assert_eq!(BigEndian::loadu64(&[0x01, 0x02, 0x03, 0x04,
                                        0x05, 0x06, 0x07, 0x08], 0),
                   0x0102030405060708);
        assert_eq!(BigEndian::loadu64(&[0x01, 0x02, 0x03, 0x04, 0x05,
                                        0x06, 0x07, 0x08, 0x09], 1),
                   0x0203040506070809);
        assert_eq!(LittleEndian::loadu64(&[0x01, 0x02, 0x03, 0x04,
                                           0x05, 0x06, 0x07, 0x08], 0),
                   0x0807060504030201);
        assert_eq!(LittleEndian::loadu64(&[0x01, 0x02, 0x03, 0x04, 0x05,
                                           0x06, 0x07, 0x08, 0x09], 1),
                   0x0908070605040302);
    }

    #[test]
    fn writeu16() {
        let mut buf = Vec::new();
        BigEndian::writeu16(&mut buf, 0x0102).unwrap();
        LittleEndian::writeu16(&mut buf, 0x0304).unwrap();
        assert_eq!(buf, b"\x01\x02\x04\x03");
    }

    #[test]
    fn writeu32() {
        let mut buf = Vec::new();
        BigEndian::writeu32(&mut buf, 0x01020304).unwrap();
        LittleEndian::writeu32(&mut buf, 0x05060708).unwrap();
        assert_eq!(buf, b"\x01\x02\x03\x04\x08\x07\x06\x05");
    }

    #[test]
    fn writeu64() {
        let mut buf = Vec::new();
        BigEndian::writeu64(&mut buf, 0x0102030405060708).unwrap();
        LittleEndian::writeu64(&mut buf, 0x090a0b0c0d0e0f10).unwrap();
        assert_eq!(buf, b"\x01\x02\x03\x04\x05\x06\x07\x08\
                          \x10\x0f\x0e\x0d\x0c\x0b\x0a\x09");
    }

    #[test]
    fn dispatch() {
        fn dispatch_sub<E>(data: &[u8]) -> u16 where E: Endian {
            E::loadu16(data, 0)
        }
        assert_eq!(dispatch_sub::<BigEndian>(&[0x01, 0x02]), 0x0102);
        assert_eq!(dispatch_sub::<LittleEndian>(&[0x01, 0x02]), 0x0201);
    }

    #[test]
    fn static_dispatch() {
        fn dispatch_sub<E>(data: &[u8]) -> u16 where E: Endian {
            E::loadu16(data, 0)
        }
        assert_eq!(dispatch_sub::<BigEndian> as *const (),
                   dispatch_sub::<BigEndian> as *const ());
        assert_ne!(dispatch_sub::<BigEndian> as *const (),
                   dispatch_sub::<LittleEndian> as *const ());
    }

    #[test]
    #[should_panic(expected = "index 3 out of range for slice of length 2")]
    fn out_of_range() {
        BigEndian::loadu16(&[0x01, 0x02], 1);
    }

    // "attempt to add with overflow" with the arithmetic overflow
    // check, and "slice index starts at 18446744073709551615 but ends
    // at 1" without it.
    #[test]
    #[should_panic(expected = "at")]
    fn wrap_around() {
        BigEndian::loadu16(&[0x01, 0x02], (-1isize) as usize);
    }
}
