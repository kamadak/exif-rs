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

use std::mem;

// This is a module to select endianess by using generics
// in order to avoid run-time dispatching penalty at the cost of
// increased object size.

pub trait Endian {
    fn loadu16(buf: &[u8], from: usize) -> u16;
    fn loadu32(buf: &[u8], from: usize) -> u32;
}

pub struct BigEndian;
pub struct LittleEndian;

macro_rules! generate_load {
    ($name:ident, $int_type:ident, $from_func:ident) => (
        fn $name(buf: &[u8], offset: usize) -> $int_type {
            // Check if the specified range of the slice is valid
            // before transmute().  This will also detect the
            // wrap-around of (offset + size_of) in the release mode.
            let buf = &buf[offset .. offset + mem::size_of::<$int_type>()];
            let ptr = buf.as_ptr() as *const $int_type;
            let num = unsafe { mem::transmute(*ptr) };
            $int_type::$from_func(num)
        }
    )
}

impl Endian for BigEndian {
    generate_load!(loadu16, u16, from_be);
    generate_load!(loadu32, u32, from_be);
}

impl Endian for LittleEndian {
    generate_load!(loadu16, u16, from_le);
    generate_load!(loadu32, u32, from_le);
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
        assert!(dispatch_sub::<BigEndian> as *const () !=
                dispatch_sub::<LittleEndian> as *const ());
    }

    #[test]
    #[should_panic(expected = "index 3 out of range for slice of length 2")]
    fn out_of_range() {
        BigEndian::loadu16(&[0x01, 0x02], 1);
    }
}
