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

use std::mem;
use std::slice;

use endian::{Endian, BigEndian, LittleEndian};
use error::Error;
use value::Value;

// Returns the type, count, and encoded value.
fn compose_value<E>(value: &Value)
                    -> Result<(u16, usize, Vec<u8>), Error> where E: Endian {
    match *value {
        Value::Byte(ref vec) =>
            Ok((1, vec.len(), vec.clone())),
        Value::Ascii(ref vec) => {
            let mut buf = Vec::new();
            for &s in vec {
                buf.extend_from_slice(s);
                buf.push(0);
            }
            Ok((2, buf.len(), buf))
        },
        Value::Short(ref vec) => {
            let mut buf = Vec::new();
            for &v in vec {
                try!(E::writeu16(&mut buf, v));
            }
            Ok((3, vec.len(), buf))
        },
        Value::Long(ref vec) => {
            let mut buf = Vec::new();
            for &v in vec {
                try!(E::writeu32(&mut buf, v));
            }
            Ok((4, vec.len(), buf))
        },
        Value::Rational(ref vec) => {
            let mut buf = Vec::new();
            for v in vec {
                try!(E::writeu32(&mut buf, v.num));
                try!(E::writeu32(&mut buf, v.denom));
            }
            Ok((5, vec.len(), buf))
        },
        Value::SByte(ref vec) => {
            let uslice = unsafe { slice::from_raw_parts(
                vec.as_ptr() as *const u8, vec.len()) };
            Ok((6, vec.len(), uslice.to_vec()))
        },
        Value::Undefined(ref s) =>
            Ok((7, s.len(), s.to_vec())),
        Value::SShort(ref vec) => {
            let mut buf = Vec::new();
            for &v in vec {
                try!(E::writeu16(&mut buf, v as u16));
            }
            Ok((8, vec.len(), buf))
        },
        Value::SLong(ref vec) => {
            let mut buf = Vec::new();
            for &v in vec {
                try!(E::writeu32(&mut buf, v as u32));
            }
            Ok((9, vec.len(), buf))
        },
        Value::SRational(ref vec) => {
            let mut buf = Vec::new();
            for v in vec {
                try!(E::writeu32(&mut buf, v.num as u32));
                try!(E::writeu32(&mut buf, v.denom as u32));
            }
            Ok((10, vec.len(), buf))
        },
        Value::Float(ref vec) => {
            let mut buf = Vec::new();
            for &v in vec {
                try!(E::writeu32(&mut buf, unsafe { mem::transmute(v) }));
            }
            Ok((11, vec.len(), buf))
        },
        Value::Double(ref vec) => {
            let mut buf = Vec::new();
            for &v in vec {
                try!(E::writeu64(&mut buf, unsafe { mem::transmute(v) }));
            }
            Ok((12, vec.len(), buf))
        },
        Value::Unknown(_, _, _) =>
            Err(Error::InvalidFormat("Cannot write unknown field types")),
    }
}

#[cfg(test)]
mod tests {
    use value::{Rational, SRational};
    use super::*;

    #[test]
    fn compose_field_value() {
        let patterns = vec![
            (Value::Byte(vec![1, 2]),
             (1, 2, vec![1, 2]),
             (1, 2, vec![1, 2])),
            (Value::Ascii(vec![b"a", b"b"]),
             (2, 4, b"a\0b\0".to_vec()),
             (2, 4, b"a\0b\0".to_vec())),
            (Value::Short(vec![0x0102, 0x0304]),
             (3, 2, b"\x01\x02\x03\x04".to_vec()),
             (3, 2, b"\x02\x01\x04\x03".to_vec())),
            (Value::Long(vec![0x01020304, 0x05060708]),
             (4, 2, b"\x01\x02\x03\x04\x05\x06\x07\x08".to_vec()),
             (4, 2, b"\x04\x03\x02\x01\x08\x07\x06\x05".to_vec())),
            (Value::Rational(vec![Rational { num: 1, denom: 2},
                                  Rational { num: 3, denom: 4}]),
             (5, 2, b"\0\0\0\x01\0\0\0\x02\0\0\0\x03\0\0\0\x04".to_vec()),
             (5, 2, b"\x01\0\0\0\x02\0\0\0\x03\0\0\0\x04\0\0\0".to_vec())),
            (Value::SByte(vec![-2, -128]),
             (6, 2, b"\xfe\x80".to_vec()),
             (6, 2, b"\xfe\x80".to_vec())),
            (Value::Undefined(b"abc"),
             (7, 3, b"abc".to_vec()),
             (7, 3, b"abc".to_vec())),
            (Value::SShort(vec![-2, -0x8000]),
             (8, 2, b"\xff\xfe\x80\x00".to_vec()),
             (8, 2, b"\xfe\xff\x00\x80".to_vec())),
            (Value::SLong(vec![-2, -0x80000000]),
             (9, 2, b"\xff\xff\xff\xfe\x80\x00\x00\x00".to_vec()),
             (9, 2, b"\xfe\xff\xff\xff\x00\x00\x00\x80".to_vec())),
            (Value::SRational(vec![SRational { num: -1, denom: -2},
                                   SRational { num: -3, denom: -4}]),
             (10, 2, b"\xff\xff\xff\xff\xff\xff\xff\xfe\
                       \xff\xff\xff\xfd\xff\xff\xff\xfc".to_vec()),
             (10, 2, b"\xff\xff\xff\xff\xfe\xff\xff\xff\
                       \xfd\xff\xff\xff\xfc\xff\xff\xff".to_vec())),
            (Value::Float(vec![2.5, -0.5]),
             (11, 2, b"\x40\x20\x00\x00\xbf\x00\x00\x00".to_vec()),
             (11, 2, b"\x00\x00\x20\x40\x00\x00\x00\xbf".to_vec())),
            (Value::Double(vec![2.5, -0.5]),
             (12, 2, b"\x40\x04\x00\x00\x00\x00\x00\x00\
                       \xbf\xe0\x00\x00\x00\x00\x00\x00".to_vec()),
             (12, 2, b"\x00\x00\x00\x00\x00\x00\x04\x40\
                       \x00\x00\x00\x00\x00\x00\xe0\xbf".to_vec())),
        ];
        for p in patterns.into_iter() {
            assert_eq!(compose_value::<BigEndian>(&p.0).unwrap(), p.1);
            assert_eq!(compose_value::<LittleEndian>(&p.0).unwrap(), p.2);
        }
    }
}
