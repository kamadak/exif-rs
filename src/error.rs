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

use std::error;
use std::fmt;
use std::io;

/// An error type returned when parsing Exif data.
#[derive(Debug)]
pub enum Error {
    /// Input data was malformed or truncated.
    InvalidFormat(&'static str),
    /// Input data could not be read due to an I/O error and
    /// a `std::io::Error` value is associated with this variant.
    Io(io::Error),
    /// Exif attribute information was not found in JPEG data.
    NotFound(&'static str),
    /// The value of the field is blank.  Some fields have blank values
    /// whose meanings are defined as "unknown".  Such a blank value
    /// should be treated the same as the absence of the field.
    BlankValue(&'static str),
    /// Field values or image data are too big to encode.
    TooBig(&'static str),
    /// The field type is not supported and cannnot be encoded.
    NotSupported(&'static str),
}

impl From<io::Error> for Error {
    fn from(err: io::Error) -> Error {
        Error::Io(err)
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Error::InvalidFormat(msg) => f.write_str(msg),
            Error::Io(ref err) => err.fmt(f),
            Error::NotFound(msg) => f.write_str(msg),
            Error::BlankValue(msg) => f.write_str(msg),
            Error::TooBig(msg) => f.write_str(msg),
            Error::NotSupported(msg) => f.write_str(msg),
        }
    }
}

impl error::Error for Error {
    fn description(&self) -> &str {
        match *self {
            Error::InvalidFormat(msg) => msg,
            Error::Io(ref err) => err.description(),
            Error::NotFound(msg) => msg,
            Error::BlankValue(msg) => msg,
            Error::TooBig(msg) => msg,
            Error::NotSupported(msg) => msg,
        }
    }

    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        match *self {
            Error::InvalidFormat(_) => None,
            Error::Io(ref err) => Some(err),
            Error::NotFound(_) => None,
            Error::BlankValue(_) => None,
            Error::TooBig(_) => None,
            Error::NotSupported(_) => None,
        }
    }
}
