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

use std::fmt;

use crate::endian::{Endian, BigEndian, LittleEndian};
use crate::error::Error;
use crate::tag::{Context, Tag, UnitPiece};
use crate::value;
use crate::value::Value;
use crate::value::get_type_info;
use crate::util::{atou16, ctou32};

// TIFF header magic numbers [EXIF23 4.5.2].
const TIFF_BE: u16 = 0x4d4d;
const TIFF_LE: u16 = 0x4949;
const TIFF_FORTY_TWO: u16 = 0x002a;
pub const TIFF_BE_SIG: [u8; 4] = [0x4d, 0x4d, 0x00, 0x2a];
pub const TIFF_LE_SIG: [u8; 4] = [0x49, 0x49, 0x2a, 0x00];

/// A TIFF field.
#[derive(Debug)]
pub struct Field<'a> {
    /// The tag of this field.
    pub tag: Tag,
    /// False for the primary image and true for the thumbnail.
    pub thumbnail: bool,
    /// The value of this field.
    pub value: Value<'a>,
}

/// Parse the Exif attributes in the TIFF format.
///
/// Returns a Vec of Exif fields and a bool.
/// The boolean value is true if the data is little endian.
/// If an error occurred, `exif::Error` is returned.
pub fn parse_exif(data: &[u8]) -> Result<(Vec<Field>, bool), Error> {
    // Check the byte order and call the real parser.
    if data.len() < 8 {
        return Err(Error::InvalidFormat("Truncated TIFF header"));
    }
    match BigEndian::loadu16(data, 0) {
        TIFF_BE => parse_exif_sub::<BigEndian>(data).map(|v| (v, false)),
        TIFF_LE => parse_exif_sub::<LittleEndian>(data).map(|v| (v, true)),
        _ => Err(Error::InvalidFormat("Invalid TIFF byte order")),
    }
}

fn parse_exif_sub<E>(data: &[u8])
                     -> Result<Vec<Field>, Error> where E: Endian {
    // Parse the rest of the header (42 and the IFD offset).
    if E::loadu16(data, 2) != TIFF_FORTY_TWO {
        return Err(Error::InvalidFormat("Invalid forty two"));
    }
    let ifd_offset = E::loadu32(data, 4) as usize;
    let mut fields = Vec::new();
    parse_ifd::<E>(&mut fields, data, ifd_offset, Context::Tiff, false)?;
    Ok(fields)
}

// Parse IFD [EXIF23 4.6.2].
fn parse_ifd<'a, E>(fields: &mut Vec<Field<'a>>, data: &'a [u8],
                    offset: usize, ctx: Context, thumbnail: bool)
                    -> Result<(), Error> where E: Endian {
    // Count (the number of the entries).
    if data.len() < offset || data.len() - offset < 2 {
        return Err(Error::InvalidFormat("Truncated IFD count"));
    }
    let count = E::loadu16(data, offset) as usize;

    // Array of entries.  (count * 12) never overflows.
    if data.len() - offset - 2 < count * 12 {
        return Err(Error::InvalidFormat("Truncated IFD"));
    }
    for i in 0..count as usize {
        let tag = E::loadu16(data, offset + 2 + i * 12);
        let typ = E::loadu16(data, offset + 2 + i * 12 + 2);
        let cnt = E::loadu32(data, offset + 2 + i * 12 + 4) as usize;
        let valofs_at = offset + 2 + i * 12 + 8;
        let (unitlen, parser) = get_type_info::<E>(typ);
        let vallen = unitlen.checked_mul(cnt).ok_or(
            Error::InvalidFormat("Invalid entry count"))?;
        let val;
        if unitlen == 0 {
            val = Value::Unknown(typ, cnt as u32, valofs_at as u32);
        } else if vallen <= 4 {
            val = parser(data, valofs_at, cnt);
        } else {
            let ofs = E::loadu32(data, valofs_at) as usize;
            if data.len() < ofs || data.len() - ofs < vallen {
                return Err(Error::InvalidFormat("Truncated field value"));
            }
            val = parser(data, ofs, cnt);
        }

        // No infinite recursion will occur because the context is not
        // recursively defined.
        let tag = Tag(ctx, tag);
        match tag {
            Tag::ExifIFDPointer => parse_child_ifd::<E>(
                fields, data, &val, Context::Exif, thumbnail)?,
            Tag::GPSInfoIFDPointer => parse_child_ifd::<E>(
                fields, data, &val, Context::Gps, thumbnail)?,
            Tag::InteropIFDPointer => parse_child_ifd::<E>(
                fields, data, &val, Context::Interop, thumbnail)?,
            _ => fields.push(Field { tag: tag, thumbnail: thumbnail,
                                     value: val }),
        }
    }

    // Offset to the next IFD.
    if data.len() - offset - 2 - count * 12 < 4 {
        return Err(Error::InvalidFormat("Truncated next IFD offset"));
    }
    let next_ifd_offset = E::loadu32(data, offset + 2 + count * 12) as usize;
    // Ignore IFDs after IFD1 (thumbnail) for now.
    if next_ifd_offset == 0 || thumbnail {
        return Ok(());
    }
    if ctx != Context::Tiff {
        return Err(Error::InvalidFormat("Unexpected next IFD"));
    }
    parse_ifd::<E>(fields, data, next_ifd_offset, Context::Tiff, true)
}

fn parse_child_ifd<'a, E>(fields: &mut Vec<Field<'a>>, data: &'a [u8],
                          pointer: &Value, ctx: Context, thumbnail: bool)
                          -> Result<(), Error> where E: Endian {
    // A pointer field has type == LONG and count == 1, so the
    // value (IFD offset) must be embedded in the "value offset"
    // element of the field.
    let ofs = pointer.get_uint(0).ok_or(
        Error::InvalidFormat("Invalid pointer"))? as usize;
    parse_ifd::<E>(fields, data, ofs, ctx, thumbnail)
}

pub fn is_tiff(buf: &[u8]) -> bool {
    buf.starts_with(&TIFF_BE_SIG) || buf.starts_with(&TIFF_LE_SIG)
}

/// A struct used to parse a DateTime field.
///
/// # Examples
/// ```
/// use exif::DateTime;
/// let dt = DateTime::from_ascii(b"2016:05:04 03:02:01").unwrap();
/// assert_eq!(dt.year, 2016);
/// assert_eq!(format!("{}", dt), "2016-05-04 03:02:01");
/// ```
#[derive(Debug)]
pub struct DateTime {
    pub year: u16,
    pub month: u8,
    pub day: u8,
    pub hour: u8,
    pub minute: u8,
    pub second: u8,
    /// The subsecond data in nanoseconds.  If the Exif attribute has
    /// more sigfinicant digits, they are rounded down.
    pub nanosecond: Option<u32>,
    /// The offset of the time zone in minutes.
    pub offset: Option<i16>,
}

impl DateTime {
    /// Parse an ASCII data of a DateTime field.  The range of a number
    /// is not validated, so, for example, 13 may be returned as the month.
    ///
    /// If the value is blank, `Error::BlankValue` is returned.
    pub fn from_ascii(data: &[u8]) -> Result<DateTime, Error> {
        if data == b"    :  :     :  :  " || data == b"                   " {
            return Err(Error::BlankValue("DateTime is blank"));
        } else if data.len() < 19 {
            return Err(Error::InvalidFormat("DateTime too short"));
        } else if !(data[4] == b':' && data[7] == b':' && data[10] == b' ' &&
                    data[13] == b':' && data[16] == b':') {
            return Err(Error::InvalidFormat("Invalid DateTime delimiter"));
        }
        Ok(DateTime {
            year: atou16(&data[0..4])?,
            month: atou16(&data[5..7])? as u8,
            day: atou16(&data[8..10])? as u8,
            hour: atou16(&data[11..13])? as u8,
            minute: atou16(&data[14..16])? as u8,
            second: atou16(&data[17..19])? as u8,
            nanosecond: None,
            offset: None,
        })
    }

    /// Parses an SubsecTime-like field.
    pub fn parse_subsec(&mut self, data: &[u8]) -> Result<(), Error> {
        let mut subsec = 0;
        let mut ndigits = 0;
        for &c in data {
            if c == b' ' {
                break;
            }
            subsec = subsec * 10 + ctou32(c)?;
            ndigits += 1;
            if ndigits >= 9 {
                break;
            }
        }
        if ndigits == 0 {
            self.nanosecond = None;
        } else {
            for _ in ndigits..9 {
                subsec *= 10;
            }
            self.nanosecond = Some(subsec);
        }
        Ok(())
    }

    /// Parses an OffsetTime-like field.
    pub fn parse_offset(&mut self, data: &[u8]) -> Result<(), Error> {
        if data == b"   :  " || data == b"      " {
            return Err(Error::BlankValue("OffsetTime is blank"));
        } else if data.len() < 6 {
            return Err(Error::InvalidFormat("OffsetTime too short"));
        } else if data[3] != b':' {
            return Err(Error::InvalidFormat("Invalid OffsetTime delimiter"));
        }
        let hour = atou16(&data[1..3])?;
        let min = atou16(&data[4..6])?;
        let offset = (hour * 60 + min) as i16;
        self.offset = Some(match data[0] {
            b'+' => offset,
            b'-' => -offset,
            _ => return Err(Error::InvalidFormat("Invalid OffsetTime sign")),
        });
        Ok(())
    }
}

impl fmt::Display for DateTime {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:04}-{:02}-{:02} {:02}:{:02}:{:02}",
               self.year, self.month, self.day,
               self.hour, self.minute, self.second)
    }
}

impl<'a> Field<'a> {
    /// Returns an object that implements `std::fmt::Display` for
    /// printing the value of this field in a tag-specific format.
    ///
    /// To print the value with the unit, call `with_unit` method on the
    /// returned object.  It takes a parameter, which is either `()`,
    /// `&Field`, or `&Reader` and provides the unit information.
    /// If the unit does not depend on another field, `()` can be used.
    /// Otherwise, `&Field` or `&Reader` should be used.
    ///
    /// # Examples
    ///
    /// ```
    /// use exif::{Field, Rational, Tag, Value};
    ///
    /// let xres = Field {
    ///     tag: Tag::XResolution,
    ///     thumbnail: false,
    ///     value: Value::Rational(vec![Rational { num: 72, denom: 1 }]),
    /// };
    /// let cm = Field {
    ///     tag: Tag::ResolutionUnit,
    ///     thumbnail: false,
    ///     value: Value::Short(vec![3]),
    /// };
    /// assert_eq!(format!("{}", xres.display_value()), "72");
    /// assert_eq!(format!("{}", cm.display_value()), "cm");
    /// // The unit of XResolution is indicated by ResolutionUnit.
    /// assert_eq!(format!("{}", xres.display_value().with_unit(&cm)),
    ///            "72 pixels per cm");
    /// // If ResolutionUnit is not given, the default value is used.
    /// assert_eq!(format!("{}", xres.display_value().with_unit(())),
    ///            "72 pixels per inch");
    /// assert_eq!(format!("{}", xres.display_value().with_unit(&xres)),
    ///            "72 pixels per inch");
    ///
    /// let flen = Field {
    ///     tag: Tag::FocalLengthIn35mmFilm,
    ///     thumbnail: false,
    ///     value: Value::Short(vec![24]),
    /// };
    /// // The unit of the focal length is always mm, so the argument
    /// // has nothing to do with the result.
    /// assert_eq!(format!("{}", flen.display_value().with_unit(())), "24 mm");
    /// assert_eq!(format!("{}", flen.display_value().with_unit(&cm)), "24 mm");
    /// ```
    #[inline]
    pub fn display_value(&self) -> DisplayValue {
        DisplayValue {
            tag: self.tag,
            thumbnail: self.thumbnail,
            value_display: self.value.display_as(self.tag),
        }
    }
}

/// Helper struct for printing a value in a tag-specific format.
pub struct DisplayValue<'a> {
    tag: Tag,
    thumbnail: bool,
    value_display: value::Display<'a>,
}

impl<'a> DisplayValue<'a> {
    #[inline]
    pub fn with_unit<'b, T>(&'b self, unit_provider: T)
                            -> DisplayValueUnit<T> where T: ProvideUnit<'b> {
        DisplayValueUnit {
            thumbnail: self.thumbnail,
            value_display: self.value_display,
            unit: self.tag.unit(),
            unit_provider: unit_provider,
        }
    }
}

impl<'a> fmt::Display for DisplayValue<'a> {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.value_display.fmt(f)
    }
}

/// Helper struct for printing a value with its unit.
pub struct DisplayValueUnit<'a, T> where T: ProvideUnit<'a> {
    thumbnail: bool,
    value_display: value::Display<'a>,
    unit: Option<&'static [UnitPiece]>,
    unit_provider: T,
}

impl<'a, T> fmt::Display for DisplayValueUnit<'a, T> where T: ProvideUnit<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(unit) = self.unit {
            assert!(!unit.is_empty());
            for piece in unit {
                match *piece {
                    UnitPiece::Value => self.value_display.fmt(f),
                    UnitPiece::Str(s) => f.write_str(s),
                    UnitPiece::Tag(tag) =>
                        if let Some(x) = self.unit_provider.get_field(
                                tag, self.thumbnail) {
                            x.value.display_as(tag).fmt(f)
                        } else if let Some(x) = tag.default_value() {
                            x.display_as(tag).fmt(f)
                        } else {
                            write!(f, "[{} missing]", tag)
                        },
                }?
            }
            Ok(())
        } else {
            self.value_display.fmt(f)
        }
    }
}

pub trait ProvideUnit<'a>: Copy {
    fn get_field(self, tag: Tag, thumbnail: bool) -> Option<&'a Field<'a>>;
}

impl<'a> ProvideUnit<'a> for () {
    fn get_field(self, _tag: Tag, _thumbnail: bool) -> Option<&'a Field<'a>> {
        None
    }
}

impl<'a> ProvideUnit<'a> for &'a Field<'a> {
    fn get_field(self, tag: Tag, thumbnail: bool) -> Option<&'a Field<'a>> {
        Some(self).filter(|x| x.tag == tag && x.thumbnail == thumbnail)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::value::Rational;

    // Before the error is returned, the IFD is parsed twice as the
    // 0th and 1st IFDs.
    #[test]
    fn inf_loop_by_next() {
        let data = b"MM\0\x2a\0\0\0\x08\
                     \0\x01\x01\0\0\x03\0\0\0\x01\0\x14\0\0\0\0\0\x08";
        // assert_err_pat!(parse_exif(data),
        //                 Error::InvalidFormat("Unexpected next IFD"));
        let (v, _) = parse_exif(data).unwrap();
        assert_eq!(v.len(), 2);
    }

    #[test]
    fn unknown_field() {
        let data = b"MM\0\x2a\0\0\0\x08\
                     \0\x01\x01\0\xff\xff\0\0\0\x01\0\x14\0\0\0\0\0\0";
        let (v, _) = parse_exif(data).unwrap();
        assert_eq!(v.len(), 1);
        assert_pat!(v[0].value, Value::Unknown(0xffff, 1, 0x12));
    }

    #[test]
    fn date_time() {
        let mut dt = DateTime::from_ascii(b"2016:05:04 03:02:01").unwrap();
        assert_eq!(dt.year, 2016);
        assert_eq!(format!("{}", dt), "2016-05-04 03:02:01");

        dt.parse_subsec(b"987").unwrap();
        assert_eq!(dt.nanosecond.unwrap(), 987000000);
        dt.parse_subsec(b"000987").unwrap();
        assert_eq!(dt.nanosecond.unwrap(), 987000);
        dt.parse_subsec(b"987654321").unwrap();
        assert_eq!(dt.nanosecond.unwrap(), 987654321);
        dt.parse_subsec(b"9876543219").unwrap();
        assert_eq!(dt.nanosecond.unwrap(), 987654321);
        dt.parse_subsec(b"130   ").unwrap();
        assert_eq!(dt.nanosecond.unwrap(), 130000000);
        dt.parse_subsec(b"0").unwrap();
        assert_eq!(dt.nanosecond.unwrap(), 0);
        dt.parse_subsec(b"").unwrap();
        assert!(dt.nanosecond.is_none());
        dt.parse_subsec(b" ").unwrap();
        assert!(dt.nanosecond.is_none());

        dt.parse_offset(b"+00:00").unwrap();
        assert_eq!(dt.offset.unwrap(), 0);
        dt.parse_offset(b"+01:23").unwrap();
        assert_eq!(dt.offset.unwrap(), 83);
        dt.parse_offset(b"+99:99").unwrap();
        assert_eq!(dt.offset.unwrap(), 6039);
        dt.parse_offset(b"-01:23").unwrap();
        assert_eq!(dt.offset.unwrap(), -83);
        dt.parse_offset(b"-99:99").unwrap();
        assert_eq!(dt.offset.unwrap(), -6039);
        assert_err_pat!(dt.parse_offset(b"   :  "), Error::BlankValue(_));
        assert_err_pat!(dt.parse_offset(b"      "), Error::BlankValue(_));
    }

    #[test]
    fn display_value_with_unit() {
        let cm = Field {
            tag: Tag::ResolutionUnit,
            thumbnail: false,
            value: Value::Short(vec![3]),
        };
        let cm_tn = Field {
            tag: Tag::ResolutionUnit,
            thumbnail: true,
            value: Value::Short(vec![3]),
        };
        // No unit.
        let exifver = Field {
            tag: Tag::ExifVersion,
            thumbnail: false,
            value: Value::Undefined(b"0231", 0),
        };
        assert_eq!(format!("{}", exifver.display_value()),
                   "2.31");
        assert_eq!(format!("{}", exifver.display_value().with_unit(())),
                   "2.31");
        assert_eq!(format!("{}", exifver.display_value().with_unit(&cm)),
                   "2.31");
        // Fixed string.
        let width = Field {
            tag: Tag::ImageWidth,
            thumbnail: false,
            value: Value::Short(vec![257]),
        };
        assert_eq!(format!("{}", width.display_value()),
                   "257");
        assert_eq!(format!("{}", width.display_value().with_unit(())),
                   "257 pixels");
        assert_eq!(format!("{}", width.display_value().with_unit(&cm)),
                   "257 pixels");
        // Unit tag (with a non-default value).
        // Unit tag is missing but the default is specified.
        let xres = Field {
            tag: Tag::XResolution,
            thumbnail: false,
            value: Value::Rational(vec![Rational { num: 300, denom: 1 }]),
        };
        assert_eq!(format!("{}", xres.display_value()),
                   "300");
        assert_eq!(format!("{}", xres.display_value().with_unit(())),
                   "300 pixels per inch");
        assert_eq!(format!("{}", xres.display_value().with_unit(&cm)),
                   "300 pixels per cm");
        assert_eq!(format!("{}", xres.display_value().with_unit(&cm_tn)),
                   "300 pixels per inch");
        // Unit tag is missing and the default is not specified.
        let gpslat = Field {
            tag: Tag::GPSLatitude,
            thumbnail: false,
            value: Value::Rational(vec![
                Rational { num: 10, denom: 1 },
                Rational { num: 0, denom: 1 },
                Rational { num: 1, denom: 10 },
            ]),
        };
        assert_eq!(format!("{}", gpslat.display_value()),
                   "10 deg 0 min 0.1 sec");
        assert_eq!(format!("{}", gpslat.display_value().with_unit(())),
                   "10 deg 0 min 0.1 sec [GPSLatitudeRef missing]");
        assert_eq!(format!("{}", gpslat.display_value().with_unit(&cm)),
                   "10 deg 0 min 0.1 sec [GPSLatitudeRef missing]");
    }
}
