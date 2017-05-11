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

use value;
use value::Value;

/// A tag of a TIFF field.
//
// This is not an enum to keep safety and API stability, while
// supporting unknown tag numbers.  This comment is based on the
// behavior of Rust 1.12.
// Storing unknown values in a repr(u16) enum is unsafe.  The compiler
// assumes that there is no undefined discriminant even with a C-like
// enum, so the exhaustiveness check of a match expression will break.
// Storing unknown values in a special variant such as Unknown(u16)
// tends to break backward compatibility.  When Tag::VariantFoo is
// defined in a new version of the library, the old codes using
// Tag::Unknown(Foo's tag number) will break.
//
// Use of constants is restricted in patterns.  As of Rust 1.12,
// PartialEq and Eq need to be _automatically derived_ for Tag to
// emulate structural equivalency.
// <https://github.com/rust-lang/rfcs/pull/1445>
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Tag(pub Context, pub u16);

impl Tag {
    /// Returns the context of the tag.
    #[inline]
    pub fn context(self) -> Context {
        self.0
    }

    /// Returns the tag number.
    #[inline]
    pub fn number(self) -> u16 {
        self.1
    }

    /// Returns the tag number.
    #[deprecated(since = "0.2.0", note = "renamed to `number()`")]
    #[inline]
    pub fn value(self) -> u16 {
        self.number()
    }

    /// Returns the description of the tag.
    #[inline]
    pub fn description(&self) -> Option<&str> {
        get_tag_info(*self).map(|ti| ti.desc)
    }

    /// Returns the default value of the tag.  `None` is returned if
    /// it is not defined in the standard or it depends on the context.
    #[inline]
    pub fn default_value(&self) -> Option<Value> {
        get_tag_info(*self).and_then(|ti| (&ti.default).into())
    }
}

impl fmt::Display for Tag {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match get_tag_info(*self) {
            Some(ti) => f.pad(ti.name),
            None => f.pad(&format!("{:?}", self)),
        }
    }
}

/// An enum that indicates how a tag value is interpreted.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Context {
    /// TIFF attributes defined in the TIFF Rev. 6.0 specification.
    Tiff,	// 0th/1st IFD
    /// Exif attributes.
    Exif,	// 0th/1st IFD -- Exif IFD
    /// GPS attributes.
    Gps,	// 0th/1st IFD -- GPS IFD
    /// Interoperability attributes.
    Interop,	// 0th/1st IFD -- Exif IFD -- Interoperability IFD
}

macro_rules! generate_well_known_tag_constants {
    (
        $( |$ctx:path| $(
            // Copy the doc attribute to the actual definition.
            $( #[$attr:meta] )*
            ($name:ident, $num:expr, $defval:expr, $dispval:ident, $desc:expr)
        ),+, )+
    ) => (
        /// A module that contains Exif tag constants.
        ///
        /// Compatibility warning: Exif tag constants in this module will be
        /// converted to associated constants of `Tag` when the feature is
        /// stabilized.
        ///
        /// It is not recommended to import the constants directly into
        /// your namespace; import the module and use with the module name
        /// like `tag::DateTime`.  The constant names follow the Exif
        /// specification but not the Rust naming conventions, and a user
        /// of the constants will get the non_upper_case_globals warning
        /// if a bare constant is used in a match arm.
        // This is discussed in
        // <https://github.com/rust-lang/rust/issues/25207>.
        pub mod constants {
            use super::{Context, Tag};
            $($(
                $( #[$attr] )*
                #[allow(non_upper_case_globals)]
                pub const $name: Tag = Tag($ctx, $num);
            )+)+
        }

        // Use a separate module to avoid name conflicts between
        // const Tag and static TagInfo.
        mod tag_info {
            use std::fmt;
            use value::Value;
            use value::DefaultValue;

            pub struct TagInfo {
                pub name: &'static str,
                pub desc: &'static str,
                pub default: DefaultValue,
                pub dispval: fn(&mut fmt::Write, &Value) -> fmt::Result,
            }

            $($(
                #[allow(non_upper_case_globals)]
                pub static $name: TagInfo = TagInfo {
                    name: stringify!($name),
                    desc: $desc,
                    default: $defval,
                    dispval: super::$dispval,
                };
            )+)+
        }

        fn get_tag_info(tag: Tag) -> Option<&'static tag_info::TagInfo> {
            match tag {
                $($(
                    constants::$name => Some(&tag_info::$name),
                )+)+
                _ => None,
            }
        }
    )
}

// Tag constant names do not follow the Rust naming conventions but
// the Exif field names: camel cases and all-capital acronyms.
generate_well_known_tag_constants!(
    // Exif-specific IFDs [EXIF23 4.6.3].
    |Context::Tiff|

    /// A pointer to the Exif IFD.  This is used for the internal structure
    /// of Exif data and will not be returned to the user.
    (ExifIFDPointer, 0x8769, DefaultValue::None, d_tbd,
     "Exif IFD pointer"),
    /// A pointer to the GPS IFD.  This is used for the internal structure
    /// of Exif data and will not be returned to the user.
    (GPSInfoIFDPointer, 0x8825, DefaultValue::None, d_tbd,
     "GPS Info IFD pointer"),

    |Context::Exif|

    /// A pointer to the interoperability IFD.  This is used for the internal
    /// structure of Exif data and will not be returned to the user.
    (InteropIFDPointer, 0xa005, DefaultValue::None, d_tbd,
     "Interoperability IFD pointer"),

    // TIFF primary and thumbnail attributes [EXIF23 4.6.4 Table 4,
    // 4.6.8 Table 17, and 4.6.8 Table 21].
    |Context::Tiff|

    (ImageWidth, 0x100, DefaultValue::None, d_tbd,
     "Image width"),
    (ImageLength, 0x101, DefaultValue::None, d_tbd,
     "Image height"),
    (BitsPerSample, 0x102, DefaultValue::Short(&[8, 8, 8]), d_tbd,
     "Number of bits per component"),
    (Compression, 0x103, DefaultValue::None, d_tbd,
     "Compression scheme"),
    (PhotometricInterpretation, 0x106, DefaultValue::None, d_tbd,
     "Pixel composition"),
    (ImageDescription, 0x10e, DefaultValue::None, d_tbd,
     "Image title"),
    (Make, 0x10f, DefaultValue::None, d_tbd,
     "Manufacturer of image input equipment"),
    (Model, 0x110, DefaultValue::None, d_tbd,
     "Model of image input equipment"),
    (StripOffsets, 0x111, DefaultValue::None, d_tbd,
     "Image data location"),
    (Orientation, 0x112, DefaultValue::Short(&[1]), d_tbd,
     "Orientation of image"),
    (SamplesPerPixel, 0x115, DefaultValue::Short(&[3]), d_tbd,
     "Number of components"),
    (RowsPerStrip, 0x116, DefaultValue::None, d_tbd,
     "Number of rows per strip"),
    (StripByteCounts, 0x117, DefaultValue::None, d_tbd,
     "Bytes per compressed strip"),
    (XResolution, 0x11a, DefaultValue::Rational(&[(72, 1)]), d_tbd,
     "Image resolution in width direction"),
    (YResolution, 0x11b, DefaultValue::Rational(&[(72, 1)]), d_tbd,
     "Image resolution in height direction"),
    (PlanarConfiguration, 0x11c, DefaultValue::Short(&[1]), d_tbd,
     "Image data arrangement"),
    (ResolutionUnit, 0x128, DefaultValue::Short(&[2]), d_tbd,
     "Unit of X and Y resolution"),
    (TransferFunction, 0x12d, DefaultValue::None, d_tbd,
     "Transfer function"),
    (Software, 0x131, DefaultValue::None, d_tbd,
     "Software used"),
    (DateTime, 0x132, DefaultValue::None, d_tbd,
     "File change date and time"),
    (Artist, 0x13b, DefaultValue::None, d_tbd,
     "Person who created the image"),
    (WhitePoint, 0x13e, DefaultValue::None, d_tbd,
     "White point chromaticity"),
    (PrimaryChromaticities, 0x13f, DefaultValue::None, d_tbd,
     "Chromaticities of primaries"),
    (JPEGInterchangeFormat, 0x201, DefaultValue::None, d_tbd,
     "Offset to JPEG SOI"),
    (JPEGInterchangeFormatLength, 0x202, DefaultValue::None, d_tbd,
     "Bytes of JPEG data"),
    (YCbCrCoefficients, 0x211, DefaultValue::Unspecified, d_tbd,
     "Color space transformation matrix coefficients"),
    (YCbCrSubSampling, 0x212, DefaultValue::None, d_tbd,
     "Subsampling ratio of Y to C"),
    (YCbCrPositioning, 0x213, DefaultValue::Short(&[1]), d_tbd,
     "Y and C positioning"),
    (ReferenceBlackWhite, 0x214, DefaultValue::ContextDependent, d_tbd,
     "Pair of black and white reference values"),
    (Copyright, 0x8298, DefaultValue::None, d_tbd,
     "Copyright holder"),

    // Exif IFD attributes [EXIF23 4.6.5 Table 7 and 4.6.8 Table 18].
    |Context::Exif|

    (ExposureTime, 0x829a, DefaultValue::None, d_tbd,
     "Exposure time"),
    (FNumber, 0x829d, DefaultValue::None, d_tbd,
     "F number"),
    (ExposureProgram, 0x8822, DefaultValue::None, d_tbd,
     "Exposure program"),
    (SpectralSensitivity, 0x8824, DefaultValue::None, d_tbd,
     "Spectral sensitivity"),
    (PhotographicSensitivity, 0x8827, DefaultValue::None, d_tbd,
     "Photographic sensitivity"),
    (OECF, 0x8828, DefaultValue::None, d_tbd,
     "Optoelectric conversion factor"),
    (SensitivityType, 0x8830, DefaultValue::None, d_tbd,
     "Sensitivity type"),
    (StandardOutputSensitivity, 0x8831, DefaultValue::None, d_tbd,
     "Standard output sensitivity"),
    (RecommendedExposureIndex, 0x8832, DefaultValue::None, d_tbd,
     "Recommended exposure index"),
    (ISOSpeed, 0x8833, DefaultValue::None, d_tbd,
     "ISO speed"),
    (ISOSpeedLatitudeyyy, 0x8834, DefaultValue::None, d_tbd,
     "ISO speed latitude yyy"),
    (ISOSpeedLatitudezzz, 0x8835, DefaultValue::None, d_tbd,
     "ISO speed latitude zzz"),
    // The absence of this field means non-conformance to Exif, so the default
    // value specified in the standard (e.g., "0231") should not apply.
    (ExifVersion, 0x9000, DefaultValue::None, d_tbd,
     "Exif version"),
    (DateTimeOriginal, 0x9003, DefaultValue::None, d_tbd,
     "Date and time of original data generation"),
    (DateTimeDigitized, 0x9004, DefaultValue::None, d_tbd,
     "Date and time of digital data generation"),
    (OffsetTime, 0x9010, DefaultValue::None, d_tbd,
     "Offset data of DateTime"),
    (OffsetTimeOriginal, 0x9011, DefaultValue::None, d_tbd,
     "Offset data of DateTimeOriginal"),
    (OffsetTimeDigitized, 0x9012, DefaultValue::None, d_tbd,
     "Offset data of DateTimeDigitized"),
    (ComponentsConfiguration, 0x9101, DefaultValue::ContextDependent, d_tbd,
     "Meaning of each component"),
    (CompressedBitsPerPixel, 0x9102, DefaultValue::None, d_tbd,
     "Image compression mode"),
    (ShutterSpeedValue, 0x9201, DefaultValue::None, d_tbd,
     "Shutter speed"),
    (ApertureValue, 0x9202, DefaultValue::None, d_tbd,
     "Aperture"),
    (BrightnessValue, 0x9203, DefaultValue::None, d_tbd,
     "Brightness"),
    (ExposureBiasValue, 0x9204, DefaultValue::None, d_tbd,
     "Exposure bias"),
    (MaxApertureValue, 0x9205, DefaultValue::None, d_tbd,
     "Maximum lens aperture"),
    (SubjectDistance, 0x9206, DefaultValue::None, d_tbd,
     "Subject distance"),
    (MeteringMode, 0x9207, DefaultValue::Short(&[0]), d_tbd,
     "Metering mode"),
    (LightSource, 0x9208, DefaultValue::Short(&[0]), d_tbd,
     "Light source"),
    (Flash, 0x9209, DefaultValue::Unspecified, d_tbd,
     "Flash"),
    (FocalLength, 0x920a, DefaultValue::None, d_tbd,
     "Lens focal length"),
    (SubjectArea, 0x9214, DefaultValue::None, d_tbd,
     "Subject area"),
    (MakerNote, 0x927c, DefaultValue::None, d_tbd,
     "Manufacturer notes"),
    (UserComment, 0x9286, DefaultValue::None, d_tbd,
     "User comments"),
    (SubSecTime, 0x9290, DefaultValue::None, d_tbd,
     "DateTime subseconds"),
    (SubSecTimeOriginal, 0x9291, DefaultValue::None, d_tbd,
     "DateTimeOriginal subseconds"),
    (SubSecTimeDigitized, 0x9292, DefaultValue::None, d_tbd,
     "DateTimeDigitized subseconds"),
    (Temperature, 0x9400, DefaultValue::None, d_tbd,
     "Temperature"),
    (Humidity, 0x9401, DefaultValue::None, d_tbd,
     "Humidity"),
    (Pressure, 0x9402, DefaultValue::None, d_tbd,
     "Pressure"),
    (WaterDepth, 0x9403, DefaultValue::None, d_tbd,
     "Water depth"),
    (Acceleration, 0x9404, DefaultValue::None, d_tbd,
     "Acceleration"),
    (CameraElevationAngle, 0x9405, DefaultValue::None, d_tbd,
     "Camera elevation angle"),
    (FlashpixVersion, 0xa000, DefaultValue::Undefined(b"0100"), d_tbd,
     "Supported Flashpix version"),
    (ColorSpace, 0xa001, DefaultValue::Unspecified, d_tbd,
     "Color space information"),
    (PixelXDimension, 0xa002, DefaultValue::None, d_tbd,
     "Valid image width"),
    (PixelYDimension, 0xa003, DefaultValue::Unspecified, d_tbd,
     "Valid image height"),
    (RelatedSoundFile, 0xa004, DefaultValue::None, d_tbd,
     "Related audio file"),
    (FlashEnergy, 0xa20b, DefaultValue::None, d_tbd,
     "Flash energy"),
    (SpatialFrequencyResponse, 0xa20c, DefaultValue::None, d_tbd,
     "Spatial frequency response"),
    (FocalPlaneXResolution, 0xa20e, DefaultValue::None, d_tbd,
     "Focal plane X resolution"),
    (FocalPlaneYResolution, 0xa20f, DefaultValue::None, d_tbd,
     "Focal plane Y resolution"),
    (FocalPlaneResolutionUnit, 0xa210, DefaultValue::Short(&[2]), d_tbd,
     "Focal plane resolution unit"),
    (SubjectLocation, 0xa214, DefaultValue::None, d_tbd,
     "Subject location"),
    (ExposureIndex, 0xa215, DefaultValue::None, d_tbd,
     "Exposure index"),
    (SensingMethod, 0xa217, DefaultValue::None, d_tbd,
     "Sensing method"),
    (FileSource, 0xa300, DefaultValue::Undefined(&[3]), d_tbd,
     "File source"),
    (SceneType, 0xa301, DefaultValue::Undefined(&[1]), d_tbd,
     "Scene type"),
    (CFAPattern, 0xa302, DefaultValue::None, d_tbd,
     "CFA pattern"),
    (CustomRendered, 0xa401, DefaultValue::Short(&[0]), d_tbd,
     "Custom image processing"),
    (ExposureMode, 0xa402, DefaultValue::None, d_tbd,
     "Exposure mode"),
    (WhiteBalance, 0xa403, DefaultValue::None, d_tbd,
     "White balance"),
    (DigitalZoomRatio, 0xa404, DefaultValue::None, d_tbd,
     "Digital zoom ratio"),
    (FocalLengthIn35mmFilm, 0xa405, DefaultValue::None, d_tbd,
     "Focal length in 35 mm film"),
    (SceneCaptureType, 0xa406, DefaultValue::Short(&[0]), d_tbd,
     "Scene capture type"),
    (GainControl, 0xa407, DefaultValue::None, d_tbd,
     "Gain control"),
    (Contrast, 0xa408, DefaultValue::Short(&[0]), d_tbd,
     "Contrast"),
    (Saturation, 0xa409, DefaultValue::Short(&[0]), d_tbd,
     "Saturation"),
    (Sharpness, 0xa40a, DefaultValue::Short(&[0]), d_tbd,
     "Sharpness"),
    (DeviceSettingDescription, 0xa40b, DefaultValue::None, d_tbd,
     "Device settings description"),
    (SubjectDistanceRange, 0xa40c, DefaultValue::None, d_tbd,
     "Subject distance range"),
    (ImageUniqueID, 0xa420, DefaultValue::None, d_tbd,
     "Unique image ID"),
    (CameraOwnerName, 0xa430, DefaultValue::None, d_tbd,
     "Camera owner name"),
    (BodySerialNumber, 0xa431, DefaultValue::None, d_tbd,
     "Body serial number"),
    (LensSpecification, 0xa432, DefaultValue::None, d_tbd,
     "Lens specification"),
    (LensMake, 0xa433, DefaultValue::None, d_tbd,
     "Lens make"),
    (LensModel, 0xa434, DefaultValue::None, d_tbd,
     "Lens model"),
    (LensSerialNumber, 0xa435, DefaultValue::None, d_tbd,
     "Lens serial number"),
    (Gamma, 0xa500, DefaultValue::None, d_tbd,
     "Gamma"),

    // GPS attributes [EXIF23 4.6.6 Table 15 and 4.6.8 Table 19].
    |Context::Gps|

    // Depends on the Exif version.
    (GPSVersionID, 0x0, DefaultValue::ContextDependent, d_tbd,
     "GPS tag version"),
    (GPSLatitudeRef, 0x1, DefaultValue::None, d_tbd,
     "North or south latitude"),
    (GPSLatitude, 0x2, DefaultValue::None, d_tbd,
     "Latitude"),
    (GPSLongitudeRef, 0x3, DefaultValue::None, d_tbd,
     "East or West Longitude"),
    (GPSLongitude, 0x4, DefaultValue::None, d_tbd,
     "Longitude"),
    (GPSAltitudeRef, 0x5, DefaultValue::Byte(&[0]), d_tbd,
     "Altitude reference"),
    (GPSAltitude, 0x6, DefaultValue::None, d_tbd,
     "Altitude"),
    (GPSTimeStamp, 0x7, DefaultValue::None, d_tbd,
     "GPS time (atomic clock)"),
    (GPSSatellites, 0x8, DefaultValue::None, d_tbd,
     "GPS satellites used for measurement"),
    (GPSStatus, 0x9, DefaultValue::None, d_tbd,
     "GPS receiver status"),
    (GPSMeasureMode, 0xa, DefaultValue::None, d_tbd,
     "GPS measurement mode"),
    (GPSDOP, 0xb, DefaultValue::None, d_tbd,
     "Measurement precision"),
    (GPSSpeedRef, 0xc, DefaultValue::Ascii(&[b"K"]), d_tbd,
     "Speed unit"),
    (GPSSpeed, 0xd, DefaultValue::None, d_tbd,
     "Speed of GPS receiver"),
    (GPSTrackRef, 0xe, DefaultValue::Ascii(&[b"T"]), d_tbd,
     "Reference for direction of movement"),
    (GPSTrack, 0xf, DefaultValue::None, d_tbd,
     "Direction of movement"),
    (GPSImgDirectionRef, 0x10, DefaultValue::Ascii(&[b"T"]), d_tbd,
     "Reference for direction of image"),
    (GPSImgDirection, 0x11, DefaultValue::None, d_tbd,
     "Direction of image"),
    (GPSMapDatum, 0x12, DefaultValue::None, d_tbd,
     "Geodetic survey data used"),
    (GPSDestLatitudeRef, 0x13, DefaultValue::None, d_tbd,
     "Reference for latitude of destination"),
    (GPSDestLatitude, 0x14, DefaultValue::None, d_tbd,
     "Latitude of destination"),
    (GPSDestLongitudeRef, 0x15, DefaultValue::None, d_tbd,
     "Reference for longitude of destination"),
    (GPSDestLongitude, 0x16, DefaultValue::None, d_tbd,
     "Longitude of destination"),
    (GPSDestBearingRef, 0x17, DefaultValue::Ascii(&[b"T"]), d_tbd,
     "Reference for bearing of destination"),
    (GPSDestBearing, 0x18, DefaultValue::None, d_tbd,
     "Bearing of destination"),
    (GPSDestDistanceRef, 0x19, DefaultValue::Ascii(&[b"K"]), d_tbd,
     "Reference for distance to destination"),
    (GPSDestDistance, 0x1a, DefaultValue::None, d_tbd,
     "Distance to destination"),
    (GPSProcessingMethod, 0x1b, DefaultValue::None, d_tbd,
     "Name of GPS processing method"),
    (GPSAreaInformation, 0x1c, DefaultValue::None, d_tbd,
     "Name of GPS area"),
    (GPSDateStamp, 0x1d, DefaultValue::None, d_tbd,
     "GPS date"),
    (GPSDifferential, 0x1e, DefaultValue::None, d_tbd,
     "GPS differential correction"),
    (GPSHPositioningError, 0x1f, DefaultValue::None, d_tbd,
     "Horizontal positioning error"),

    // Interoperability attributes [EXIF23 4.6.7 Table 16 and 4.6.8 Table 20].
    |Context::Interop|

    (InteroperabilityIndex, 0x1, DefaultValue::None, d_tbd,
     "Interoperability identification"),
);

// For Value::display_as().
pub fn display_value_as<'a>(value: &'a Value, tag: Tag) -> value::Display<'a> {
    match get_tag_info(tag) {
        Some(ti) => value::Display { fmt: ti.dispval, value: value },
        None => value::Display { fmt: d_default, value: value },
    }
}

fn d_tbd(_w: &mut fmt::Write, _value: &Value) -> fmt::Result {
    unimplemented!();
}

fn d_default(w: &mut fmt::Write, value: &Value) -> fmt::Result {
    match *value {
        Value::Byte(ref v) => d_sub_comma(w, v),
        Value::Ascii(ref v) => {
            let mut first = true;
            for x in v {
                if !first {
                    try!(w.write_char('\n'));
                }
                first = false;
                try!(d_escape_ascii(w, x));
            }
            Ok(())
        },
        Value::Short(ref v) => d_sub_comma(w, v),
        Value::Long(ref v) => d_sub_comma(w, v),
        Value::Rational(ref v) => d_sub_comma(w, v),
        Value::SByte(ref v) => d_sub_comma(w, v),
        Value::Undefined(ref s) => d_sub_hex(w, s),
        Value::SShort(ref v) => d_sub_comma(w, v),
        Value::SLong(ref v) => d_sub_comma(w, v),
        Value::SRational(ref v) => d_sub_comma(w, v),
        Value::Float(ref v) => d_sub_comma(w, v),
        Value::Double(ref v) => d_sub_comma(w, v),
        Value::Unknown(t, c, o) =>
            write!(w, "unknown value (type={}, count={}, offset={:#x})",
                   t, c, o),
    }
}

fn d_sub_comma<T>(w: &mut fmt::Write, slice: &[T])
                  -> fmt::Result where T: fmt::Display {
    let mut first = true;
    for x in slice {
        try!(match first {
            true => write!(w, "{}", x),
            false => write!(w, ", {}", x),
        });
        first = false;
    }
    Ok(())
}

fn d_sub_hex(w: &mut fmt::Write, bytes: &[u8]) -> fmt::Result {
    try!(w.write_str("0x"));
    for x in bytes {
        try!(write!(w, "{:02x}", x));
    }
    Ok(())
}

fn d_escape_ascii(w: &mut fmt::Write, bytes: &[u8]) -> fmt::Result {
    for &c in bytes {
        match c {
            b'\\' => {
                try!(w.write_char('\\'));
                try!(w.write_char(c as char));
            },
            0x20...0x7e => try!(w.write_char(c as char)),
            _ => try!(write!(w, "\\x{:02x}", c)),
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use tag;
    use super::*;

    // This test checks if Tag constants can be used in patterns.
    #[test]
    fn tag_constant_in_pattern() {
        // Destructuring, which will always work.
        match Tag(Context::Tiff, 0x132) {
            Tag(Context::Tiff, 0x132) => {},
            _ => panic!("failed to match Tag"),
        }
        // Matching against a constant.  Test if this compiles.
        match Tag(Context::Tiff, 0x132) {
            tag::DateTime => {},
            _ => panic!("failed to match Tag"),
        }
    }

    #[test]
    fn default_value() {
        assert_pat!(tag::DateTime.default_value(), None);
        match tag::BitsPerSample.default_value() {
            Some(Value::Short(v)) => assert_eq!(v, &[8, 8, 8]),
            _ => panic!(),
        }
        match tag::XResolution.default_value() {
            Some(Value::Rational(v)) => {
                assert_eq!(v.len(), 1);
                assert_eq!(v[0].num, 72);
                assert_eq!(v[0].denom, 1);
            },
            _ => panic!(),
        }
        match tag::FileSource.default_value() {
            Some(Value::Undefined(v)) => assert_eq!(v, &[3]),
            _ => panic!(),
        }
        match tag::GPSAltitudeRef.default_value() {
            Some(Value::Byte(v)) => assert_eq!(v, &[0]),
            _ => panic!(),
        }
        match tag::GPSSpeedRef.default_value() {
            Some(Value::Ascii(v)) => assert_eq!(v, &[b"K"]),
            _ => panic!(),
        }
    }

    #[test]
    fn tag_fmt_display() {
        let tag1 = Tag(Context::Tiff, 0x132);
        assert_eq!(format!("{:15}", tag1), "DateTime       ");
        assert_eq!(format!("{:>15}", tag1), "       DateTime");
        assert_eq!(format!("{:5.6}", tag1), "DateTi");
        let tag2 = Tag(Context::Exif, 0);
        assert_eq!(format!("{:15}", tag2), "Tag(Exif, 0)   ");
        assert_eq!(format!("{:>15}", tag2), "   Tag(Exif, 0)");
        assert_eq!(format!("{:5.6}", tag2), "Tag(Ex");
    }
}
