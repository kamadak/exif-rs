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

use value::Value;

/// A tag of a TIFF field.
//
// This is not an enum to keep safety and API stability, while
// supporting unknown tag values.  This comment is based on the
// behavior of Rust 1.12.
// Storing unknown values in a repr(u16) enum is unsafe.  The compiler
// assumes that there is no undefined discriminant even with a C-like
// enum, so the exhaustiveness check of a match expression will break.
// Storing unknown values in a special variant such as Unknown(u16)
// tends to break backward compatibility.  When Tag::VariantFoo is
// defined in a new version of the library, the old codes using
// Tag::Unknown(Foo's value) will break.
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

    /// Returns the value of the tag.
    #[inline]
    pub fn value(self) -> u16 {
        self.1
    }

    /// Returns the description of the tag.
    #[inline]
    pub fn description(&self) -> Option<&str> {
        get_tag_info(self).map(|ti| ti.desc)
    }

    /// Returns the default value of the tag.  `None` is returned if
    /// it is not defined in the standard or it depends on the context.
    #[inline]
    pub fn default_value(&self) -> Option<Value> {
        get_tag_info(self).and_then(|ti| (&ti.default).into())
    }
}

impl fmt::Display for Tag {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match get_tag_info(self) {
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
            ($name:ident, $num:expr, $defval:expr, $desc:expr)
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
            use value::DefaultValue;

            pub struct TagInfo {
                pub name: &'static str,
                pub desc: &'static str,
                pub default: DefaultValue,
            }

            $($(
                #[allow(non_upper_case_globals)]
                pub static $name: TagInfo = TagInfo {
                    name: stringify!($name), desc: $desc, default: $defval };
            )+)+
        }

        fn get_tag_info(tag: &Tag) -> Option<&tag_info::TagInfo> {
            match *tag {
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
    (ExifIFDPointer, 0x8769, DefaultValue::None,
     "Exif IFD pointer"),
    /// A pointer to the GPS IFD.  This is used for the internal structure
    /// of Exif data and will not be returned to the user.
    (GPSInfoIFDPointer, 0x8825, DefaultValue::None,
     "GPS Info IFD pointer"),

    |Context::Exif|

    /// A pointer to the interoperability IFD.  This is used for the internal
    /// structure of Exif data and will not be returned to the user.
    (InteropIFDPointer, 0xa005, DefaultValue::None,
     "Interoperability IFD pointer"),

    // TIFF primary and thumbnail attributes [EXIF23 4.6.4 Table 4,
    // 4.6.8 Table 17, and 4.6.8 Table 21].
    |Context::Tiff|

    (ImageWidth, 0x100, DefaultValue::None,
     "Image width"),
    (ImageLength, 0x101, DefaultValue::None,
     "Image height"),
    (BitsPerSample, 0x102, DefaultValue::Short(&[8, 8, 8]),
     "Number of bits per component"),
    (Compression, 0x103, DefaultValue::None,
     "Compression scheme"),
    (PhotometricInterpretation, 0x106, DefaultValue::None,
     "Pixel composition"),
    (ImageDescription, 0x10e, DefaultValue::None,
     "Image title"),
    (Make, 0x10f, DefaultValue::None,
     "Manufacturer of image input equipment"),
    (Model, 0x110, DefaultValue::None,
     "Model of image input equipment"),
    (StripOffsets, 0x111, DefaultValue::None,
     "Image data location"),
    (Orientation, 0x112, DefaultValue::Short(&[1]),
     "Orientation of image"),
    (SamplesPerPixel, 0x115, DefaultValue::Short(&[3]),
     "Number of components"),
    (RowsPerStrip, 0x116, DefaultValue::None,
     "Number of rows per strip"),
    (StripByteCounts, 0x117, DefaultValue::None,
     "Bytes per compressed strip"),
    (XResolution, 0x11a, DefaultValue::Rational(&[(72, 1)]),
     "Image resolution in width direction"),
    (YResolution, 0x11b, DefaultValue::Rational(&[(72, 1)]),
     "Image resolution in height direction"),
    (PlanarConfiguration, 0x11c, DefaultValue::Short(&[1]),
     "Image data arrangement"),
    (ResolutionUnit, 0x128, DefaultValue::Short(&[2]),
     "Unit of X and Y resolution"),
    (TransferFunction, 0x12d, DefaultValue::None,
     "Transfer function"),
    (Software, 0x131, DefaultValue::None,
     "Software used"),
    (DateTime, 0x132, DefaultValue::None,
     "File change date and time"),
    (Artist, 0x13b, DefaultValue::None,
     "Person who created the image"),
    (WhitePoint, 0x13e, DefaultValue::None,
     "White point chromaticity"),
    (PrimaryChromaticities, 0x13f, DefaultValue::None,
     "Chromaticities of primaries"),
    (JPEGInterchangeFormat, 0x201, DefaultValue::None,
     "Offset to JPEG SOI"),
    (JPEGInterchangeFormatLength, 0x202, DefaultValue::None,
     "Bytes of JPEG data"),
    (YCbCrCoefficients, 0x211, DefaultValue::Unspecified,
     "Color space transformation matrix coefficients"),
    (YCbCrSubSampling, 0x212, DefaultValue::None,
     "Subsampling ratio of Y to C"),
    (YCbCrPositioning, 0x213, DefaultValue::Short(&[1]),
     "Y and C positioning"),
    (ReferenceBlackWhite, 0x214, DefaultValue::ContextDependent,
     "Pair of black and white reference values"),
    (Copyright, 0x8298, DefaultValue::None,
     "Copyright holder"),

    // Exif IFD attributes [EXIF23 4.6.5 Table 7 and 4.6.8 Table 18].
    |Context::Exif|

    (ExposureTime, 0x829a, DefaultValue::None,
     "Exposure time"),
    (FNumber, 0x829d, DefaultValue::None,
     "F number"),
    (ExposureProgram, 0x8822, DefaultValue::None,
     "Exposure program"),
    (SpectralSensitivity, 0x8824, DefaultValue::None,
     "Spectral sensitivity"),
    (PhotographicSensitivity, 0x8827, DefaultValue::None,
     "Photographic sensitivity"),
    (OECF, 0x8828, DefaultValue::None,
     "Optoelectric conversion factor"),
    (SensitivityType, 0x8830, DefaultValue::None,
     "Sensitivity type"),
    (StandardOutputSensitivity, 0x8831, DefaultValue::None,
     "Standard output sensitivity"),
    (RecommendedExposureIndex, 0x8832, DefaultValue::None,
     "Recommended exposure index"),
    (ISOSpeed, 0x8833, DefaultValue::None,
     "ISO speed"),
    (ISOSpeedLatitudeyyy, 0x8834, DefaultValue::None,
     "ISO speed latitude yyy"),
    (ISOSpeedLatitudezzz, 0x8835, DefaultValue::None,
     "ISO speed latitude zzz"),
    // The absence of this field means non-conformance to Exif, so the default
    // value specified in the standard (e.g., "0231") should not apply.
    (ExifVersion, 0x9000, DefaultValue::None,
     "Exif version"),
    (DateTimeOriginal, 0x9003, DefaultValue::None,
     "Date and time of original data generation"),
    (DateTimeDigitized, 0x9004, DefaultValue::None,
     "Date and time of digital data generation"),
    (OffsetTime, 0x9010, DefaultValue::None,
     "Offset data of DateTime"),
    (OffsetTimeOriginal, 0x9011, DefaultValue::None,
     "Offset data of DateTimeOriginal"),
    (OffsetTimeDigitized, 0x9012, DefaultValue::None,
     "Offset data of DateTimeDigitized"),
    (ComponentsConfiguration, 0x9101, DefaultValue::ContextDependent,
     "Meaning of each component"),
    (CompressedBitsPerPixel, 0x9102, DefaultValue::None,
     "Image compression mode"),
    (ShutterSpeedValue, 0x9201, DefaultValue::None,
     "Shutter speed"),
    (ApertureValue, 0x9202, DefaultValue::None,
     "Aperture"),
    (BrightnessValue, 0x9203, DefaultValue::None,
     "Brightness"),
    (ExposureBiasValue, 0x9204, DefaultValue::None,
     "Exposure bias"),
    (MaxApertureValue, 0x9205, DefaultValue::None,
     "Maximum lens aperture"),
    (SubjectDistance, 0x9206, DefaultValue::None,
     "Subject distance"),
    (MeteringMode, 0x9207, DefaultValue::Short(&[0]),
     "Metering mode"),
    (LightSource, 0x9208, DefaultValue::Short(&[0]),
     "Light source"),
    (Flash, 0x9209, DefaultValue::Unspecified,
     "Flash"),
    (FocalLength, 0x920a, DefaultValue::None,
     "Lens focal length"),
    (SubjectArea, 0x9214, DefaultValue::None,
     "Subject area"),
    (MakerNote, 0x927c, DefaultValue::None,
     "Manufacturer notes"),
    (UserComment, 0x9286, DefaultValue::None,
     "User comments"),
    (SubSecTime, 0x9290, DefaultValue::None,
     "DateTime subseconds"),
    (SubSecTimeOriginal, 0x9291, DefaultValue::None,
     "DateTimeOriginal subseconds"),
    (SubSecTimeDigitized, 0x9292, DefaultValue::None,
     "DateTimeDigitized subseconds"),
    (Temperature, 0x9400, DefaultValue::None,
     "Temperature"),
    (Humidity, 0x9401, DefaultValue::None,
     "Humidity"),
    (Pressure, 0x9402, DefaultValue::None,
     "Pressure"),
    (WaterDepth, 0x9403, DefaultValue::None,
     "Water depth"),
    (Acceleration, 0x9404, DefaultValue::None,
     "Acceleration"),
    (CameraElevationAngle, 0x9405, DefaultValue::None,
     "Camera elevation angle"),
    (FlashpixVersion, 0xa000, DefaultValue::Undefined(b"0100"),
     "Supported Flashpix version"),
    (ColorSpace, 0xa001, DefaultValue::Unspecified,
     "Color space information"),
    (PixelXDimension, 0xa002, DefaultValue::None,
     "Valid image width"),
    (PixelYDimension, 0xa003, DefaultValue::Unspecified,
     "Valid image height"),
    (RelatedSoundFile, 0xa004, DefaultValue::None,
     "Related audio file"),
    (FlashEnergy, 0xa20b, DefaultValue::None,
     "Flash energy"),
    (SpatialFrequencyResponse, 0xa20c, DefaultValue::None,
     "Spatial frequency response"),
    (FocalPlaneXResolution, 0xa20e, DefaultValue::None,
     "Focal plane X resolution"),
    (FocalPlaneYResolution, 0xa20f, DefaultValue::None,
     "Focal plane Y resolution"),
    (FocalPlaneResolutionUnit, 0xa210, DefaultValue::Short(&[2]),
     "Focal plane resolution unit"),
    (SubjectLocation, 0xa214, DefaultValue::None,
     "Subject location"),
    (ExposureIndex, 0xa215, DefaultValue::None,
     "Exposure index"),
    (SensingMethod, 0xa217, DefaultValue::None,
     "Sensing method"),
    (FileSource, 0xa300, DefaultValue::Undefined(&[3]),
     "File source"),
    (SceneType, 0xa301, DefaultValue::Undefined(&[1]),
     "Scene type"),
    (CFAPattern, 0xa302, DefaultValue::None,
     "CFA pattern"),
    (CustomRendered, 0xa401, DefaultValue::Short(&[0]),
     "Custom image processing"),
    (ExposureMode, 0xa402, DefaultValue::None,
     "Exposure mode"),
    (WhiteBalance, 0xa403, DefaultValue::None,
     "White balance"),
    (DigitalZoomRatio, 0xa404, DefaultValue::None,
     "Digital zoom ratio"),
    (FocalLengthIn35mmFilm, 0xa405, DefaultValue::None,
     "Focal length in 35 mm film"),
    (SceneCaptureType, 0xa406, DefaultValue::Short(&[0]),
     "Scene capture type"),
    (GainControl, 0xa407, DefaultValue::None,
     "Gain control"),
    (Contrast, 0xa408, DefaultValue::Short(&[0]),
     "Contrast"),
    (Saturation, 0xa409, DefaultValue::Short(&[0]),
     "Saturation"),
    (Sharpness, 0xa40a, DefaultValue::Short(&[0]),
     "Sharpness"),
    (DeviceSettingDescription, 0xa40b, DefaultValue::None,
     "Device settings description"),
    (SubjectDistanceRange, 0xa40c, DefaultValue::None,
     "Subject distance range"),
    (ImageUniqueID, 0xa420, DefaultValue::None,
     "Unique image ID"),
    (CameraOwnerName, 0xa430, DefaultValue::None,
     "Camera owner name"),
    (BodySerialNumber, 0xa431, DefaultValue::None,
     "Body serial number"),
    (LensSpecification, 0xa432, DefaultValue::None,
     "Lens specification"),
    (LensMake, 0xa433, DefaultValue::None,
     "Lens make"),
    (LensModel, 0xa434, DefaultValue::None,
     "Lens model"),
    (LensSerialNumber, 0xa435, DefaultValue::None,
     "Lens serial number"),
    (Gamma, 0xa500, DefaultValue::None,
     "Gamma"),

    // GPS attributes [EXIF23 4.6.6 Table 15 and 4.6.8 Table 19].
    |Context::Gps|

    // Depends on the Exif version.
    (GPSVersionID, 0x0, DefaultValue::ContextDependent,
     "GPS tag version"),
    (GPSLatitudeRef, 0x1, DefaultValue::None,
     "North or south latitude"),
    (GPSLatitude, 0x2, DefaultValue::None,
     "Latitude"),
    (GPSLongitudeRef, 0x3, DefaultValue::None,
     "East or West Longitude"),
    (GPSLongitude, 0x4, DefaultValue::None,
     "Longitude"),
    (GPSAltitudeRef, 0x5, DefaultValue::Byte(&[0]),
     "Altitude reference"),
    (GPSAltitude, 0x6, DefaultValue::None,
     "Altitude"),
    (GPSTimeStamp, 0x7, DefaultValue::None,
     "GPS time (atomic clock)"),
    (GPSSatellites, 0x8, DefaultValue::None,
     "GPS satellites used for measurement"),
    (GPSStatus, 0x9, DefaultValue::None,
     "GPS receiver status"),
    (GPSMeasureMode, 0xa, DefaultValue::None,
     "GPS measurement mode"),
    (GPSDOP, 0xb, DefaultValue::None,
     "Measurement precision"),
    (GPSSpeedRef, 0xc, DefaultValue::Ascii(&[b"K"]),
     "Speed unit"),
    (GPSSpeed, 0xd, DefaultValue::None,
     "Speed of GPS receiver"),
    (GPSTrackRef, 0xe, DefaultValue::Ascii(&[b"T"]),
     "Reference for direction of movement"),
    (GPSTrack, 0xf, DefaultValue::None,
     "Direction of movement"),
    (GPSImgDirectionRef, 0x10, DefaultValue::Ascii(&[b"T"]),
     "Reference for direction of image"),
    (GPSImgDirection, 0x11, DefaultValue::None,
     "Direction of image"),
    (GPSMapDatum, 0x12, DefaultValue::None,
     "Geodetic survey data used"),
    (GPSDestLatitudeRef, 0x13, DefaultValue::None,
     "Reference for latitude of destination"),
    (GPSDestLatitude, 0x14, DefaultValue::None,
     "Latitude of destination"),
    (GPSDestLongitudeRef, 0x15, DefaultValue::None,
     "Reference for longitude of destination"),
    (GPSDestLongitude, 0x16, DefaultValue::None,
     "Longitude of destination"),
    (GPSDestBearingRef, 0x17, DefaultValue::Ascii(&[b"T"]),
     "Reference for bearing of destination"),
    (GPSDestBearing, 0x18, DefaultValue::None,
     "Bearing of destination"),
    (GPSDestDistanceRef, 0x19, DefaultValue::Ascii(&[b"K"]),
     "Reference for distance to destination"),
    (GPSDestDistance, 0x1a, DefaultValue::None,
     "Distance to destination"),
    (GPSProcessingMethod, 0x1b, DefaultValue::None,
     "Name of GPS processing method"),
    (GPSAreaInformation, 0x1c, DefaultValue::None,
     "Name of GPS area"),
    (GPSDateStamp, 0x1d, DefaultValue::None,
     "GPS date"),
    (GPSDifferential, 0x1e, DefaultValue::None,
     "GPS differential correction"),
    (GPSHPositioningError, 0x1f, DefaultValue::None,
     "Horizontal positioning error"),

    // Interoperability attributes [EXIF23 4.6.7 Table 16 and 4.6.8 Table 20].
    |Context::Interop|

    (InteroperabilityIndex, 0x1, DefaultValue::None,
     "Interoperability identification"),
);

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
}
