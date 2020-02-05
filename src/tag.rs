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

use crate::value;
use crate::value::Value;
use crate::util::atou16;

/// A tag of a TIFF field.
///
/// Some well-known tags are provided as associated constants of
/// this type.  The constant names follow the Exif specification
/// but not the Rust naming conventions.
///
/// A non-predefined tag can also be specified
/// by the context and the number as in `Tag(Context::Tiff, 0x100)`.
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
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Tag(pub Context, pub u16);

impl Tag {
    /// Returns the context of the tag.
    ///
    /// # Examples
    /// ```
    /// use exif::{Context, Tag};
    /// assert_eq!(Tag::DateTime.context(), Context::Tiff);
    /// assert_eq!(Tag::ExposureTime.context(), Context::Exif);
    /// ```
    #[inline]
    pub fn context(self) -> Context {
        self.0
    }

    /// Returns the tag number.
    ///
    /// # Examples
    /// ```
    /// use exif::Tag;
    /// assert_eq!(Tag::DateTime.number(), 0x132);
    /// ```
    #[inline]
    pub fn number(self) -> u16 {
        self.1
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

    pub(crate) fn unit(self) -> Option<&'static [UnitPiece]> {
        get_tag_info(self).and_then(|ti| ti.unit)
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

/// An enum that indicates how a tag number is interpreted.
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[non_exhaustive]
pub enum Context {
    /// TIFF attributes defined in the TIFF Rev. 6.0 specification.
    Tiff,	// 0th/1st IFD (toplevel)
    /// Exif attributes.
    Exif,	// -- Exif IFD
    /// GPS attributes.
    Gps,	// -- GPS IFD
    /// Interoperability attributes.
    Interop,	// -- Exif IFD -- Interoperability IFD
}

#[derive(Debug)]
pub enum UnitPiece {
    Value,
    Str(&'static str),
    Tag(Tag),
}

macro_rules! unit {
    () => ( None );
    ( $str:literal ) => ( unit![V, concat!(" ", $str)] );
    ( Tag::$tag:ident ) => ( unit![V, " ", Tag::$tag] );
    ( $($tokens:tt)* ) => ( Some(unit_expand!( ; $($tokens)* , )) );
}

macro_rules! unit_expand {
    ( $($built:expr),* ; ) => ( &[$($built),*] );
    ( $($built:expr),* ; , ) => ( &[$($built),*] );
    ( $($built:expr),* ; V, $($rest:tt)* ) => (
        unit_expand!($($built,)* UnitPiece::Value ; $($rest)*) );
    ( $($built:expr),* ; $str:literal, $($rest:tt)* ) => (
        unit_expand!($($built,)* UnitPiece::Str($str) ; $($rest)*) );
    ( $($built:expr),* ; concat!($($strs:literal),*), $($rest:tt)* ) => (
        unit_expand!($($built,)* UnitPiece::Str(concat!($($strs),*)) ; $($rest)*) );
    ( $($built:expr),* ; Tag::$tag:ident, $($rest:tt)* ) => (
        unit_expand!($($built,)* UnitPiece::Tag(Tag::$tag) ; $($rest)*) );
}

macro_rules! generate_well_known_tag_constants {
    (
        $( |$ctx:path| $(
            // Copy the doc attribute to the actual definition.
            $( #[$attr:meta] )*
            ($name:ident, $num:expr, $defval:expr, $dispval:ident, $unit:expr,
             $desc:expr)
        ),+, )+
    ) => (
        // This is not relevant for associated constants, because
        // they cannot be imported even with "uniform paths".
        // /// It is not recommended to import the constants directly into
        // /// your namespace; import the module and use with the module name
        // /// like `tag::DateTime`.  The constant names follow the Exif
        // /// specification but not the Rust naming conventions, and a user
        // /// of the constants will get the non_upper_case_globals warning
        // /// if a bare constant is used in a match arm.
        // // This is discussed in
        // // <https://github.com/rust-lang/rust/issues/25207>.
        impl Tag {
            $($(
                $( #[$attr] )*
                #[allow(non_upper_case_globals)]
                pub const $name: Tag = Tag($ctx, $num);
            )+)+
        }

        mod tag_info {
            use std::fmt;
            use crate::value::Value;
            use crate::value::DefaultValue;
            use super::{Tag, UnitPiece};

            pub struct TagInfo {
                pub name: &'static str,
                pub desc: &'static str,
                pub default: DefaultValue,
                pub dispval: fn(&mut dyn fmt::Write, &Value) -> fmt::Result,
                pub unit: Option<&'static [UnitPiece]>,
            }

            $($(
                #[allow(non_upper_case_globals)]
                pub static $name: TagInfo = TagInfo {
                    name: stringify!($name),
                    desc: $desc,
                    default: $defval,
                    dispval: super::$dispval,
                    unit: $unit,
                };
            )+)+
        }

        fn get_tag_info(tag: Tag) -> Option<&'static tag_info::TagInfo> {
            match tag {
                $($(
                    Tag::$name => Some(&tag_info::$name),
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
    #[doc(hidden)]
    (ExifIFDPointer, 0x8769, DefaultValue::None, d_default,
     unit![],
     "Exif IFD pointer"),
    /// A pointer to the GPS IFD.  This is used for the internal structure
    /// of Exif data and will not be returned to the user.
    #[doc(hidden)]
    (GPSInfoIFDPointer, 0x8825, DefaultValue::None, d_default,
     unit![],
     "GPS Info IFD pointer"),

    |Context::Exif|

    /// A pointer to the interoperability IFD.  This is used for the internal
    /// structure of Exif data and will not be returned to the user.
    #[doc(hidden)]
    (InteropIFDPointer, 0xa005, DefaultValue::None, d_default,
     unit![],
     "Interoperability IFD pointer"),

    // TIFF primary and thumbnail attributes [EXIF23 4.6.4 Table 4,
    // 4.6.8 Table 17, and 4.6.8 Table 21].
    |Context::Tiff|

    (ImageWidth, 0x100, DefaultValue::None, d_default,
     unit!["pixels"],
     "Image width"),
    (ImageLength, 0x101, DefaultValue::None, d_default,
     unit!["pixels"],
     "Image height"),
    (BitsPerSample, 0x102, DefaultValue::Short(&[8, 8, 8]), d_default,
     unit![],
     "Number of bits per component"),
    (Compression, 0x103, DefaultValue::None, d_compression,
     unit![],
     "Compression scheme"),
    (PhotometricInterpretation, 0x106, DefaultValue::None, d_photointp,
     unit![],
     "Pixel composition"),
    (ImageDescription, 0x10e, DefaultValue::None, d_default,
     unit![],
     "Image title"),
    (Make, 0x10f, DefaultValue::None, d_default,
     unit![],
     "Manufacturer of image input equipment"),
    (Model, 0x110, DefaultValue::None, d_default,
     unit![],
     "Model of image input equipment"),
    (StripOffsets, 0x111, DefaultValue::None, d_default,
     unit![],
     "Image data location"),
    (Orientation, 0x112, DefaultValue::Short(&[1]), d_orientation,
     unit![],
     "Orientation of image"),
    (SamplesPerPixel, 0x115, DefaultValue::Short(&[3]), d_default,
     unit![],
     "Number of components"),
    (RowsPerStrip, 0x116, DefaultValue::None, d_default,
     unit![],
     "Number of rows per strip"),
    (StripByteCounts, 0x117, DefaultValue::None, d_default,
     unit![],
     "Bytes per compressed strip"),
    (XResolution, 0x11a, DefaultValue::Rational(&[(72, 1)]), d_decimal,
     unit![V, " pixels per ", Tag::ResolutionUnit],
     "Image resolution in width direction"),
    (YResolution, 0x11b, DefaultValue::Rational(&[(72, 1)]), d_decimal,
     unit![V, " pixels per ", Tag::ResolutionUnit],
     "Image resolution in height direction"),
    (PlanarConfiguration, 0x11c, DefaultValue::Short(&[1]), d_planarcfg,
     unit![],
     "Image data arrangement"),
    (ResolutionUnit, 0x128, DefaultValue::Short(&[2]), d_resunit,
     unit![],
     "Unit of X and Y resolution"),
    (TransferFunction, 0x12d, DefaultValue::None, d_default,
     unit![],
     "Transfer function"),
    (Software, 0x131, DefaultValue::None, d_default,
     unit![],
     "Software used"),
    (DateTime, 0x132, DefaultValue::None, d_datetime,
     unit![],
     "File change date and time"),
    (Artist, 0x13b, DefaultValue::None, d_default,
     unit![],
     "Person who created the image"),
    (WhitePoint, 0x13e, DefaultValue::None, d_decimal,
     unit![],
     "White point chromaticity"),
    (PrimaryChromaticities, 0x13f, DefaultValue::None, d_decimal,
     unit![],
     "Chromaticities of primaries"),
    // Not referenced in Exif.
    (TileOffsets, 0x144, DefaultValue::None, d_default,
     unit![],
     "Tiled image data location"),
    // Not referenced in Exif.
    (TileByteCounts, 0x145, DefaultValue::None, d_default,
     unit![],
     "Bytes per compressed tile"),
    (JPEGInterchangeFormat, 0x201, DefaultValue::None, d_default,
     unit![],
     "Offset to JPEG SOI"),
    (JPEGInterchangeFormatLength, 0x202, DefaultValue::None, d_default,
     unit![],
     "Bytes of JPEG data"),
    (YCbCrCoefficients, 0x211, DefaultValue::Unspecified, d_decimal,
     unit![],
     "Color space transformation matrix coefficients"),
    (YCbCrSubSampling, 0x212, DefaultValue::None, d_ycbcrsubsamp,
     unit![],
     "Subsampling ratio of Y to C"),
    (YCbCrPositioning, 0x213, DefaultValue::Short(&[1]), d_ycbcrpos,
     unit![],
     "Y and C positioning"),
    (ReferenceBlackWhite, 0x214, DefaultValue::ContextDependent, d_decimal,
     unit![],
     "Pair of black and white reference values"),
    (Copyright, 0x8298, DefaultValue::None, d_default,
     unit![],
     "Copyright holder"),

    // Exif IFD attributes [EXIF23 4.6.5 Table 7 and 4.6.8 Table 18].
    |Context::Exif|

    (ExposureTime, 0x829a, DefaultValue::None, d_exptime,
     unit!["s"],
     "Exposure time"),
    (FNumber, 0x829d, DefaultValue::None, d_decimal,
     // F-number is dimensionless, but usually prefixed with "F" in Japan,
     // "f/" in the U.S., and so on.
     unit!["f/", V],
     "F number"),
    (ExposureProgram, 0x8822, DefaultValue::None, d_expprog,
     unit![],
     "Exposure program"),
    (SpectralSensitivity, 0x8824, DefaultValue::None, d_default,
     unit![],
     "Spectral sensitivity"),
    (PhotographicSensitivity, 0x8827, DefaultValue::None, d_default,
     unit![],
     "Photographic sensitivity"),
    (OECF, 0x8828, DefaultValue::None, d_default,
     unit![],
     "Optoelectric conversion factor"),
    (SensitivityType, 0x8830, DefaultValue::None, d_sensitivitytype,
     unit![],
     "Sensitivity type"),
    (StandardOutputSensitivity, 0x8831, DefaultValue::None, d_default,
     unit![],
     "Standard output sensitivity"),
    (RecommendedExposureIndex, 0x8832, DefaultValue::None, d_default,
     unit![],
     "Recommended exposure index"),
    (ISOSpeed, 0x8833, DefaultValue::None, d_default,
     unit![],
     "ISO speed"),
    (ISOSpeedLatitudeyyy, 0x8834, DefaultValue::None, d_default,
     unit![],
     "ISO speed latitude yyy"),
    (ISOSpeedLatitudezzz, 0x8835, DefaultValue::None, d_default,
     unit![],
     "ISO speed latitude zzz"),
    // The absence of this field means non-conformance to Exif, so the default
    // value specified in the standard (e.g., "0231") should not apply.
    (ExifVersion, 0x9000, DefaultValue::None, d_exifver,
     unit![],
     "Exif version"),
    (DateTimeOriginal, 0x9003, DefaultValue::None, d_datetime,
     unit![],
     "Date and time of original data generation"),
    (DateTimeDigitized, 0x9004, DefaultValue::None, d_datetime,
     unit![],
     "Date and time of digital data generation"),
    (OffsetTime, 0x9010, DefaultValue::None, d_default,
     unit![],
     "Offset data of DateTime"),
    (OffsetTimeOriginal, 0x9011, DefaultValue::None, d_default,
     unit![],
     "Offset data of DateTimeOriginal"),
    (OffsetTimeDigitized, 0x9012, DefaultValue::None, d_default,
     unit![],
     "Offset data of DateTimeDigitized"),
    (ComponentsConfiguration, 0x9101, DefaultValue::ContextDependent, d_cpntcfg,
     unit![],
     "Meaning of each component"),
    (CompressedBitsPerPixel, 0x9102, DefaultValue::None, d_decimal,
     unit![],
     "Image compression mode"),
    (ShutterSpeedValue, 0x9201, DefaultValue::None, d_decimal,
     unit!["EV"],
     "Shutter speed"),
    (ApertureValue, 0x9202, DefaultValue::None, d_decimal,
     unit!["EV"],
     "Aperture"),
    (BrightnessValue, 0x9203, DefaultValue::None, d_decimal,
     unit!["EV"],
     "Brightness"),
    (ExposureBiasValue, 0x9204, DefaultValue::None, d_decimal,
     unit!["EV"],
     "Exposure bias"),
    (MaxApertureValue, 0x9205, DefaultValue::None, d_decimal,
     unit!["EV"],
     "Maximum lens aperture"),
    (SubjectDistance, 0x9206, DefaultValue::None, d_subjdist,
     unit!["m"],
     "Subject distance"),
    (MeteringMode, 0x9207, DefaultValue::Short(&[0]), d_metering,
     unit![],
     "Metering mode"),
    (LightSource, 0x9208, DefaultValue::Short(&[0]), d_lightsrc,
     unit![],
     "Light source"),
    (Flash, 0x9209, DefaultValue::Unspecified, d_flash,
     unit![],
     "Flash"),
    (FocalLength, 0x920a, DefaultValue::None, d_decimal,
     unit!["mm"],
     "Lens focal length"),
    (SubjectArea, 0x9214, DefaultValue::None, d_subjarea,
     unit![],
     "Subject area"),
    (MakerNote, 0x927c, DefaultValue::None, d_default,
     unit![],
     "Manufacturer notes"),
    (UserComment, 0x9286, DefaultValue::None, d_default,
     unit![],
     "User comments"),
    (SubSecTime, 0x9290, DefaultValue::None, d_default,
     unit![],
     "DateTime subseconds"),
    (SubSecTimeOriginal, 0x9291, DefaultValue::None, d_default,
     unit![],
     "DateTimeOriginal subseconds"),
    (SubSecTimeDigitized, 0x9292, DefaultValue::None, d_default,
     unit![],
     "DateTimeDigitized subseconds"),
    (Temperature, 0x9400, DefaultValue::None, d_optdecimal,
     unit!["degC"],
     "Temperature"),
    (Humidity, 0x9401, DefaultValue::None, d_optdecimal,
     unit!["%"],
     "Humidity"),
    (Pressure, 0x9402, DefaultValue::None, d_optdecimal,
     unit!["hPa"],
     "Pressure"),
    (WaterDepth, 0x9403, DefaultValue::None, d_optdecimal,
     unit!["m"],
     "Water depth"),
    (Acceleration, 0x9404, DefaultValue::None, d_optdecimal,
     unit!["mGal"],
     "Acceleration"),
    (CameraElevationAngle, 0x9405, DefaultValue::None, d_optdecimal,
     unit!["deg"],
     "Camera elevation angle"),
    (FlashpixVersion, 0xa000, DefaultValue::Undefined(b"0100"), d_exifver,
     unit![],
     "Supported Flashpix version"),
    (ColorSpace, 0xa001, DefaultValue::Unspecified, d_cspace,
     unit![],
     "Color space information"),
    (PixelXDimension, 0xa002, DefaultValue::None, d_default,
     unit!["pixels"],
     "Valid image width"),
    (PixelYDimension, 0xa003, DefaultValue::Unspecified, d_default,
     unit!["pixels"],
     "Valid image height"),
    (RelatedSoundFile, 0xa004, DefaultValue::None, d_default,
     unit![],
     "Related audio file"),
    (FlashEnergy, 0xa20b, DefaultValue::None, d_decimal,
     unit!["BCPS"],
     "Flash energy"),
    (SpatialFrequencyResponse, 0xa20c, DefaultValue::None, d_default,
     unit![],
     "Spatial frequency response"),
    (FocalPlaneXResolution, 0xa20e, DefaultValue::None, d_decimal,
     unit![V, " pixels per ", Tag::FocalPlaneResolutionUnit],
     "Focal plane X resolution"),
    (FocalPlaneYResolution, 0xa20f, DefaultValue::None, d_decimal,
     unit![V, " pixels per ", Tag::FocalPlaneResolutionUnit],
     "Focal plane Y resolution"),
    (FocalPlaneResolutionUnit, 0xa210, DefaultValue::Short(&[2]), d_resunit,
     unit![],
     "Focal plane resolution unit"),
    (SubjectLocation, 0xa214, DefaultValue::None, d_subjarea,
     unit![],
     "Subject location"),
    (ExposureIndex, 0xa215, DefaultValue::None, d_decimal,
     unit![],
     "Exposure index"),
    (SensingMethod, 0xa217, DefaultValue::None, d_sensingmethod,
     unit![],
     "Sensing method"),
    (FileSource, 0xa300, DefaultValue::Undefined(&[3]), d_filesrc,
     unit![],
     "File source"),
    (SceneType, 0xa301, DefaultValue::Undefined(&[1]), d_scenetype,
     unit![],
     "Scene type"),
    (CFAPattern, 0xa302, DefaultValue::None, d_default,
     unit![],
     "CFA pattern"),
    (CustomRendered, 0xa401, DefaultValue::Short(&[0]), d_customrendered,
     unit![],
     "Custom image processing"),
    (ExposureMode, 0xa402, DefaultValue::None, d_expmode,
     unit![],
     "Exposure mode"),
    (WhiteBalance, 0xa403, DefaultValue::None, d_whitebalance,
     unit![],
     "White balance"),
    (DigitalZoomRatio, 0xa404, DefaultValue::None, d_dzoomratio,
     unit![],
     "Digital zoom ratio"),
    (FocalLengthIn35mmFilm, 0xa405, DefaultValue::None, d_focallen35,
     unit!["mm"],
     "Focal length in 35 mm film"),
    (SceneCaptureType, 0xa406, DefaultValue::Short(&[0]), d_scenecaptype,
     unit![],
     "Scene capture type"),
    (GainControl, 0xa407, DefaultValue::None, d_gainctrl,
     unit![],
     "Gain control"),
    (Contrast, 0xa408, DefaultValue::Short(&[0]), d_contrast,
     unit![],
     "Contrast"),
    (Saturation, 0xa409, DefaultValue::Short(&[0]), d_saturation,
     unit![],
     "Saturation"),
    (Sharpness, 0xa40a, DefaultValue::Short(&[0]), d_sharpness,
     unit![],
     "Sharpness"),
    (DeviceSettingDescription, 0xa40b, DefaultValue::None, d_default,
     unit![],
     "Device settings description"),
    (SubjectDistanceRange, 0xa40c, DefaultValue::None, d_subjdistrange,
     unit![],
     "Subject distance range"),
    (ImageUniqueID, 0xa420, DefaultValue::None, d_default,
     unit![],
     "Unique image ID"),
    (CameraOwnerName, 0xa430, DefaultValue::None, d_default,
     unit![],
     "Camera owner name"),
    (BodySerialNumber, 0xa431, DefaultValue::None, d_default,
     unit![],
     "Body serial number"),
    (LensSpecification, 0xa432, DefaultValue::None, d_lensspec,
     unit![],
     "Lens specification"),
    (LensMake, 0xa433, DefaultValue::None, d_default,
     unit![],
     "Lens make"),
    (LensModel, 0xa434, DefaultValue::None, d_default,
     unit![],
     "Lens model"),
    (LensSerialNumber, 0xa435, DefaultValue::None, d_default,
     unit![],
     "Lens serial number"),
    (CompositeImage, 0xa460, DefaultValue::Short(&[0]), d_cpstimg,
     unit![],
     "Composite image"),
    (SourceImageNumberOfCompositeImage, 0xa461, DefaultValue::None, d_numcpstimg,
     unit![],
     "Source image number of composite image"),
    (SourceExposureTimesOfCompositeImage, 0xa462, DefaultValue::None, d_default,
     unit![],
     "Source exposure times of composite image"),
    (Gamma, 0xa500, DefaultValue::None, d_decimal,
     unit![],
     "Gamma"),

    // GPS attributes [EXIF23 4.6.6 Table 15 and 4.6.8 Table 19].
    |Context::Gps|

    // Depends on the Exif version.
    (GPSVersionID, 0x0, DefaultValue::ContextDependent, d_gpsver,
     unit![],
     "GPS tag version"),
    (GPSLatitudeRef, 0x1, DefaultValue::None, d_gpslatlongref,
     unit![],
     "North or south latitude"),
    (GPSLatitude, 0x2, DefaultValue::None, d_gpsdms,
     unit![Tag::GPSLatitudeRef],
     "Latitude"),
    (GPSLongitudeRef, 0x3, DefaultValue::None, d_gpslatlongref,
     unit![],
     "East or West Longitude"),
    (GPSLongitude, 0x4, DefaultValue::None, d_gpsdms,
     unit![Tag::GPSLongitudeRef],
     "Longitude"),
    (GPSAltitudeRef, 0x5, DefaultValue::Byte(&[0]), d_gpsaltref,
     unit![],
     "Altitude reference"),
    (GPSAltitude, 0x6, DefaultValue::None, d_decimal,
     unit![V, " meters ", Tag::GPSAltitudeRef],
     "Altitude"),
    (GPSTimeStamp, 0x7, DefaultValue::None, d_gpstimestamp,
     unit![],
     "GPS time (atomic clock)"),
    (GPSSatellites, 0x8, DefaultValue::None, d_default,
     unit![],
     "GPS satellites used for measurement"),
    (GPSStatus, 0x9, DefaultValue::None, d_gpsstatus,
     unit![],
     "GPS receiver status"),
    (GPSMeasureMode, 0xa, DefaultValue::None, d_gpsmeasuremode,
     unit![],
     "GPS measurement mode"),
    (GPSDOP, 0xb, DefaultValue::None, d_decimal,
     unit![],
     "Measurement precision"),
    (GPSSpeedRef, 0xc, DefaultValue::Ascii(&[b"K"]), d_gpsspeedref,
     unit![],
     "Speed unit"),
    (GPSSpeed, 0xd, DefaultValue::None, d_decimal,
     unit![Tag::GPSSpeedRef],
     "Speed of GPS receiver"),
    (GPSTrackRef, 0xe, DefaultValue::Ascii(&[b"T"]), d_gpsdirref,
     unit![],
     "Reference for direction of movement"),
    (GPSTrack, 0xf, DefaultValue::None, d_decimal,
     unit![V, " degrees in ", Tag::GPSTrackRef],
     "Direction of movement"),
    (GPSImgDirectionRef, 0x10, DefaultValue::Ascii(&[b"T"]), d_gpsdirref,
     unit![],
     "Reference for direction of image"),
    (GPSImgDirection, 0x11, DefaultValue::None, d_decimal,
     unit![V, " degrees in ", Tag::GPSImgDirectionRef],
     "Direction of image"),
    (GPSMapDatum, 0x12, DefaultValue::None, d_default,
     unit![],
     "Geodetic survey data used"),
    (GPSDestLatitudeRef, 0x13, DefaultValue::None, d_gpslatlongref,
     unit![],
     "Reference for latitude of destination"),
    (GPSDestLatitude, 0x14, DefaultValue::None, d_gpsdms,
     unit![Tag::GPSDestLatitudeRef],
     "Latitude of destination"),
    (GPSDestLongitudeRef, 0x15, DefaultValue::None, d_gpslatlongref,
     unit![],
     "Reference for longitude of destination"),
    (GPSDestLongitude, 0x16, DefaultValue::None, d_gpsdms,
     unit![Tag::GPSDestLongitudeRef],
     "Longitude of destination"),
    (GPSDestBearingRef, 0x17, DefaultValue::Ascii(&[b"T"]), d_gpsdirref,
     unit![],
     "Reference for bearing of destination"),
    (GPSDestBearing, 0x18, DefaultValue::None, d_decimal,
     unit![V, " degrees in ", Tag::GPSDestBearingRef],
     "Bearing of destination"),
    (GPSDestDistanceRef, 0x19, DefaultValue::Ascii(&[b"K"]), d_gpsdistref,
     unit![],
     "Reference for distance to destination"),
    (GPSDestDistance, 0x1a, DefaultValue::None, d_decimal,
     unit![Tag::GPSDestDistanceRef],
     "Distance to destination"),
    (GPSProcessingMethod, 0x1b, DefaultValue::None, d_ascii_in_undef,
     unit![],
     "Name of GPS processing method"),
    (GPSAreaInformation, 0x1c, DefaultValue::None, d_default,
     unit![],
     "Name of GPS area"),
    (GPSDateStamp, 0x1d, DefaultValue::None, d_gpsdatestamp,
     unit![],
     "GPS date"),
    (GPSDifferential, 0x1e, DefaultValue::None, d_gpsdifferential,
     unit![],
     "GPS differential correction"),
    (GPSHPositioningError, 0x1f, DefaultValue::None, d_decimal,
     unit!["m"],
     "Horizontal positioning error"),

    // Interoperability attributes [EXIF23 4.6.7 Table 16 and 4.6.8 Table 20].
    |Context::Interop|

    (InteroperabilityIndex, 0x1, DefaultValue::None, d_default,
     unit![],
     "Interoperability identification"),
);

// For Value::display_as().
pub fn display_value_as<'a>(value: &'a Value, tag: Tag) -> value::Display<'a> {
    match get_tag_info(tag) {
        Some(ti) => value::Display { fmt: ti.dispval, value: value },
        None => value::Display { fmt: d_default, value: value },
    }
}

// Compression (TIFF 0x103)
fn d_compression(w: &mut dyn fmt::Write, value: &Value) -> fmt::Result {
    let s = match value.get_uint(0) {
        Some(1) => "uncompressed",
        Some(2) => "Modified Huffman",
        Some(6) => "JPEG",
        Some(32773) => "PackBits",
        _ => return d_unknown(w, value, "unknown compression "),
    };
    w.write_str(s)
}

// PhotometricInterpretation (TIFF 0x106)
fn d_photointp(w: &mut dyn fmt::Write, value: &Value) -> fmt::Result {
    let s = match value.get_uint(0) {
        Some(0) => "white is zero",
        Some(1) => "black is zero",
        Some(2) => "RGB",
        Some(3) => "palette color",
        Some(4) => "transparency mask",
        Some(6) => "YCbCr",
        _ => return d_unknown(w, value, "unknown photometric interpretation "),
    };
    w.write_str(s)
}

// Orientation (TIFF 0x112)
fn d_orientation(w: &mut dyn fmt::Write, value: &Value) -> fmt::Result {
    let s = match value.get_uint(0) {
        Some(1) => "row 0 at top and column 0 at left",
        Some(2) => "row 0 at top and column 0 at right",
        Some(3) => "row 0 at bottom and column 0 at right",
        Some(4) => "row 0 at bottom and column 0 at left",
        Some(5) => "row 0 at left and column 0 at top",
        Some(6) => "row 0 at right and column 0 at top",
        Some(7) => "row 0 at right and column 0 at bottom",
        Some(8) => "row 0 at left and column 0 at bottom",
        _ => return d_unknown(w, value, "unknown orientation "),
    };
    w.write_str(s)
}

// PlanarConfiguration (TIFF 0x11c)
fn d_planarcfg(w: &mut dyn fmt::Write, value: &Value) -> fmt::Result {
    let s = match value.get_uint(0) {
        Some(1) => "chunky",
        Some(2) => "planar",
        _ => return d_unknown(w, value, "unknown planar configuration "),
    };
    w.write_str(s)
}

// ResolutionUnit (TIFF 0x128)
fn d_resunit(w: &mut dyn fmt::Write, value: &Value) -> fmt::Result {
    let s = match value.get_uint(0) {
        Some(1) => "no absolute unit",
        Some(2) => "inch",
        Some(3) => "cm",
        _ => return d_unknown(w, value, "unknown unit "),
    };
    w.write_str(s)
}

// DateTime (TIFF 0x132), DateTimeOriginal (Exif 0x9003), and
// DateTimeDigitized (Exif 0x9004)
fn d_datetime(w: &mut dyn fmt::Write, value: &Value) -> fmt::Result {
    if let Value::Ascii(ref v) = *value {
        if let Some(dt) = v.first() {
            if let Ok(dt) = crate::tiff::DateTime::from_ascii(dt) {
                return write!(w, "{}", dt)
            }
        }
    }
    d_default(w, value)
}

// YCbCrSubSampling (TIFF 0x212)
fn d_ycbcrsubsamp(w: &mut dyn fmt::Write, value: &Value) -> fmt::Result {
    let horiz = value.get_uint(0).unwrap_or(0);
    let vert = value.get_uint(1).unwrap_or(0);
    let s = match (horiz, vert) {
        (1, 1) => "full horizontally, full vertically (4:4:4)",
        (1, 2) => "full horizontally, half vertically",
        (1, 4) => "full horizontally, quarter vertically",
        (2, 1) => "half horizontally, full vertically (4:2:2)",
        (2, 2) => "half horizontally, half vertically (4:2:0)",
        (2, 4) => "half horizontally, quarter vertically",
        (4, 1) => "quarter horizontally, full vertically (4:1:1)",
        (4, 2) => "quarter horizontally, half vertically",
        (4, 4) => "quarter horizontally, quarter vertically",
        _ => return d_default(w, value),
    };
    w.write_str(s)
}

// YCbCrPositioning (TIFF 0x213)
fn d_ycbcrpos(w: &mut dyn fmt::Write, value: &Value) -> fmt::Result {
    let s = match value.get_uint(0) {
        Some(1) => "centered",
        Some(2) => "co-sited",
        _ => return d_unknown(w, value, "unknown YCbCr positioning "),
    };
    w.write_str(s)
}

// ExposureTime (Exif 0x829a)
fn d_exptime(w: &mut dyn fmt::Write, value: &Value) -> fmt::Result {
    if let Value::Rational(ref v) = *value {
        if let Some(et) = v.first() {
            if et.num >= et.denom {
                return write!(w, "{}", et.to_f64());
            } else if et.num != 0 {
                return write!(w, "1/{}", et.denom as f64 / et.num as f64);
            }
        }
    }
    d_default(w, value)
}

// ExposureProgram (Exif 0x8822)
fn d_expprog(w: &mut dyn fmt::Write, value: &Value) -> fmt::Result {
    let s = match value.get_uint(0) {
        Some(1) => "manual",
        Some(2) => "normal program",
        Some(3) => "aperture priority",
        Some(4) => "shutter priority",
        Some(5) => "creative program",
        Some(6) => "action program",
        Some(7) => "portrait mode",
        Some(8) => "landscape mode",
        _ => return d_unknown(w, value, "unknown exposure program "),
    };
    w.write_str(s)
}

// SensitivityType (Exif 0x8830)
fn d_sensitivitytype(w: &mut dyn fmt::Write, value: &Value) -> fmt::Result {
    let s = match value.get_uint(0) {
        Some(1) => "SOS",
        Some(2) => "REI",
        Some(3) => "ISO speed",
        Some(4) => "SOS/REI",
        Some(5) => "SOS/ISO speed",
        Some(6) => "REI/ISO speed",
        Some(7) => "SOS/REI/ISO speed",
        _ => return d_unknown(w, value, "unknown sensitivity type "),
    };
    w.write_str(s)
}

// ExifVersion (Exif 0x9000), FlashpixVersion (Exif 0xa000)
fn d_exifver(w: &mut dyn fmt::Write, value: &Value) -> fmt::Result {
    if let Value::Undefined(ref v, _) = *value {
        if v.len() == 4 {
            if let Ok(major) = atou16(&v[0..2]) {
                if let Ok(minor) = atou16(&v[2..4]) {
                    if minor % 10 == 0 {
                        return write!(w, "{}.{}", major, minor / 10);
                    } else {
                        return write!(w, "{}.{:02}", major, minor);
                    }
                }
            }
        }
    }
    d_default(w, value)
}

// ComponentsConfiguration (Exif 0x9101)
fn d_cpntcfg(w: &mut dyn fmt::Write, value: &Value) -> fmt::Result {
    if let Value::Undefined(ref v, _) = *value {
        for x in v {
            match x {
                0 => w.write_char('_'),
                1 => w.write_char('Y'),
                2 => w.write_str("Cb"),
                3 => w.write_str("Cr"),
                4 => w.write_char('R'),
                5 => w.write_char('G'),
                6 => w.write_char('B'),
                _ => w.write_char('?'),
            }?;
        }
        return Ok(());
    }
    d_default(w, value)
}

// SubjectDistance (Exif 0x9206)
fn d_subjdist(w: &mut dyn fmt::Write, value: &Value) -> fmt::Result {
    if let Value::Rational(ref v) = *value {
        if let Some(dist) = v.first() {
            if dist.num == 0 {
                return w.write_str("unknown");
            } else if dist.num == 0xffffffff {
                return w.write_str("infinity");
            }
        }
    }
    d_decimal(w, value)
}

// MeteringMode (Exif 0x9207)
fn d_metering(w: &mut dyn fmt::Write, value: &Value) -> fmt::Result {
    let s = match value.get_uint(0) {
        Some(1) => "average",
        Some(2) => "center-weighted average",
        Some(3) => "spot",
        Some(4) => "multi-spot",
        Some(5) => "pattern",
        Some(6) => "partial",
        Some(255) => "other",
        _ => return d_unknown(w, value, "unknown metering mode "),
    };
    w.write_str(s)
}

// LightSource (Exif 0x9208)
fn d_lightsrc(w: &mut dyn fmt::Write, value: &Value) -> fmt::Result {
    let s = match value.get_uint(0) {
        Some(1) => "daylight",
        Some(2) => "fluorescent",
        Some(3) => "tungsten",
        Some(4) => "flash",
        Some(9) => "fine weather",
        Some(10) => "cloudy weather",
        Some(11) => "shade",
        Some(12) => "daylight fluorescent (D 5700-7100K)",
        Some(13) => "day white fluorescent (N 4600-5500K)",
        Some(14) => "cool white fluorescent (W 3800-4500K)",
        Some(15) => "white fluorescent (WW 3250-3800K)",
        Some(16) => "warm white fluorescent (L 2600-3250K)",
        Some(17) => "standard light A",
        Some(18) => "standard light B",
        Some(19) => "standard light C",
        Some(20) => "D55",
        Some(21) => "D65",
        Some(22) => "D75",
        Some(23) => "D50",
        Some(24) => "ISO studio tungsten",
        Some(255) => "other",
        _ => return d_unknown(w, value, "unknown light source "),
    };
    w.write_str(s)
}

// Flash (Exif 0x9209)
fn d_flash(w: &mut dyn fmt::Write, value: &Value) -> fmt::Result {
    const FIRED: &[&str] = &["not fired", "fired"];
    const RETURN: &[&str] = &[
        ", no return light detection function",
        ", return light status 1 (reserved)",
        ", return light not detected",
        ", return light detected",
    ];
    const AUTO: &[&str] = &[
        ", auto mode 0 (unknown)", ", forced", ", suppressed", ", auto"];
    const FUNCTION: &[&str] = &["", ", no function present"];
    const RED_EYE: &[&str] = &["", ", red-eye reduction"];

    if let Some(v) = value.get_uint(0) {
        write!(w, "{}{}{}{}{}{}",
               FIRED[v as usize & 1],
               RETURN[v as usize >> 1 & 3],
               AUTO[v as usize >> 3 & 3],
               FUNCTION[v as usize >> 5 & 1],
               RED_EYE[v as usize >> 6 & 1],
               if v >> 7 != 0 { ", unknown MSB bits" } else { "" })
    } else {
        d_default(w, value)
    }
}

// SubjectArea (Exif 0x9214), SubjectLocation (Exif 0xa214)
// Only (x, y) case is valid for SubjectLocation.
fn d_subjarea(w: &mut dyn fmt::Write, value: &Value) -> fmt::Result {
    if let Some(x) = value.get_uint(0) {
        if let Some(y) = value.get_uint(1) {
            if let Some(d) = value.get_uint(2) {
                if let Some(h) = value.get_uint(3) {
                    return write!(w, "rectangle (x={}, y={}, w={}, h={})",
                                  x, y, d, h);
                }
                return write!(w, "circle (x={}, y={}, d={})", x, y, d);
            }
            return write!(w, "point (x={}, y={})", x, y);
        }
    }
    d_default(w, value)
}

// Rational/SRational with 0xffffffff being unknown.
// Temperature (Exif 0x9400), Humidity (Exif 0x9401),
// Pressure (Exif 0x9402), WaterDepth (Exif 0x9403),
// Acceleration (Exif 0x9404), CameraElevationAngle (Exif 0x9405)
fn d_optdecimal(w: &mut dyn fmt::Write, value: &Value) -> fmt::Result {
    match *value {
        Value::Rational(ref v) if v.len() > 0 =>
            if v[0].denom != 0xffffffff {
                write!(w, "{}", v[0].to_f64())
            } else {
                w.write_str("unknown")
            },
        Value::SRational(ref v) if v.len() > 0 =>
            if v[0].denom != -1 {
                write!(w, "{}", v[0].to_f64())
            } else {
                w.write_str("unknown")
            },
        _ => d_decimal(w, value),
    }
}

// ColorSpace (Exif 0xa001)
fn d_cspace(w: &mut dyn fmt::Write, value: &Value) -> fmt::Result {
    let s = match value.get_uint(0) {
        Some(1) => "sRGB",
        Some(0xffff) => "uncalibrated",
        _ => return d_unknown(w, value, "unknown color space "),
    };
    w.write_str(s)
}

// SensingMethod (Exif 0xa217)
fn d_sensingmethod(w: &mut dyn fmt::Write, value: &Value) -> fmt::Result {
    let s = match value.get_uint(0) {
        Some(1) => "not defined",
        Some(2) => "one-chip color area sensor",
        Some(3) => "two-chip color area sensor",
        Some(4) => "three-chip color area sensor",
        Some(5) => "color sequential area sensor",
        Some(7) => "trilinear sensor",
        Some(8) => "color sequential linear sensor",
        _ => return d_unknown(w, value, "unknown sensing method "),
    };
    w.write_str(s)
}

// FileSource (Exif 0xa300)
fn d_filesrc(w: &mut dyn fmt::Write, value: &Value) -> fmt::Result {
    let s = match match *value {
        Value::Undefined(ref v, _) => v.first().map(|&x| x),
        _ => None,
    } {
        Some(0) => "others",
        Some(1) => "transparency scanner",
        Some(2) => "reflective scanner",
        Some(3) => "DSC",
        _ => return d_unknown(w, value, "unknown file source "),
    };
    w.write_str(s)
}

// SceneType (Exif 0xa301)
fn d_scenetype(w: &mut dyn fmt::Write, value: &Value) -> fmt::Result {
    let s = match match *value {
        Value::Undefined(ref v, _) => v.first().map(|&x| x),
        _ => None,
    } {
        Some(1) => "directly photographed image",
        _ => return d_unknown(w, value, "unknown scene type "),
    };
    w.write_str(s)
}

// CustomRendered (Exif 0xa401)
fn d_customrendered(w: &mut dyn fmt::Write, value: &Value) -> fmt::Result {
    let s = match value.get_uint(0) {
        Some(0) => "normal process",
        Some(1) => "custom process",
        _ => return d_unknown(w, value, "unknown custom rendered "),
    };
    w.write_str(s)
}

// ExposureMode (Exif 0xa402)
fn d_expmode(w: &mut dyn fmt::Write, value: &Value) -> fmt::Result {
    let s = match value.get_uint(0) {
        Some(0) => "auto exposure",
        Some(1) => "manual exposure",
        Some(2) => "auto bracket",
        _ => return d_unknown(w, value, "unknown exposure mode "),
    };
    w.write_str(s)
}

// WhiteBalance (Exif 0xa403)
fn d_whitebalance(w: &mut dyn fmt::Write, value: &Value) -> fmt::Result {
    let s = match value.get_uint(0) {
        Some(0) => "auto white balance",
        Some(1) => "manual white balance",
        _ => return d_unknown(w, value, "unknown white balance mode "),
    };
    w.write_str(s)
}

// DigitalZoomRatio (Exif 0xa404)
fn d_dzoomratio(w: &mut dyn fmt::Write, value: &Value) -> fmt::Result {
    if let Value::Rational(ref v) = *value {
        if v.len() > 0 && v[0].num == 0 {
            return w.write_str("unused");
        }
    }
    d_decimal(w, value)
}

// FocalLengthIn35mmFilm (Exif 0xa405)
fn d_focallen35(w: &mut dyn fmt::Write, value: &Value) -> fmt::Result {
    match value.get_uint(0) {
        Some(0) => w.write_str("unknown"),
        _ => d_default(w, value),
    }
}

// SceneCaptureType (Exif 0xa406)
fn d_scenecaptype(w: &mut dyn fmt::Write, value: &Value) -> fmt::Result {
    let s = match value.get_uint(0) {
        Some(0) => "standard",
        Some(1) => "landscape",
        Some(2) => "portrait",
        Some(3) => "night scene",
        _ => return d_unknown(w, value, "unknown scene capture type "),
    };
    w.write_str(s)
}

// GainControl (Exif 0xa407)
fn d_gainctrl(w: &mut dyn fmt::Write, value: &Value) -> fmt::Result {
    let s = match value.get_uint(0) {
        Some(0) => "none",
        Some(1) => "low gain up",
        Some(2) => "high gain up",
        Some(3) => "low gain down",
        Some(4) => "high gain down",
        _ => return d_unknown(w, value, "unknown gain control "),
    };
    w.write_str(s)
}

// Contrast (Exif 0xa408)
fn d_contrast(w: &mut dyn fmt::Write, value: &Value) -> fmt::Result {
    let s = match value.get_uint(0) {
        Some(0) => "normal",
        Some(1) => "soft",
        Some(2) => "hard",
        _ => return d_unknown(w, value, "unknown contrast processing "),
    };
    w.write_str(s)
}

// Saturation (Exif 0xa409)
fn d_saturation(w: &mut dyn fmt::Write, value: &Value) -> fmt::Result {
    let s = match value.get_uint(0) {
        Some(0) => "normal",
        Some(1) => "low saturation",
        Some(2) => "high saturation",
        _ => return d_unknown(w, value, "unknown saturation processing "),
    };
    w.write_str(s)
}

// Sharpness (Exif 0xa40a)
fn d_sharpness(w: &mut dyn fmt::Write, value: &Value) -> fmt::Result {
    let s = match value.get_uint(0) {
        Some(0) => "normal",
        Some(1) => "soft",
        Some(2) => "hard",
        _ => return d_unknown(w, value, "unknown sharpness processing "),
    };
    w.write_str(s)
}

// SubjectDistanceRange (Exif 0xa40c)
fn d_subjdistrange(w: &mut dyn fmt::Write, value: &Value) -> fmt::Result {
    let s = match value.get_uint(0) {
        Some(1) => "macro",
        Some(2) => "close view",
        Some(3) => "distant view",
        _ => return d_unknown(w, value, "unknown subject distance range "),
    };
    w.write_str(s)
}

// LensSpecification (Exif 0xa432)
fn d_lensspec(w: &mut dyn fmt::Write, value: &Value) -> fmt::Result {
    match *value {
        Value::Rational(ref v) if v.len() >= 4 =>
            // There are several notations: "F1.4" in Japan, "f/1.4"
            // in the U.S., and so on.
            write!(w, "{}-{} mm, f/{}-{}",
                   v[0].to_f64(), v[1].to_f64(),
                   v[2].to_f64(), v[3].to_f64()),
        _ => d_default(w, value),
    }
}

// CompositeImage (Exif 0xa460)
fn d_cpstimg(w: &mut dyn fmt::Write, value: &Value) -> fmt::Result {
    let s = match value.get_uint(0) {
        Some(1) => "non-composite",
        Some(2) => "composite (general)",
        Some(3) => "composite (at the moment of shooting)",
        _ => return d_unknown(w, value, "unknown composite image "),
    };
    w.write_str(s)
}

// SourceImageNumberOfCompositeImage (Exif 0xa461)
fn d_numcpstimg(w: &mut dyn fmt::Write, value: &Value) -> fmt::Result {
    match (value.get_uint(0), value.get_uint(1)) {
        (Some(t), Some(u)) => write!(w, "total {}, used {}", t, u),
        _ => d_unknown(w, value, "unknown image number of composite imsage "),
    }
}

// GPSVersionID (Exif/GPS 0x0)
fn d_gpsver(w: &mut dyn fmt::Write, value: &Value) -> fmt::Result {
    match *value {
        Value::Byte(ref v) if v.len() >= 4 =>
            write!(w, "{}.{}.{}.{}", v[0], v[1], v[2], v[3]),
        _ => d_default(w, value),
    }
}

// GPSLatitudeRef (Exif/GPS 0x1), GPSLongitudeRef (Exif/GPS 0x3)
// GPSDestLatitudeRef (Exif/GPS 0x13), GPSDestLongitudeRef (Exif/GPS 0x15)
fn d_gpslatlongref(w: &mut dyn fmt::Write, value: &Value) -> fmt::Result {
    match *value {
        Value::Ascii(ref v) if (v.len() == 1 && v[0].len() == 1 &&
                                v[0][0].is_ascii_uppercase()) =>
            w.write_char(v[0][0] as char),
        _ => d_default(w, value),
    }
}

// GPSLatitude (Exif/GPS 0x2), GPSLongitude (Exif/GPS 0x4),
// GPSDestLatitude (Exif/GPS 0x14), GPSDestLongitude (Exif/GPS 0x16)
fn d_gpsdms(w: &mut dyn fmt::Write, value: &Value) -> fmt::Result {
    match *value {
        Value::Rational(ref v) if v.len() >= 3 =>
            write!(w, "{} deg {} min {} sec",
                   v[0].to_f64(), v[1].to_f64(), v[2].to_f64()),
        _ => d_default(w, value),
    }
}

// GPSAltitudeRef (Exif/GPS 0x5)
fn d_gpsaltref(w: &mut dyn fmt::Write, value: &Value) -> fmt::Result {
    let s = match value.get_uint(0) {
        Some(0) => "above sea level",
        Some(1) => "below sea level",
        _ => return d_unknown(w, value, "unknown GPS altitude ref "),
    };
    w.write_str(s)
}

// GPSTimeStamp (Exif/GPS 0x7)
fn d_gpstimestamp(w: &mut dyn fmt::Write, value: &Value) -> fmt::Result {
    match *value {
        Value::Rational(ref v) if v.len() >= 3 => {
            let (h, m, s) = (v[0].to_f64(), v[1].to_f64(), v[2].to_f64());
            write!(w, "{}{}:{}{}:{}{}",
                   if h < 10.0 { "0" } else { "" }, h,
                   if m < 10.0 { "0" } else { "" }, m,
                   if s < 10.0 { "0" } else { "" }, s)
        },
        _ => d_default(w, value),
    }
}

// GPSStatus (Exif/GPS 0x9)
fn d_gpsstatus(w: &mut dyn fmt::Write, value: &Value) -> fmt::Result {
    let s = match match *value {
        Value::Ascii(ref v) => v.first().map(|x| &x[..]),
        _ => None,
    } {
        Some(b"A") => "measurement in progress",
        Some(b"V") => "measurement interrupted",
        _ => return d_unknown(w, value, "unknown GPS status "),
    };
    w.write_str(s)
}

// GPSMeasure (Exif/GPS 0xa)
fn d_gpsmeasuremode(w: &mut dyn fmt::Write, value: &Value) -> fmt::Result {
    let s = match match *value {
        Value::Ascii(ref v) => v.first().map(|x| &x[..]),
        _ => None,
    } {
        Some(b"2") => "2-dimensional measurement",
        Some(b"3") => "3-dimensional measurement",
        _ => return d_unknown(w, value, "unknown GPS measurement mode "),
    };
    w.write_str(s)
}

// GPSSpeedRef (Exif/GPS 0xc)
fn d_gpsspeedref(w: &mut dyn fmt::Write, value: &Value) -> fmt::Result {
    let s = match match *value {
        Value::Ascii(ref v) => v.first().map(|x| &x[..]),
        _ => None,
    } {
        Some(b"K") => "km/h",
        Some(b"M") => "mph",
        Some(b"N") => "knots",
        _ => return d_unknown(w, value, "unknown GPS speed ref "),
    };
    w.write_str(s)
}

// GPSTrackRef (Exif/GPS 0xe), GPSImgDirectionRef (Exif/GPS 0x10),
// GPSDestBearingRef (Exif/GPS 0x17)
fn d_gpsdirref(w: &mut dyn fmt::Write, value: &Value) -> fmt::Result {
    let s = match match *value {
        Value::Ascii(ref v) => v.first().map(|x| &x[..]),
        _ => None,
    } {
        Some(b"T") => "true direction",
        Some(b"M") => "magnetic direction",
        _ => return d_unknown(w, value, "unknown GPS direction ref "),
    };
    w.write_str(s)
}

// GPSDestDistanceRef (Exif/GPS 0x19)
fn d_gpsdistref(w: &mut dyn fmt::Write, value: &Value) -> fmt::Result {
    let s = match match *value {
        Value::Ascii(ref v) => v.first().map(|x| &x[..]),
        _ => None,
    } {
        Some(b"K") => "km",
        Some(b"M") => "miles",
        Some(b"N") => "nautical miles",
        _ => return d_unknown(w, value, "unknown GPS distance ref "),
    };
    w.write_str(s)
}

// GPSDateStamp (Exif/GPS 0x1d)
fn d_gpsdatestamp(w: &mut dyn fmt::Write, value: &Value) -> fmt::Result {
    if let Value::Ascii(ref v) = *value {
        if let Some(data) = v.first() {
            if data.len() >= 10 && data[4] == b':' && data[7] == b':' {
                if let Ok(year) = atou16(&data[0..4]) {
                    if let Ok(month) = atou16(&data[5..7]) {
                        if let Ok(day) = atou16(&data[8..10]) {
                            return write!(w, "{:04}-{:02}-{:02}",
                                          year, month, day)
                        }
                    }
                }
            }
        }
    }
    d_default(w, value)
}

// GPSDifferential (Exif/GPS 0x1e)
fn d_gpsdifferential(w: &mut dyn fmt::Write, value: &Value) -> fmt::Result {
    let s = match value.get_uint(0) {
        Some(0) => "no differential correction",
        Some(1) => "differential correction applied",
        _ => return d_unknown(w, value, "unknown GPS differential correction "),
    };
    w.write_str(s)
}

fn d_ascii_in_undef(w: &mut dyn fmt::Write, value: &Value) -> fmt::Result {
    match *value {
        Value::Undefined(ref v, _) => d_sub_ascii(w, v),
        _ => d_default(w, value),
    }
}

fn d_decimal(w: &mut dyn fmt::Write, value: &Value) -> fmt::Result {
    match *value {
        Value::Rational(ref v) => d_sub_comma_f64(w, v),
        Value::SRational(ref v) => d_sub_comma_f64(w, v),
        _ => d_default(w, value),
    }
}

#[inline(never)]
fn d_unknown(w: &mut dyn fmt::Write, value: &Value, prefix: &str)
             -> fmt::Result {
    w.write_str(prefix)?;
    d_default(w, value)
}

fn d_default(w: &mut dyn fmt::Write, value: &Value) -> fmt::Result {
    match *value {
        Value::Byte(ref v) => d_sub_comma(w, v),
        Value::Ascii(ref v) => {
            let mut first = true;
            for x in v {
                if !first {
                    w.write_str(", ")?;
                }
                first = false;
                d_sub_ascii(w, x)?;
            }
            Ok(())
        },
        Value::Short(ref v) => d_sub_comma(w, v),
        Value::Long(ref v) => d_sub_comma(w, v),
        Value::Rational(ref v) => d_sub_comma(w, v),
        Value::SByte(ref v) => d_sub_comma(w, v),
        Value::Undefined(ref v, _) => d_sub_hex(w, v),
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

fn d_sub_comma<T>(w: &mut dyn fmt::Write, slice: &[T])
                  -> fmt::Result where T: fmt::Display {
    let mut first = true;
    for x in slice {
        match first {
            true => write!(w, "{}", x),
            false => write!(w, ", {}", x),
        }?;
        first = false;
    }
    Ok(())
}

fn d_sub_comma_f64<T>(w: &mut dyn fmt::Write, slice: &[T])
                      -> fmt::Result where T: Copy + Into<f64> {
    let mut first = true;
    for &x in slice {
        let x: f64 = x.into();
        match first {
            true => write!(w, "{}", x),
            false => write!(w, ", {}", x),
        }?;
        first = false;
    }
    Ok(())
}

fn d_sub_hex(w: &mut dyn fmt::Write, bytes: &[u8]) -> fmt::Result {
    w.write_str("0x")?;
    for x in bytes {
        write!(w, "{:02x}", x)?;
    }
    Ok(())
}

fn d_sub_ascii(w: &mut dyn fmt::Write, bytes: &[u8]) -> fmt::Result {
    w.write_char('"')?;
    for &c in bytes {
        match c {
            b'\\' | b'"' => {
                w.write_char('\\')?;
                w.write_char(c as char)?;
            },
            0x20..=0x7e => w.write_char(c as char)?,
            _ => write!(w, "\\x{:02x}", c)?,
        }
    }
    w.write_char('"')
}

#[cfg(test)]
mod tests {
    use value::Rational;
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
            Tag::DateTime => {},
            _ => panic!("failed to match Tag"),
        }
    }

    #[test]
    fn default_value() {
        assert_pat!(Tag::DateTime.default_value(), None);
        match Tag::BitsPerSample.default_value() {
            Some(Value::Short(v)) => assert_eq!(v, &[8, 8, 8]),
            _ => panic!(),
        }
        match Tag::XResolution.default_value() {
            Some(Value::Rational(v)) => {
                assert_eq!(v.len(), 1);
                assert_eq!(v[0].num, 72);
                assert_eq!(v[0].denom, 1);
            },
            _ => panic!(),
        }
        match Tag::FileSource.default_value() {
            Some(Value::Undefined(v, _)) => assert_eq!(v, &[3]),
            _ => panic!(),
        }
        match Tag::GPSAltitudeRef.default_value() {
            Some(Value::Byte(v)) => assert_eq!(v, &[0]),
            _ => panic!(),
        }
        match Tag::GPSSpeedRef.default_value() {
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

    #[test]
    fn disp_val_sub() {
        let mut buf = String::new();
        d_sub_comma(&mut buf, &[0u16, 1, 2]).unwrap();
        assert_eq!(buf, "0, 1, 2");

        let mut buf = String::new();
        d_sub_comma(&mut buf, &[Rational::from((3, 5))]).unwrap();
        assert_eq!(buf, "3/5");

        let mut buf = String::new();
        d_sub_comma_f64(&mut buf, &[Rational::from((1, 2))]).unwrap();
        assert_eq!(buf, "0.5");

        let mut buf = String::new();
        d_sub_hex(&mut buf, b"abc\x00\xff").unwrap();
        assert_eq!(buf, "0x61626300ff");

        let mut buf = String::new();
        d_sub_ascii(&mut buf, b"a \"\\b\"\n").unwrap();
        assert_eq!(buf, r#""a \"\\b\"\x0a""#);
    }
}
