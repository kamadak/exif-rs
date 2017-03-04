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

//! Compatibility warning: Exif tag constants in this module will be
//! converted to associated constants of Tag when the feature is
//! stabilized.

use std::fmt;

/// A tag of a TIFF field.
///
/// Use `exif::Tag` instead of `exif::tag::Tag`.  They are the same,
/// but `exif::tag` will become private in the future versions.
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
#[derive(Debug, PartialEq, Eq)]
pub struct Tag(pub Context, pub u16);

impl Tag {
    /// Returns the context of the tag.
    #[inline]
    pub fn context(&self) -> Context {
        self.0
    }

    /// Returns the value of the tag.
    #[inline]
    pub fn value(&self) -> u16 {
        self.1
    }

    /// Returns the description of the tag.
    #[inline]
    pub fn description(&self) -> Option<&str> {
        get_tag_info(self).map(|ti| ti.desc)
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
///
/// Use `exif::Context` instead of `exif::tag::Context`.  They are the
/// same, but `exif::tag` will become private in the future versions.
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
            ($name:ident, $num:expr, $desc:expr)
        ),+, )+
    ) => (
        $($(
            $( #[$attr] )*
            #[allow(non_upper_case_globals)]
            pub const $name: Tag = Tag($ctx, $num);
        )+)+

        // Use a separate module to avoid name conflicts between
        // const Tag and static TagInfo.
        mod tag_info {
            pub struct TagInfo {
                pub name: &'static str,
                pub desc: &'static str,
            }

            $($(
                #[allow(non_upper_case_globals)]
                pub static $name: TagInfo = TagInfo {
                    name: stringify!($name), desc: $desc };
            )+)+
        }

        fn get_tag_info(tag: &Tag) -> Option<&tag_info::TagInfo> {
            match *tag {
                $($(
                    self::$name => Some(&tag_info::$name),
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
    (ExifIFDPointer, 0x8769, "Exif IFD pointer"),
    /// A pointer to the GPS IFD.  This is used for the internal structure
    /// of Exif data and will not be returned to the user.
    (GPSInfoIFDPointer, 0x8825, "GPS Info IFD pointer"),

    |Context::Exif|

    /// A pointer to the interoperability IFD.  This is used for the internal
    /// structure of Exif data and will not be returned to the user.
    (InteropIFDPointer, 0xa005, "Interoperability IFD pointer"),

    // TIFF primary and thumbnail attributes [EXIF23 4.6.4 Table 4,
    // 4.6.8 Table 17, and 4.6.8 Table 21].
    |Context::Tiff|

    (ImageWidth, 0x100, "Image width"),
    (ImageLength, 0x101, "Image height"),
    (BitsPerSample, 0x102, "Number of bits per component"),
    (Compression, 0x103, "Compression scheme"),
    (PhotometricInterpretation, 0x106, "Pixel composition"),
    (ImageDescription, 0x10e, "Image title"),
    (Make, 0x10f, "Manufacturer of image input equipment"),
    (Model, 0x110, "Model of image input equipment"),
    (StripOffsets, 0x111, "Image data location"),
    (Orientation, 0x112, "Orientation of image"),
    (SamplesPerPixel, 0x115, "Number of components"),
    (RowsPerStrip, 0x116, "Number of rows per strip"),
    (StripByteCounts, 0x117, "Bytes per compressed strip"),
    (XResolution, 0x11a, "Image resolution in width direction"),
    (YResolution, 0x11b, "Image resolution in height direction"),
    (PlanarConfiguration, 0x11c, "Image data arrangement"),
    (ResolutionUnit, 0x128, "Unit of X and Y resolution"),
    (TransferFunction, 0x12d, "Transfer function"),
    (Software, 0x131, "Software used"),
    (DateTime, 0x132, "File change date and time"),
    (Artist, 0x13b, "Person who created the image"),
    (WhitePoint, 0x13e, "White point chromaticity"),
    (PrimaryChromaticities, 0x13f, "Chromaticities of primaries"),
    (JPEGInterchangeFormat, 0x201, "Offset to JPEG SOI"),
    (JPEGInterchangeFormatLength, 0x202, "Bytes of JPEG data"),
    (YCbCrCoefficients, 0x211, "Color space transformation matrix coefficients"),
    (YCbCrSubSampling, 0x212, "Subsampling ratio of Y to C"),
    (YCbCrPositioning, 0x213, "Y and C positioning"),
    (ReferenceBlackWhite, 0x214, "Pair of black and white reference values"),
    (Copyright, 0x8298, "Copyright holder"),

    // Exif IFD attributes [EXIF23 4.6.5 Table 7 and 4.6.8 Table 18].
    |Context::Exif|

    (ExposureTime, 0x829a, "Exposure time"),
    (FNumber, 0x829d, "F number"),
    (ExposureProgram, 0x8822, "Exposure program"),
    (SpectralSensitivity, 0x8824, "Spectral sensitivity"),
    (PhotographicSensitivity, 0x8827, "Photographic sensitivity"),
    (OECF, 0x8828, "Optoelectric conversion factor"),
    (SensitivityType, 0x8830, "Sensitivity type"),
    (StandardOutputSensitivity, 0x8831, "Standard output sensitivity"),
    (RecommendedExposureIndex, 0x8832, "Recommended exposure index"),
    (ISOSpeed, 0x8833, "ISO speed"),
    (ISOSpeedLatitudeyyy, 0x8834, "ISO speed latitude yyy"),
    (ISOSpeedLatitudezzz, 0x8835, "ISO speed latitude zzz"),
    (ExifVersion, 0x9000, "Exif version"),
    (DateTimeOriginal, 0x9003, "Date and time of original data generation"),
    (DateTimeDigitized, 0x9004, "Date and time of digital data generation"),
    (OffsetTime, 0x9010, "Offset data of DateTime"),
    (OffsetTimeOriginal, 0x9011, "Offset data of DateTimeOriginal"),
    (OffsetTimeDigitized, 0x9012, "Offset data of DateTimeDigitized"),
    (ComponentsConfiguration, 0x9101, "Meaning of each component"),
    (CompressedBitsPerPixel, 0x9102, "Image compression mode"),
    (ShutterSpeedValue, 0x9201, "Shutter speed"),
    (ApertureValue, 0x9202, "Aperture"),
    (BrightnessValue, 0x9203, "Brightness"),
    (ExposureBiasValue, 0x9204, "Exposure bias"),
    (MaxApertureValue, 0x9205, "Maximum lens aperture"),
    (SubjectDistance, 0x9206, "Subject distance"),
    (MeteringMode, 0x9207, "Metering mode"),
    (LightSource, 0x9208, "Light source"),
    (Flash, 0x9209, "Flash"),
    (FocalLength, 0x920a, "Lens focal length"),
    (SubjectArea, 0x9214, "Subject area"),
    (MakerNote, 0x927c, "Manufacturer notes"),
    (UserComment, 0x9286, "User comments"),
    (SubSecTime, 0x9290, "DateTime subseconds"),
    (SubSecTimeOriginal, 0x9291, "DateTimeOriginal subseconds"),
    (SubSecTimeDigitized, 0x9292, "DateTimeDigitized subseconds"),
    (Temperature, 0x9400, "Temperature"),
    (Humidity, 0x9401, "Humidity"),
    (Pressure, 0x9402, "Pressure"),
    (WaterDepth, 0x9403, "Water depth"),
    (Acceleration, 0x9404, "Acceleration"),
    (CameraElevationAngle, 0x9405, "Camera elevation angle"),
    (FlashpixVersion, 0xa000, "Supported Flashpix version"),
    (ColorSpace, 0xa001, "Color space information"),
    (PixelXDimension, 0xa002, "Valid image width"),
    (PixelYDimension, 0xa003, "Valid image height"),
    (RelatedSoundFile, 0xa004, "Related audio file"),
    (FlashEnergy, 0xa20b, "Flash energy"),
    (SpatialFrequencyResponse, 0xa20c, "Spatial frequency response"),
    (FocalPlaneXResolution, 0xa20e, "Focal plane X resolution"),
    (FocalPlaneYResolution, 0xa20f, "Focal plane Y resolution"),
    (FocalPlaneResolutionUnit, 0xa210, "Focal plane resolution unit"),
    (SubjectLocation, 0xa214, "Subject location"),
    (ExposureIndex, 0xa215, "Exposure index"),
    (SensingMethod, 0xa217, "Sensing method"),
    (FileSource, 0xa300, "File source"),
    (SceneType, 0xa301, "Scene type"),
    (CFAPattern, 0xa302, "CFA pattern"),
    (CustomRendered, 0xa401, "Custom image processing"),
    (ExposureMode, 0xa402, "Exposure mode"),
    (WhiteBalance, 0xa403, "White balance"),
    (DigitalZoomRatio, 0xa404, "Digital zoom ratio"),
    (FocalLengthIn35mmFilm, 0xa405, "Focal length in 35 mm film"),
    (SceneCaptureType, 0xa406, "Scene capture type"),
    (GainControl, 0xa407, "Gain control"),
    (Contrast, 0xa408, "Contrast"),
    (Saturation, 0xa409, "Saturation"),
    (Sharpness, 0xa40a, "Sharpness"),
    (DeviceSettingDescription, 0xa40b, "Device settings description"),
    (SubjectDistanceRange, 0xa40c, "Subject distance range"),
    (ImageUniqueID, 0xa420, "Unique image ID"),
    (CameraOwnerName, 0xa430, "Camera owner name"),
    (BodySerialNumber, 0xa431, "Body serial number"),
    (LensSpecification, 0xa432, "Lens specification"),
    (LensMake, 0xa433, "Lens make"),
    (LensModel, 0xa434, "Lens model"),
    (LensSerialNumber, 0xa435, "Lens serial number"),
    (Gamma, 0xa500, "Gamma"),

    // GPS attributes [EXIF23 4.6.6 Table 15 and 4.6.8 Table 19].
    |Context::Gps|

    (GPSVersionID, 0x0, "GPS tag version"),
    (GPSLatitudeRef, 0x1, "North or south latitude"),
    (GPSLatitude, 0x2, "Latitude"),
    (GPSLongitudeRef, 0x3, "East or West Longitude"),
    (GPSLongitude, 0x4, "Longitude"),
    (GPSAltitudeRef, 0x5, "Altitude reference"),
    (GPSAltitude, 0x6, "Altitude"),
    (GPSTimeStamp, 0x7, "GPS time (atomic clock)"),
    (GPSSatellites, 0x8, "GPS satellites used for measurement"),
    (GPSStatus, 0x9, "GPS receiver status"),
    (GPSMeasureMode, 0xa, "GPS measurement mode"),
    (GPSDOP, 0xb, "Measurement precision"),
    (GPSSpeedRef, 0xc, "Speed unit"),
    (GPSSpeed, 0xd, "Speed of GPS receiver"),
    (GPSTrackRef, 0xe, "Reference for direction of movement"),
    (GPSTrack, 0xf, "Direction of movement"),
    (GPSImgDirectionRef, 0x10, "Reference for direction of image"),
    (GPSImgDirection, 0x11, "Direction of image"),
    (GPSMapDatum, 0x12, "Geodetic survey data used"),
    (GPSDestLatitudeRef, 0x13, "Reference for latitude of destination"),
    (GPSDestLatitude, 0x14, "Latitude of destination"),
    (GPSDestLongitudeRef, 0x15, "Reference for longitude of destination"),
    (GPSDestLongitude, 0x16, "Longitude of destination"),
    (GPSDestBearingRef, 0x17, "Reference for bearing of destination"),
    (GPSDestBearing, 0x18, "Bearing of destination"),
    (GPSDestDistanceRef, 0x19, "Reference for distance to destination"),
    (GPSDestDistance, 0x1a, "Distance to destination"),
    (GPSProcessingMethod, 0x1b, "Name of GPS processing method"),
    (GPSAreaInformation, 0x1c, "Name of GPS area"),
    (GPSDateStamp, 0x1d, "GPS date"),
    (GPSDifferential, 0x1e, "GPS differential correction"),
    (GPSHPositioningError, 0x1f, "Horizontal positioning error"),

    // Interoperability attributes [EXIF23 4.6.7 Table 16 and 4.6.8 Table 20].
    |Context::Interop|

    (InteroperabilityIndex, 0x1, "Interoperability identification"),
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
}
