//
// Sigma MakerNote Tag definitions
// Reference: https://exiv2.org/tags-sigma.html
//

use crate::make_note::maker_tag::{MakerTag, MakerNoteVendor, StructuredMakerNoteData};
use strum::{Display, FromRepr};

/// Sigma Color Mode (Tag 0x002c)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Display, FromRepr)]
#[repr(u32)]
pub enum SigmaColorMode {
    #[strum(serialize = "n/a")]
    NA = 0,
    #[strum(serialize = "Sepia")]
    Sepia = 1,
    #[strum(serialize = "B&W")]
    BW = 2,
    #[strum(serialize = "Standard")]
    Standard = 3,
    #[strum(serialize = "Vivid")]
    Vivid = 4,
    #[strum(serialize = "Neutral")]
    Neutral = 5,
    #[strum(serialize = "Portrait")]
    Portrait = 6,
    #[strum(serialize = "Landscape")]
    Landscape = 7,
    #[strum(serialize = "FOV Classic Blue")]
    FOVClassicBlue = 8,
}

impl_simple_enum_make_note_raw_parse!(SigmaColorMode, u32);

generate_maker_tags! {
    vendor: Sigma,
    tags: [
    (SerialNumber, 0x0002, "Serial Number"),
    (DriveMode, 0x0003, "Drive Mode"),
    (ResolutionMode, 0x0004, "Resolution Mode"),
    (AFMode, 0x0005, "AF Mode"),
    (FocusSetting, 0x0006, "Focus Setting"),
    (WhiteBalance, 0x0007, "White Balance"),
    (ExposureMode, 0x0008, "Exposure Mode"),
    (MeteringMode, 0x0009, "Metering Mode"),
    (LensFocalRange, 0x000a, "Lens Focal Range"),
    (ColorSpace, 0x000b, "Color Space"),
    (Contrast, 0x000d, "Contrast"),
    (Shadow, 0x000e, "Shadow"),
    (Highlight, 0x000f, "Highlight"),
    (Saturation, 0x0010, "Saturation"),
    (Sharpness, 0x0011, "Sharpness"),
    (X3FillLight, 0x0012, "X3 Fill Light"),
    (Quality, 0x0016, "Quality"),
    (Firmware, 0x0017, "Firmware"),
    (Software, 0x0018, "Software"),
    (AutoBracket, 0x0019, "Auto Bracket"),
    (FileFormat, 0x0026, "File Format"),
    (LensType, 0x0027, "Lens Type"),
    (LensFocalRange2, 0x002a, "Lens Focal Range 2"),
    (LensMaxApertureRange, 0x002b, "Lens Max Aperture Range"),
    (PictureMode, 0x003d, "Picture Mode"),
    ]
}
