//
// Nikon MakerNote Tag definitions
// Based on https://exiftool.org/TagNames/Nikon.html
//

use crate::make_note::maker_tag::{MakerTag, MakerNoteVendor, StructuredMakerNoteData};
use crate::make_note::maker_tag::{extract_string, extract_optional_string};
use strum::{Display, FromRepr};


/// Nikon Picture Control Data parsed structure
/// Based on https://exiftool.org/TagNames/Nikon.html#PictureControl
#[derive(Debug, Clone, PartialEq)]
pub struct NikonPictureControl {
    pub version: String,
    pub name: String,
    pub base: Option<String>,
    pub quick_adjust: i8,
    pub sharpness: i8,
    pub contrast: i8,
    pub brightness: i8,
    pub saturation: i8,
    pub hue: i8,
    pub filter_effect: u8,
    pub toning_effect: u8,
    pub toning_saturation: u8,
}

impl std::fmt::Display for NikonPictureControl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} [QuickAdjust:{:+}, Sharpness:{:+}, Contrast:{:+}, Brightness:{:+}, \
        Saturation:{:+}, Hue:{:+}, FilterEffect:{}, ToningEffect:{}, ToningSaturation:{}]",
            self.name,
            self.quick_adjust,
            self.sharpness,
            self.contrast,
            self.brightness,
            self.saturation,
            self.hue,
            self.filter_effect,
            self.toning_effect,
            self.toning_saturation,
        )?;

        Ok(())
    }
}

/// Nikon AF Area Mode
#[derive(Debug, Clone, Copy, PartialEq, Eq, Display, FromRepr)]
#[repr(u8)]
pub enum NikonAFAreaMode {
    #[strum(serialize = "Single Area")]
    SingleArea = 0,
    #[strum(serialize = "Dynamic Area")]
    DynamicArea = 1,
    #[strum(serialize = "Dynamic Area (closest)")]
    DynamicAreaClosest = 2,
    #[strum(serialize = "Group Dynamic")]
    GroupDynamic = 3,
    #[strum(serialize = "Single Area (wide)")]
    SingleAreaWide = 4,
    #[strum(serialize = "Dynamic Area (wide)")]
    DynamicAreaWide = 5,
}

impl_simple_enum_make_note_raw_parse!(NikonAFAreaMode, u8);

/// Nikon AF Point
#[derive(Debug, Clone, Copy, PartialEq, Eq, Display, FromRepr)]
#[repr(u8)]
pub enum NikonAFPoint {
    #[strum(serialize = "Center")]
    Center = 0,
    #[strum(serialize = "Top")]
    Top = 1,
    #[strum(serialize = "Bottom")]
    Bottom = 2,
    #[strum(serialize = "Mid-left")]
    MidLeft = 3,
    #[strum(serialize = "Mid-right")]
    MidRight = 4,
    #[strum(serialize = "Upper-left")]
    UpperLeft = 5,
    #[strum(serialize = "Upper-right")]
    UpperRight = 6,
    #[strum(serialize = "Lower-left")]
    LowerLeft = 7,
    #[strum(serialize = "Lower-right")]
    LowerRight = 8,
    #[strum(serialize = "Far Left")]
    FarLeft = 9,
    #[strum(serialize = "Far Right")]
    FarRight = 10,
}

impl_simple_enum_make_note_raw_parse!(NikonAFPoint, u8);

/// Nikon AF Info parsed structure
/// Based on https://exiftool.org/TagNames/Nikon.html
#[derive(Debug, Clone, PartialEq)]
pub struct NikonAFInfo {
    pub af_area_mode: Option<NikonAFAreaMode>,
    pub af_point: Option<NikonAFPoint>,
    pub af_points_in_focus: u16,
}

impl std::fmt::Display for NikonAFInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Mode:{}, Point:{}, InFocus:0x{:04X}",
            self.af_area_mode.map(|m| m.to_string()).unwrap_or_else(|| "Unknown".to_string()),
            self.af_point.map(|p| p.to_string()).unwrap_or_else(|| "Unknown".to_string()),
            self.af_points_in_focus
        )
    }
}

impl StructuredMakerNoteData for NikonAFInfo {
    /// Parse Nikon AF Info from raw bytes using zero-copy transmute
    /// Based on https://exiftool.org/TagNames/Nikon.html
    ///
    /// Nikon uses big-endian, so `le` parameter is ignored
    fn raw_parse(data: &[u8], _le: Option<bool>) -> Option<NikonAFInfo> {
        #[repr(C)]
        struct AFInfoRaw {
            af_area_mode: u8,
            af_point: u8,
            af_points_in_focus: [u8; 2],  // Big-endian int16u
        }

        if data.len() < core::mem::size_of::<AFInfoRaw>() {
            return None;
        }

        let raw: &AFInfoRaw = unsafe { core::mem::transmute(data.as_ptr()) };

        Some(NikonAFInfo {
            af_area_mode: NikonAFAreaMode::from_repr(raw.af_area_mode),
            af_point: NikonAFPoint::from_repr(raw.af_point),
            af_points_in_focus: u16::from_be_bytes(raw.af_points_in_focus),
        })
    }
}


impl StructuredMakerNoteData for NikonPictureControl {
    /// Parse Nikon Picture Control Data from raw bytes using zero-copy transmute
    /// Based on https://exiftool.org/TagNames/Nikon.html#PictureControl
    ///
    /// Nikon uses big-endian, so `le` parameter is ignored
    fn raw_parse(data: &[u8], _le: Option<bool>) -> Option<NikonPictureControl> {
        /// Raw binary layout for Picture Control Data (Version 01xx/02xx)
        /// Based on https://exiftool.org/TagNames/Nikon.html#PictureControl
        #[repr(C)]
        struct PictureControlDataV1V2 {
            version: [u8; 4],
            name: [u8; 20],
            base: [u8; 20],
            _padding0: [u8; 4],
            quick_adjust: i8,
            _padding1: u8,
            sharpness: i8,
            _padding2: u8,
            contrast: i8,
            _padding3: u8,
            brightness: i8,
            _padding4: u8,
            saturation: i8,
            _padding5: u8,
            hue: i8,
            filter_effect: u8,
            toning_effect: u8,
            toning_saturation: u8,
        }

        /// Raw binary layout for Picture Control Data (Version 03xx)
        /// Based on https://exiftool.org/TagNames/Nikon.html#PictureControl
        #[repr(C)]
        struct PictureControlDataV3 {
            version: [u8; 4],
            _padding_v3: [u8; 4],
            name: [u8; 20],
            base: [u8; 20],
            _padding0: [u8; 4],
            quick_adjust: i8,
            _padding1: u8,
            sharpness: i8,
            _padding2: u8,
            contrast: i8,
            _padding3: u8,
            brightness: i8,
            _padding4: u8,
            saturation: i8,
            _padding5: u8,
            hue: i8,
            filter_effect: u8,
            toning_effect: u8,
            toning_saturation: u8,
        }

        if data.len() < 4 {
            return None;
        }

        // Determine version and use appropriate struct layout
        let is_v3 = data.get(1).copied() == Some(b'3');

        if is_v3 {
            // Version 03xx
            if data.len() < core::mem::size_of::<PictureControlDataV3>() {
                return None;
            }
            let raw: &PictureControlDataV3 = unsafe { core::mem::transmute(data.as_ptr()) };

            let version = extract_string(&raw.version);
            let name = extract_string(&raw.name);
            let base = extract_optional_string(&raw.base);

            Some(NikonPictureControl {
                version,
                name,
                base,
                quick_adjust: raw.quick_adjust,
                sharpness: raw.sharpness,
                contrast: raw.contrast,
                brightness: raw.brightness,
                saturation: raw.saturation,
                hue: raw.hue,
                filter_effect: raw.filter_effect,
                toning_effect: raw.toning_effect,
                toning_saturation: raw.toning_saturation,
            })
        } else {
            // Version 01xx/02xx
            if data.len() < core::mem::size_of::<PictureControlDataV1V2>() {
                return None;
            }
            let raw: &PictureControlDataV1V2 = unsafe { core::mem::transmute(data.as_ptr()) };

            let version = extract_string(&raw.version);
            let name = extract_string(&raw.name);
            let base = extract_optional_string(&raw.base);

            Some(NikonPictureControl {
                version,
                name,
                base,
                quick_adjust: raw.quick_adjust,
                sharpness: raw.sharpness,
                contrast: raw.contrast,
                brightness: raw.brightness,
                saturation: raw.saturation,
                hue: raw.hue,
                filter_effect: raw.filter_effect,
                toning_effect: raw.toning_effect,
                toning_saturation: raw.toning_saturation,
            })
        }
    }

}


generate_maker_tags! {
    vendor: Nikon,
    tags: [
    (MakerNoteVersion, 0x0001, "Maker Note Version"),
    (ISO, 0x0002, "ISO"),
    (ColorMode, 0x0003, "Color Mode"),
    (Quality, 0x0004, "Quality"),
    (WhiteBalance, 0x0005, "White Balance"),
    (Sharpness, 0x0006, "Sharpness"),
    (FocusMode, 0x0007, "Focus Mode"),
    (FlashSetting, 0x0008, "Flash Setting"),
    (FlashType, 0x0009, "Flash Type"),
    (WhiteBalanceFineTune, 0x000b, "White Balance Fine Tune"),
    (WB_RBLevels, 0x000c, "WB RB Levels"),
    (ProgramShift, 0x000d, "Program Shift"),
    (ExposureDifference, 0x000e, "Exposure Difference"),
    (ISOSelection, 0x000f, "ISO Selection"),
    (DataDump, 0x0010, "Data Dump"),
    (PreviewIFD, 0x0011, "Preview IFD"),
    (FlashExposureComp, 0x0012, "Flash Exposure Comp"),
    (ISOSetting, 0x0013, "ISO Setting"),
    (ColorBalanceA, 0x0014, "Color Balance A"),
    (NRWData, 0x0015, "NRW Data"),
    (ImageBoundary, 0x0016, "Image Boundary"),
    (ExternalFlashExposureComp, 0x0017, "External Flash Exposure Comp"),
    (FlashExposureBracketValue, 0x0018, "Flash Exposure Bracket Value"),
    (ExposureBracketValue, 0x0019, "Exposure Bracket Value"),
    (ImageProcessing, 0x001a, "Image Processing"),
    (CropHiSpeed, 0x001b, "Crop Hi Speed"),
    (ExposureTuning, 0x001c, "Exposure Tuning"),
    (SerialNumber, 0x001d, "Serial Number"),
    (ColorSpace, 0x001e, "Color Space"),
    (VRInfo, 0x001f, "VR Info"),
    (ImageAuthentication, 0x0020, "Image Authentication"),
    (FaceDetect, 0x0021, "Face Detect"),
    (ActiveDLighting, 0x0022, "Active D-Lighting"),
    (PictureControlData, 0x0023, "Picture Control Data", NikonPictureControl::from_value_to_string),
    (WorldTime, 0x0024, "World Time"),
    (ISOInfo, 0x0025, "ISO Info"),
    (VignetteControl, 0x002a, "Vignette Control"),
    (DistortInfo, 0x002b, "Distort Info"),
    (UnknownInfo, 0x002c, "Unknown Info"),
    (UnknownInfo2, 0x0032, "Unknown Info 2"),
    (ShutterMode, 0x0034, "Shutter Mode"),
    (HDRInfo, 0x0035, "HDR Info"),
    (MechanicalShutterCount, 0x0037, "Mechanical Shutter Count"),
    (LocationInfo, 0x0039, "Location Info"),
    (BlackLevel, 0x003d, "Black Level"),
    (ImageSizeRAW, 0x003e, "Image Size RAW"),
    (WhiteBalanceFineTune2, 0x003f, "White Balance Fine Tune 2"),
    (JPGCompression, 0x0044, "JPG Compression"),
    (CropArea, 0x0045, "Crop Area"),
    (NikonSettings, 0x004e, "Nikon Settings"),
    (ColorTemperatureAuto, 0x004f, "Color Temperature Auto"),
    (ImageAdjustment, 0x0080, "Image Adjustment"),
    (ToneComp, 0x0081, "Tone Comp"),
    (AuxiliaryLens, 0x0082, "Auxiliary Lens"),
    (LensType, 0x0083, "Lens Type"),
    (Lens, 0x0084, "Lens"),
    (ManualFocusDistance, 0x0085, "Manual Focus Distance"),
    (DigitalZoom, 0x0086, "Digital Zoom"),
    (FlashMode, 0x0087, "Flash Mode"),
    (AFInfo, 0x0088, "AF Info", NikonAFInfo::from_value_to_string),
    (ShootingMode, 0x0089, "Shooting Mode"),
    (LensFStops, 0x008b, "Lens F Stops"),
    (ContrastCurve, 0x008c, "Contrast Curve"),
    (ColorHue, 0x008d, "Color Hue"),
    (SceneMode, 0x008f, "Scene Mode"),
    (LightSource, 0x0090, "Light Source"),
    (ShotInfo, 0x0091, "Shot Info"),
    (HueAdjustment, 0x0092, "Hue Adjustment"),
    (NEFCompression, 0x0093, "NEF Compression"),
    (SaturationAdj, 0x0094, "Saturation Adj"),
    (NoiseReduction, 0x0095, "Noise Reduction"),
    (NEFLinearizationTable, 0x0096, "NEF Linearization Table"),
    (ColorBalance, 0x0097, "Color Balance"),
    (LensData, 0x0098, "Lens Data"),
    (RawImageCenter, 0x0099, "Raw Image Center"),
    (SensorPixelSize, 0x009a, "Sensor Pixel Size"),
    (SceneAssist, 0x009c, "Scene Assist"),
    (DateStampMode, 0x009d, "Date Stamp Mode"),
    (RetouchHistory, 0x009e, "Retouch History"),
    (SerialNumber2, 0x00a0, "Serial Number 2"),
    (ImageDataSize, 0x00a2, "Image Data Size"),
    (ImageCount, 0x00a5, "Image Count"),
    (DeletedImageCount, 0x00a6, "Deleted Image Count"),
    (ShutterCount, 0x00a7, "Shutter Count"),
    (FlashInfo, 0x00a8, "Flash Info"),
    (ImageOptimization, 0x00a9, "Image Optimization"),
    (Saturation, 0x00aa, "Saturation"),
    (VariProgram, 0x00ab, "Vari Program"),
    (ImageStabilization, 0x00ac, "Image Stabilization"),
    (AFResponse, 0x00ad, "AF Response"),
    (MultiExposure, 0x00b0, "Multi Exposure"),
    (HighISONoiseReduction, 0x00b1, "High ISO Noise Reduction"),
    (ToningEffect, 0x00b3, "Toning Effect"),
    (PowerUpTime, 0x00b6, "Power Up Time"),
    (AFInfo2, 0x00b7, "AF Info 2"),
    (FileInfo, 0x00b8, "File Info"),
    (AFTune, 0x00b9, "AF Tune"),
    (RetouchInfo, 0x00bb, "Retouch Info"),
    (PictureControlData2, 0x00bd, "Picture Control Data 2", NikonPictureControl::from_value_to_string),
    (SilentPhotography, 0x00bf, "Silent Photography"),
    (BarometerInfo, 0x00c3, "Barometer Info"),
    (PrintIM, 0x0e00, "Print IM"),
    (NikonCaptureData, 0x0e01, "Nikon Capture Data"),
    (NikonCaptureVersion, 0x0e09, "Nikon Capture Version"),
    (NikonCaptureOffsets, 0x0e0e, "Nikon Capture Offsets"),
    (NikonScanIFD, 0x0e10, "Nikon Scan IFD"),
    (NikonCaptureEditVersions, 0x0e13, "Nikon Capture Edit Versions"),
    (NikonICCProfile, 0x0e1d, "Nikon ICC Profile"),
    (NikonCaptureOutput, 0x0e1e, "Nikon Capture Output"),
    (NEFBitDepth, 0x0e22, "NEF Bit Depth"),
    ]
}
