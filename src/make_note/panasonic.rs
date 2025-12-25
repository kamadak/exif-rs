//
// Panasonic & Leica MakerNote Tag definitions
// Based on:
// - https://exiftool.org/TagNames/Panasonic.html
// - Leica uses Panasonic-compatible tag structure (Leica5 format)
//
use crate::make_note::maker_tag::{MakerTag, MakerNoteVendor, StructuredMakerNoteData};
use crate::make_note::maker_tag::d_undef_as_string;
use strum::{Display, FromRepr};

/// Panasonic Image Stabilization (Tag 0x001a)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Display, FromRepr)]
#[repr(u16)]
pub enum PanasonicImageStabilization {
    #[strum(serialize = "On, Optical")]
    OnOptical = 2,
    #[strum(serialize = "Off")]
    Off = 3,
    #[strum(serialize = "On, Mode 2")]
    OnMode2 = 4,
    #[strum(serialize = "On, Optical Panning")]
    OnOpticalPanning = 5,
    #[strum(serialize = "On, Body-only")]
    OnBodyOnly = 6,
    #[strum(serialize = "On, Body-only Panning")]
    OnBodyOnlyPanning = 7,
    #[strum(serialize = "Dual IS")]
    DualIS = 9,
    #[strum(serialize = "Dual IS Panning")]
    DualISPanning = 10,
    #[strum(serialize = "Dual2 IS")]
    Dual2IS = 11,
    #[strum(serialize = "Dual2 IS Panning")]
    Dual2ISPanning = 12,
}

impl_simple_enum_make_note_raw_parse!(PanasonicImageStabilization, u16);

/// Panasonic Macro Mode (Tag 0x001c)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Display, FromRepr)]
#[repr(u16)]
pub enum PanasonicMacroMode {
    #[strum(serialize = "On")]
    On = 1,
    #[strum(serialize = "Off")]
    Off = 2,
    #[strum(serialize = "Tele-Macro")]
    TeleMacro = 0x101,
    #[strum(serialize = "Macro Zoom")]
    MacroZoom = 0x201,
}

impl_simple_enum_make_note_raw_parse!(PanasonicMacroMode, u16);

/// Panasonic Shooting Mode (Tag 0x001f)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Display, FromRepr)]
#[repr(u16)]
pub enum PanasonicShootingMode {
    #[strum(serialize = "Normal")]
    Normal = 1,
    #[strum(serialize = "Portrait")]
    Portrait = 2,
    #[strum(serialize = "Scenery")]
    Scenery = 3,
    #[strum(serialize = "Sports")]
    Sports = 4,
    #[strum(serialize = "Night Portrait")]
    NightPortrait = 5,
    #[strum(serialize = "Program")]
    Program = 6,
    #[strum(serialize = "Aperture Priority")]
    AperturePriority = 7,
    #[strum(serialize = "Shutter Priority")]
    ShutterPriority = 8,
    #[strum(serialize = "Macro")]
    Macro = 9,
    #[strum(serialize = "Spot")]
    Spot = 10,
    #[strum(serialize = "Night Scenery")]
    NightScenery = 11,
    #[strum(serialize = "Food")]
    Food = 12,
    #[strum(serialize = "Baby")]
    Baby = 13,
    #[strum(serialize = "Soft Skin")]
    SoftSkin = 14,
    #[strum(serialize = "Candlelight")]
    Candlelight = 15,
    #[strum(serialize = "Starry Night")]
    StarryNight = 16,
    #[strum(serialize = "High Sensitivity")]
    HighSensitivity = 17,
    #[strum(serialize = "Panorama Assist")]
    PanoramaAssist = 18,
    #[strum(serialize = "Underwater")]
    Underwater = 19,
    #[strum(serialize = "Beach")]
    Beach = 20,
    #[strum(serialize = "Aerial Photo")]
    AerialPhoto = 21,
    #[strum(serialize = "Sunset")]
    Sunset = 22,
    #[strum(serialize = "Pet")]
    Pet = 23,
    #[strum(serialize = "Intelligent ISO")]
    IntelligentISO = 24,
    #[strum(serialize = "Clipboard")]
    Clipboard = 25,
    #[strum(serialize = "High Speed Continuous Shooting")]
    HighSpeedContinuousShooting = 26,
    #[strum(serialize = "Intelligent Auto")]
    IntelligentAuto = 27,
    #[strum(serialize = "Multi-aspect")]
    MultiAspect = 29,
    #[strum(serialize = "Transform")]
    Transform = 32,
    #[strum(serialize = "Flash Burst")]
    FlashBurst = 33,
    #[strum(serialize = "Pin Hole")]
    PinHole = 34,
    #[strum(serialize = "Film Grain")]
    FilmGrain = 35,
    #[strum(serialize = "My Color")]
    MyColor = 36,
    #[strum(serialize = "Photo Frame")]
    PhotoFrame = 37,
    #[strum(serialize = "Movie")]
    Movie = 38,
    #[strum(serialize = "Panorama")]
    Panorama = 39,
    #[strum(serialize = "Glass Through")]
    GlassThrough = 40,
    #[strum(serialize = "HDR")]
    HDR = 41,
    #[strum(serialize = "Intelligent Auto Plus")]
    IntelligentAutoPlus = 42,
    #[strum(serialize = "Intelligent Auto Premium")]
    IntelligentAutoPremium = 43,
    #[strum(serialize = "iHandheld Night Shot")]
    IHandheldNightShot = 45,
    #[strum(serialize = "Handheld Night Shot")]
    HandheldNightShot = 46,
    #[strum(serialize = "Sweep Panorama")]
    SweepPanorama = 51,
    #[strum(serialize = "3D Photo")]
    Photo3D = 55,
    #[strum(serialize = "Creative Control")]
    CreativeControl = 59,
    #[strum(serialize = "Expressive")]
    Expressive = 62,
    #[strum(serialize = "Retro")]
    Retro = 63,
    #[strum(serialize = "Pure")]
    Pure = 64,
    #[strum(serialize = "Elegant")]
    Elegant = 65,
    #[strum(serialize = "Silky")]
    Silky = 67,
    #[strum(serialize = "Miniature Effect")]
    MiniatureEffect = 80,
    #[strum(serialize = "Dynamic Monochrome")]
    DynamicMonochrome = 81,
    #[strum(serialize = "Impressive Art")]
    ImpressiveArt = 82,
    #[strum(serialize = "HDR Art")]
    HDRArt = 83,
    #[strum(serialize = "Toy Effect")]
    ToyEffect = 84,
    #[strum(serialize = "Toy Pop")]
    ToyPop = 85,
    #[strum(serialize = "Bleach Bypass")]
    BleachBypass = 86,
    #[strum(serialize = "Monochrome")]
    Monochrome = 89,
    #[strum(serialize = "Rough Monochrome")]
    RoughMonochrome = 90,
}

impl_simple_enum_make_note_raw_parse!(PanasonicShootingMode, u16);

/// Panasonic Color Effect (Tag 0x0028)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Display, FromRepr)]
#[repr(u16)]
pub enum PanasonicColorEffect {
    #[strum(serialize = "Off")]
    Off = 1,
    #[strum(serialize = "Warm")]
    Warm = 2,
    #[strum(serialize = "Cool")]
    Cool = 3,
    #[strum(serialize = "Black & White")]
    BlackAndWhite = 4,
    #[strum(serialize = "Sepia")]
    Sepia = 5,
    #[strum(serialize = "Happy")]
    Happy = 6,
    #[strum(serialize = "Vivid")]
    Vivid = 8,
}

impl_simple_enum_make_note_raw_parse!(PanasonicColorEffect, u16);

/// Panasonic Burst Mode (Tag 0x002a)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Display, FromRepr)]
#[repr(u16)]
pub enum PanasonicBurstMode {
    #[strum(serialize = "Off")]
    Off = 0,
    #[strum(serialize = "On")]
    On = 1,
    #[strum(serialize = "Auto Exposure Bracketing (AEB)")]
    AEB = 2,
    #[strum(serialize = "Focus Bracketing")]
    FocusBracketing = 3,
    #[strum(serialize = "Unlimited")]
    Unlimited = 4,
    #[strum(serialize = "White Balance Bracketing")]
    WhiteBalanceBracketing = 8,
    #[strum(serialize = "On (with flash)")]
    OnWithFlash = 17,
    #[strum(serialize = "Aperture Bracketing")]
    ApertureBracketing = 18,
}

impl_simple_enum_make_note_raw_parse!(PanasonicBurstMode, u16);

/// Panasonic Self Timer (Tag 0x002e)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Display, FromRepr)]
#[repr(u16)]
pub enum PanasonicSelfTimer {
    #[strum(serialize = "Off")]
    Off0 = 0,
    #[strum(serialize = "Off")]
    Off1 = 1,
    #[strum(serialize = "10 s")]
    Timer10s = 2,
    #[strum(serialize = "2 s")]
    Timer2s = 3,
    #[strum(serialize = "10 s / 3 pictures")]
    Timer10s3Pics = 4,
    #[strum(serialize = "2 s after shutter pressed")]
    Timer2sAfterShutter = 258,
    #[strum(serialize = "10 s after shutter pressed")]
    Timer10sAfterShutter = 266,
    #[strum(serialize = "3 photos after 10 s")]
    Photos3After10s = 778,
}

impl_simple_enum_make_note_raw_parse!(PanasonicSelfTimer, u16);

/// Panasonic Battery Level (Tag 0x0038)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Display, FromRepr)]
#[repr(u16)]
pub enum PanasonicBatteryLevel {
    #[strum(serialize = "Full")]
    Full = 1,
    #[strum(serialize = "Medium")]
    Medium = 2,
    #[strum(serialize = "Low")]
    Low = 3,
    #[strum(serialize = "Near Empty")]
    NearEmpty = 4,
    #[strum(serialize = "Near Full")]
    NearFull = 7,
    #[strum(serialize = "Medium Low")]
    MediumLow = 8,
    #[strum(serialize = "n/a")]
    NA = 256,
}

impl_simple_enum_make_note_raw_parse!(PanasonicBatteryLevel, u16);

/// Panasonic Film Mode (Tag 0x0042)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Display, FromRepr)]
#[repr(u16)]
pub enum PanasonicFilmMode {
    #[strum(serialize = "n/a")]
    NA = 0,
    #[strum(serialize = "Standard (color)")]
    StandardColor = 1,
    #[strum(serialize = "Dynamic (color)")]
    DynamicColor = 2,
    #[strum(serialize = "Nature (color)")]
    NatureColor = 3,
    #[strum(serialize = "Smooth (color)")]
    SmoothColor = 4,
    #[strum(serialize = "Standard (B&W)")]
    StandardBW = 5,
    #[strum(serialize = "Dynamic (B&W)")]
    DynamicBW = 6,
    #[strum(serialize = "Smooth (B&W)")]
    SmoothBW = 7,
    #[strum(serialize = "Nostalgic")]
    Nostalgic = 10,
    #[strum(serialize = "Vibrant")]
    Vibrant = 11,
}

impl_simple_enum_make_note_raw_parse!(PanasonicFilmMode, u16);

/// Panasonic JPEG Quality (Tag 0x0043)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Display, FromRepr)]
#[repr(u16)]
pub enum PanasonicJPEGQuality {
    #[strum(serialize = "n/a (Movie)")]
    NA = 0,
    #[strum(serialize = "High")]
    StandardColor = 2,
    #[strum(serialize = "Standard")]
    DynamicColor = 3,
    #[strum(serialize = "Very High")]
    NatureColor = 6,
    #[strum(serialize = "n/a (RAW only)")]
    SmoothColor = 255,
}

impl_simple_enum_make_note_raw_parse!(PanasonicJPEGQuality, u16);

/// Panasonic IntelligentResolution (Tag 0x0070)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Display, FromRepr)]
#[repr(u8)]
pub enum PanasonicIntelligentResolution {
    #[strum(serialize = "Off")]
    Off = 0,
    #[strum(serialize = "Low")]
    Low = 1,
    #[strum(serialize = "Standard")]
    Standard = 2,
    #[strum(serialize = "High")]
    High = 3,
    #[strum(serialize = "Extended")]
    Extended = 4,
}

impl_simple_enum_make_note_raw_parse!(PanasonicIntelligentResolution, u8);

/// Panasonic IntelligentD-Range (Tag 0x0079)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Display, FromRepr)]
#[repr(u16)]
pub enum PanasonicIntelligentDRange {
    #[strum(serialize = "Off")]
    Off = 0,
    #[strum(serialize = "Low")]
    Low = 1,
    #[strum(serialize = "Standard")]
    Standard = 2,
    #[strum(serialize = "High")]
    High = 3,
}

impl_simple_enum_make_note_raw_parse!(PanasonicIntelligentDRange, u16);

/// Panasonic PhotoStyle (Tag 0x0089)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Display, FromRepr)]
#[repr(u16)]
pub enum PanasonicPhotoStyle {
    #[strum(serialize = "Auto")]
    Auto = 0,
    #[strum(serialize = "Standard or Custom")]
    StandardOrCustom = 1,
    #[strum(serialize = "Vivid")]
    Vivid = 2,
    #[strum(serialize = "Natural")]
    Natural = 3,
    #[strum(serialize = "Monochrome")]
    Monochrome = 4,
    #[strum(serialize = "Scenery")]
    Scenery = 5,
    #[strum(serialize = "Portrait")]
    Portrait = 6,
    #[strum(serialize = "Cinelike D")]
    CinelikeD = 8,
    #[strum(serialize = "Cinelike V")]
    CinelikeV = 9,
    #[strum(serialize = "L. Monochrome")]
    LMonochrome = 11,
    #[strum(serialize = "Like709")]
    Like709 = 12,
    #[strum(serialize = "L. Monochrome D")]
    LMonochromeD = 15,
    #[strum(serialize = "V-Log")]
    VLog = 17,
    #[strum(serialize = "Cinelike D2")]
    CinelikeD2 = 18,
}

impl_simple_enum_make_note_raw_parse!(PanasonicPhotoStyle, u16);

/// Panasonic SweepPanoramaDirection (Tag 0x0093)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Display, FromRepr)]
#[repr(u8)]
pub enum PanasonicSweepPanoramaDirection {
    #[strum(serialize = "Off")]
    Off = 0,
    #[strum(serialize = "Left to Right")]
    LeftToRight = 1,
    #[strum(serialize = "Right to Left")]
    RightToLeft = 2,
    #[strum(serialize = "Top to Bottom")]
    TopToBottom = 3,
    #[strum(serialize = "Bottom to Top")]
    BottomToTop = 4,
}

impl_simple_enum_make_note_raw_parse!(PanasonicSweepPanoramaDirection, u8);

/// Panasonic AFSubjectDetection (Tag 0x00E9)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Display, FromRepr)]
#[repr(u16)]
pub enum PanasonicAFSubjectDetection {
    #[strum(serialize = "n/a")]
    NA = 0,
    #[strum(serialize = "Human Eye/Face/Body")]
    HumanEyeFaceBody = 1,
    #[strum(serialize = "Animal")]
    Animal = 2,
    #[strum(serialize = "Human Eye/Face")]
    HumanEyeFace = 3,
    #[strum(serialize = "Animal Body")]
    AnimalBody = 4,
    #[strum(serialize = "Animal Eye/Body")]
    AnimalEyeBody = 5,
    #[strum(serialize = "Car")]
    Car = 6,
    #[strum(serialize = "Motorcycle")]
    Motorcycle = 7,
    #[strum(serialize = "Car (main part priority)")]
    CarMainPriority = 8,
    #[strum(serialize = "Motorcycle (helmet priority)")]
    MotorcycleHelmetPriority = 9,
    #[strum(serialize = "Train")]
    Train = 10,
}

impl_simple_enum_make_note_raw_parse!(PanasonicAFSubjectDetection, u16);

/// Panasonic Face Detection Information parsed structure
/// Based on https://exiftool.org/TagNames/Panasonic.html
#[derive(Debug, Clone, PartialEq)]
pub struct PanasonicFaceDetInfo {
    pub num_faces: u16,
    pub faces: Vec<FacePosition>,
}

/// Face position with center coordinates and dimensions
#[derive(Debug, Clone, PartialEq)]
pub struct FacePosition {
    pub x_center: u16,
    pub y_center: u16,
    pub width: u16,
    pub height: u16,
}

impl std::fmt::Display for PanasonicFaceDetInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} face(s)", self.num_faces)?;
        if !self.faces.is_empty() {
            write!(f, " [")?;
            for (i, face) in self.faces.iter().enumerate() {
                if i > 0 {
                    write!(f, ", ")?;
                }
                write!(f, "({},{} {}x{})", face.x_center, face.y_center, face.width, face.height)?;
            }
            write!(f, "]")?;
        }
        Ok(())
    }
}

impl StructuredMakerNoteData for PanasonicFaceDetInfo {
    /// Parse Panasonic Face Detection Info from raw bytes
    /// Based on https://exiftool.org/TagNames/Panasonic.html
    ///
    /// Panasonic uses little-endian by default
    fn raw_parse(data: &[u8], le: Option<bool>) -> Option<PanasonicFaceDetInfo> {
        let le = le.unwrap_or(true); // Default to little-endian for Panasonic
        if data.len() < 2 {
            return None;
        }

        let num_faces = if le {
            u16::from_le_bytes([data[0], data[1]])
        } else {
            u16::from_be_bytes([data[0], data[1]])
        };

        // Each face position is 8 bytes (4 x int16u)
        let expected_size = 2 + (num_faces as usize * 8);
        if data.len() < expected_size {
            return None;
        }

        let mut faces = Vec::new();
        for i in 0..num_faces as usize {
            let offset = 2 + i * 8;
            if offset + 8 > data.len() {
                break;
            }

            let x_center = if le {
                u16::from_le_bytes([data[offset], data[offset + 1]])
            } else {
                u16::from_be_bytes([data[offset], data[offset + 1]])
            };

            let y_center = if le {
                u16::from_le_bytes([data[offset + 2], data[offset + 3]])
            } else {
                u16::from_be_bytes([data[offset + 2], data[offset + 3]])
            };

            let width = if le {
                u16::from_le_bytes([data[offset + 4], data[offset + 5]])
            } else {
                u16::from_be_bytes([data[offset + 4], data[offset + 5]])
            };

            let height = if le {
                u16::from_le_bytes([data[offset + 6], data[offset + 7]])
            } else {
                u16::from_be_bytes([data[offset + 6], data[offset + 7]])
            };

            faces.push(FacePosition { x_center, y_center, width, height });
        }

        Some(PanasonicFaceDetInfo { num_faces, faces })
    }
}


generate_maker_tags! {
    vendor: Panasonic,
    tags: [
    (ImageQuality, 0x0001, "Image Quality"),
    (FirmwareVersion, 0x0002, "Firmware Version"),
    (WhiteBalance, 0x0003, "White Balance"),
    (FocusMode, 0x0007, "Focus Mode"),
    (AFAreaMode, 0x000f, "AF Area Mode"),
    (ImageStabilization, 0x001a, "Image Stabilization", PanasonicImageStabilization::from_value_to_string),
    (MacroMode, 0x001c, "Macro Mode", PanasonicMacroMode::from_value_to_string),
    (ShootingMode, 0x001f, "Shooting Mode", PanasonicShootingMode::from_value_to_string),
    (Audio, 0x0020, "Audio"),
    (DataDump, 0x0021, "Data Dump"),
    (WhiteBalanceBias, 0x0023, "White Balance Bias"),
    (FlashBias, 0x0024, "Flash Bias"),
    (InternalSerialNumber, 0x0025, "Internal Serial Number", d_undef_as_string),
    (PanasonicExifVersion, 0x0026, "Panasonic Exif Version"),
    (VideoFrameRate, 0x0027, "Video Frame Rate"),
    (ColorEffect, 0x0028, "Color Effect", PanasonicColorEffect::from_value_to_string),
    (TimeSincePowerOn, 0x0029, "Time Since Power On"),
    (BurstMode, 0x002a, "Burst Mode", PanasonicBurstMode::from_value_to_string),
    (SequenceNumber, 0x002b, "Sequence Number"),
    (ContrastMode, 0x002c, "Contrast Mode"),
    (NoiseReduction, 0x002d, "Noise Reduction"),
    (SelfTimer, 0x002e, "Self Timer", PanasonicSelfTimer::from_value_to_string),
    (Rotation, 0x0030, "Rotation"),
    (AFAssistLamp, 0x0031, "AF Assist Lamp"),
    (ColorMode, 0x0032, "Color Mode"),
    (BabyAge, 0x0033, "Baby Age"),
    (OpticalZoomMode, 0x0034, "Optical Zoom Mode"),
    (ConversionLens, 0x0035, "Conversion Lens"),
    (TravelDay, 0x0036, "Travel Day"),
    (BatteryLevel, 0x0038, "Battery Level", PanasonicBatteryLevel::from_value_to_string),
    (Contrast, 0x0039, "Contrast"),
    (WorldTimeLocation, 0x003a, "World Time Location"),
    (TextStamp, 0x003b, "Text Stamp"),
    (ProgramISO, 0x003c, "Program ISO"),
    (AdvancedSceneType, 0x003d, "Advanced Scene Type"),
    (TextStamp2, 0x003e, "Text Stamp 2"),
    (FacesDetected, 0x003f, "Faces Detected"),
    (Saturation, 0x0040, "Saturation"),
    (Sharpness, 0x0041, "Sharpness"),
    (FilmMode, 0x0042, "Film Mode", PanasonicFilmMode::from_value_to_string),
    (JPEGQuality, 0x0043, "JPEG Quality", PanasonicJPEGQuality::from_value_to_string),
    (ColorTempKelvin, 0x0044, "Color Temp Kelvin"),
    (BracketSettings, 0x0045, "Bracket Settings"),
    (WBShiftAB, 0x0046, "WB Shift AB"),
    (WBShiftGM, 0x0047, "WB Shift GM"),
    (FlashCurtain, 0x0048, "Flash Curtain"),
    (LongExposureNoiseReduction, 0x0049, "Long Exposure Noise Reduction"),
    (PanasonicImageWidth, 0x004b, "Panasonic Image Width"),
    (PanasonicImageHeight, 0x004c, "Panasonic Image Height"),
    (AFPointPosition, 0x004d, "AF Point Position"),
    (FaceDetInfo, 0x004e, "Face Det Info", PanasonicFaceDetInfo::from_value_to_string),
    (LensType, 0x0051, "Lens Type"),
    (LensSerialNumber, 0x0052, "Lens Serial Number"),
    (AccessoryType, 0x0053, "Accessory Type"),
    (AccessorySerialNumber, 0x0054, "Accessory Serial Number"),
    (Transform, 0x0059, "Transform"),
    (IntelligentExposure, 0x005d, "Intelligent Exposure"),
    (LensFirmwareVersion, 0x0060, "Lens Firmware Version"),
    (FaceRecInfo, 0x0061, "Face Rec Info"),
    (FlashWarning, 0x0062, "Flash Warning"),
    (RecognizedFaceFlags, 0x0063, "Recognized Face Flags"),
    (Title, 0x0065, "Title"),
    (BabyName, 0x0066, "Baby Name", d_undef_as_string),
    (Location, 0x0067, "Location"),
    (Country, 0x0069, "Country"),
    (State, 0x006b, "State"),
    (City, 0x006d, "City"),
    (Landmark, 0x006f, "Landmark"),
    (IntelligentResolution, 0x0070, "Intelligent Resolution", PanasonicIntelligentResolution::from_value_to_string),
    (MergedImages, 0x0076, "Merged Images"),
    (BurstSpeed, 0x0077, "Burst Speed"),
    (IntelligentDRange, 0x0079, "Intelligent D-Range", PanasonicIntelligentDRange::from_value_to_string),
    (ClearRetouch, 0x007c, "Clear Retouch"),
    (City2, 0x0080, "City 2"),
    (ManometerPressure, 0x0086, "Manometer Pressure"),
    (PhotoStyle, 0x0089, "Photo Style", PanasonicPhotoStyle::from_value_to_string),
    (ShadingCompensation, 0x008a, "Shading Compensation"),
    (WBShiftIntelligentAuto, 0x008b, "WB Shift Intelligent Auto"),
    (AccelerometerZ, 0x008c, "Accelerometer Z"),
    (AccelerometerX, 0x008d, "Accelerometer X"),
    (AccelerometerY, 0x008e, "Accelerometer Y"),
    (CameraOrientation, 0x008f, "Camera Orientation"),
    (RollAngle, 0x0090, "Roll Angle"),
    (PitchAngle, 0x0091, "Pitch Angle"),
    (WBShiftCreativeControl, 0x0092, "WB Shift Creative Control"),
    (SweepPanoramaDirection, 0x0093, "Sweep Panorama Direction", PanasonicSweepPanoramaDirection::from_value_to_string),
    (SweepPanoramaFieldOfView, 0x0094, "Sweep Panorama Field Of View"),
    (TimerRecording, 0x0096, "Timer Recording"),
    (InternalNDFilter, 0x009d, "Internal ND Filter"),
    (HDR, 0x009e, "HDR"),
    (ShutterType, 0x009f, "Shutter Type"),
    (FilterEffect, 0x00a1, "Filter Effect"),
    (ClearRetouchValue, 0x00a3, "Clear Retouch Value"),
    (OutputLUT, 0x00a7, "Output LUT"),
    (TouchAE, 0x00ab, "Touch AE"),
    (MonochromeFilterEffect, 0x00ac, "Monochrome Filter Effect"),
    (HighlightShadow, 0x00ad, "Highlight Shadow"),
    (TimeStamp, 0x00af, "Time Stamp"),
    (VideoBurstResolution, 0x00b3, "Video Burst Resolution"),
    (MultiExposure, 0x00b4, "Multi Exposure"),
    (RedEyeRemoval, 0x00b9, "Red Eye Removal"),
    (VideoBurstMode, 0x00bb, "Video Burst Mode"),
    (DiffractionCorrection, 0x00bc, "Diffraction Correction"),
    (FocusBracket, 0x00bd, "Focus Bracket"),
    (LongExposureNRUsed, 0x00be, "Long Exposure NR Used"),
    (PostFocusMerging, 0x00bf, "Post Focus Merging"),
    (VideoPreburst, 0x00c1, "Video Preburst"),
    (LensTypeMake, 0x00c4, "Lens Type Make"),
    (LensTypeModel, 0x00c5, "Lens Type Model"),
    (SensorType, 0x00ca, "Sensor Type"),
    (ISO, 0x00d1, "ISO"),
    (MonochromeGrainEffect, 0x00d2, "Monochrome Grain Effect"),
    (PhotoStyleName, 0x00d5, "Photo Style Name", d_undef_as_string),
    (NoiseReductionStrength, 0x00d6, "Noise Reduction Strength"),
    (AFAreaSize, 0x00de, "AF Area Size"),
    (LensTypeModel2, 0x00e4, "Lens Type Model 2"),
    (MinimumISO, 0x00e8, "Minimum ISO"),
    (AFSubjectDetection, 0x00e9, "AF Subject Detection", PanasonicAFSubjectDetection::from_value_to_string),
    (DynamicRangeBoost, 0x00ee, "Dynamic Range Boost"),
    (LutPrimaryFile, 0x00f1, "Primary LUT File Name", d_undef_as_string),
    (LutPrimaryGain, 0x00f3, "Primary LUT Gain"),
    (LutSecondaryFile, 0x00f4, "Secondary LUT File Name", d_undef_as_string),
    (LutSecondaryGain, 0x00f5, "Secondary LUT Gain"),
    (PrintIM, 0x0e00, "Print IM"),
    (TimeInfo, 0x2003, "Time Info"),
    (MakerNoteVersion, 0x8000, "Maker Note Version"),
    (SceneMode, 0x8001, "Scene Mode"),
    (HighlightWarning, 0x8002, "Highlight Warning"),
    (DarkFocusEnvironment, 0x8003, "Dark Focus Environment"),
    (WBRedLevel, 0x8004, "WB Red Level"),
    (WBGreenLevel, 0x8005, "WB Green Level"),
    (WBBlueLevel, 0x8006, "WB Blue Level"),
    (TextStamp3, 0x8008, "Text Stamp 3"),
    (TextStamp4, 0x8009, "Text Stamp 4"),
    (BabyAge2, 0x8010, "Baby Age 2"),
    (Transform2, 0x8012, "Transform 2"),

    // Leica-specific tags (Leica5 format) - 0x03xx and 0x04xx range
    // Found in Leica Q2, Q3, Q3 MONO models
    (Leica_0x0302, 0x0302, "Leica 0x0302"),
    (Leica_0x0304, 0x0304, "Leica 0x0304"),
    (SerialNumber, 0x0305, "Serial Number"),
    (Leica_0x0306, 0x0306, "Leica 0x0306"),
    (Leica_0x0400, 0x0400, "Leica 0x0400"),
    (Leica_0x0401, 0x0401, "Leica 0x0401"),
    (Leica_0x0402, 0x0402, "Leica 0x0402"),
    (Leica_0x0404, 0x0404, "Leica 0x0404"),
    (Leica_0x0405, 0x0405, "Leica 0x0405"),
    (Leica_0x0406, 0x0406, "Leica 0x0406"),
    (OriginalFileName, 0x0407, "Original File Name", d_undef_as_string),
    (OriginalDirectory, 0x0408, "Original Directory", d_undef_as_string),
    (Leica_0x0409, 0x0409, "Leica 0x0409"),
    (FocusInfo, 0x040a, "Focus Info"),
    (Leica_0x040b, 0x040b, "Leica 0x040b"),
    (Leica_0x040c, 0x040c, "Leica 0x040c"),
    (Leica_0x040e, 0x040e, "Leica 0x040e"),
    (ShotInfo, 0x0410, "Shot Info"),
    (Leica_0x0411, 0x0411, "Leica 0x0411"),
    (FilmMode_Leica, 0x0412, "Film Mode"),
    (WB_RGBLevels, 0x0413, "WB RGB Levels"),
    (Leica_0x0414, 0x0414, "Leica 0x0414"),
    (Leica_0x0415, 0x0415, "Leica 0x0415"),
    (Leica_0x0416, 0x0416, "Leica 0x0416"),
    (Leica_0x0417, 0x0417, "Leica 0x0417"),
    (Leica_0x0418, 0x0418, "Leica 0x0418"),
    ]
}
