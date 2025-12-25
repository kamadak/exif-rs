//
// Olympus MakerNote Tag definitions
// Based on https://exiftool.org/TagNames/Olympus.html
//

use crate::make_note::maker_tag::{MakerTag, MakerNoteVendor, StructuredMakerNoteData};
use crate::make_note::maker_tag::d_undef_as_string;
use strum::{Display, FromRepr};

/// Olympus Flash Mode (Tag 0x1004)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Display, FromRepr)]
#[repr(u16)]
pub enum OlympusFlashMode {
    #[strum(serialize = "Off")]
    Off = 0,
    #[strum(serialize = "On")]
    On = 1,
}

impl_simple_enum_make_note_raw_parse!(OlympusFlashMode, u16);

/// Olympus Focus Mode (Tag 0x100b)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Display, FromRepr)]
#[repr(u16)]
pub enum OlympusFocusMode {
    #[strum(serialize = "Auto")]
    Auto = 0,
    #[strum(serialize = "Manual")]
    Manual = 1,
}

impl_simple_enum_make_note_raw_parse!(OlympusFocusMode, u16);

/// Olympus Focus Range (Tag 0x100a)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Display, FromRepr)]
#[repr(u16)]
pub enum OlympusFocusRange {
    #[strum(serialize = "Normal")]
    Normal = 0,
    #[strum(serialize = "Macro")]
    Macro = 1,
}

impl_simple_enum_make_note_raw_parse!(OlympusFocusRange, u16);

/// Olympus Image Stabilization (Tag CameraSettings:0x0604)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Display, FromRepr)]
#[repr(u8)]
pub enum OlympusImageStabilization {
    #[strum(serialize = "Off")]
    Off = 0,
    #[strum(serialize = "On")]
    On = 1,
    #[strum(serialize = "On, Mode 1")]
    OnMode1 = 2,
    #[strum(serialize = "On, Mode 2")]
    OnMode2 = 3,
    #[strum(serialize = "On, Mode 3")]
    OnMode3 = 4,
}

impl_simple_enum_make_note_raw_parse!(OlympusImageStabilization, u8);

/// Olympus Picture Mode (Tag CameraSettings:0x0520)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Display, FromRepr)]
#[repr(u16)]
pub enum OlympusPictureMode {
    #[strum(serialize = "Vivid")]
    Vivid = 1,
    #[strum(serialize = "Natural")]
    Natural = 2,
    #[strum(serialize = "Muted")]
    Muted = 3,
    #[strum(serialize = "Portrait")]
    Portrait = 4,
    #[strum(serialize = "i-Enhance")]
    IEnhance = 5,
    #[strum(serialize = "e-Portrait")]
    EPortrait = 6,
    #[strum(serialize = "Color Creator")]
    ColorCreator = 7,
    #[strum(serialize = "Underwater")]
    Underwater = 8,
    #[strum(serialize = "Color Profile 1")]
    ColorProfile1 = 9,
    #[strum(serialize = "Color Profile 2")]
    ColorProfile2 = 10,
    #[strum(serialize = "Color Profile 3")]
    ColorProfile3 = 11,
    #[strum(serialize = "Monochrome Profile 1")]
    MonochromeProfile1 = 12,
    #[strum(serialize = "Monochrome Profile 2")]
    MonochromeProfile2 = 13,
    #[strum(serialize = "Art Mode")]
    ArtMode = 17,
    #[strum(serialize = "Monochrome Profile 3")]
    MonochromeProfile3 = 18,
    #[strum(serialize = "Monotone")]
    Monotone = 256,
    #[strum(serialize = "Sepia")]
    Sepia = 512,
}

impl_simple_enum_make_note_raw_parse!(OlympusPictureMode, u16);

/// Olympus Art Filter (Tag CameraSettings:0x0529)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Display, FromRepr)]
#[repr(u16)]
pub enum OlympusArtFilter {
    #[strum(serialize = "Off")]
    Off = 0,
    #[strum(serialize = "Soft Focus")]
    SoftFocus = 1,
    #[strum(serialize = "Pop Art")]
    PopArt = 2,
    #[strum(serialize = "Pale & Light Color")]
    PaleLightColor = 3,
    #[strum(serialize = "Light Tone")]
    LightTone = 4,
    #[strum(serialize = "Pin Hole")]
    PinHole = 5,
    #[strum(serialize = "Grainy Film")]
    GrainyFilm = 6,
    #[strum(serialize = "Underwater")]
    Underwater = 8,
    #[strum(serialize = "Diorama")]
    Diorama = 9,
    #[strum(serialize = "Cross Process")]
    CrossProcess = 10,
    #[strum(serialize = "Fish Eye")]
    FishEye = 12,
    #[strum(serialize = "Drawing")]
    Drawing = 13,
}

impl_simple_enum_make_note_raw_parse!(OlympusArtFilter, u16);

/// Olympus Color Space (Tag CameraSettings:0x0507)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Display, FromRepr)]
#[repr(u16)]
pub enum OlympusColorSpace {
    #[strum(serialize = "sRGB")]
    SRGB = 0,
    #[strum(serialize = "Adobe RGB")]
    AdobeRGB = 1,
    #[strum(serialize = "Pro Photo RGB")]
    ProPhotoRGB = 2,
}

impl_simple_enum_make_note_raw_parse!(OlympusColorSpace, u16);

generate_maker_tags! {
    vendor: Olympus,
    tags: [
    (MakerNoteVersion, 0x0000, "Maker Note Version"),
    (MinoltaCameraSettingsOld, 0x0001, "Minolta Camera Settings Old"),
    (MinoltaCameraSettings, 0x0003, "Minolta Camera Settings"),
    (CompressedImageSize, 0x0040, "Compressed Image Size"),
    (PreviewImageData, 0x0081, "Preview Image Data"),
    (PreviewImageStart, 0x0088, "Preview Image Start"),
    (PreviewImageLength, 0x0089, "Preview Image Length"),
    (ThumbnailImage, 0x0100, "Thumbnail Image"),
    (BodyFirmwareVersion, 0x0104, "Body Firmware Version"),
    (SpecialMode, 0x0200, "Special Mode"),
    (Quality, 0x0201, "Quality"),
    (Macro, 0x0202, "Macro"),
    (BWMode, 0x0203, "BW Mode"),
    (DigitalZoom, 0x0204, "Digital Zoom"),
    (FocalPlaneDiagonal, 0x0205, "Focal Plane Diagonal"),
    (LensDistortionParams, 0x0206, "Lens Distortion Params"),
    (CameraType, 0x0207, "Camera Type"),
    (TextInfo, 0x0208, "Text Info"),
    (CameraID, 0x0209, "Camera ID", d_undef_as_string),
    (DataDump, 0x0f00, "Data Dump"),
    (ShutterSpeedValue, 0x1000, "Shutter Speed Value"),
    (ISOValue, 0x1001, "ISO Value"),
    (ApertureValue, 0x1002, "Aperture Value"),
    (BrightnessValue, 0x1003, "Brightness Value"),
    (FlashMode, 0x1004, "Flash Mode"),
    (FlashDevice, 0x1005, "Flash Device"),
    (ExposureCompensation, 0x1006, "Exposure Compensation"),
    (SensorTemperature, 0x1007, "Sensor Temperature"),
    (LensTemperature, 0x1008, "Lens Temperature"),
    (LightCondition, 0x1009, "Light Condition"),
    (FocusRange, 0x100a, "Focus Range"),
    (FocusMode, 0x100b, "Focus Mode"),
    (ManualFocusDistance, 0x100c, "Manual Focus Distance"),
    (ZoomStepCount, 0x100d, "Zoom Step Count"),
    (FocusStepCount, 0x100e, "Focus Step Count"),
    (Sharpness, 0x100f, "Sharpness"),
    (FlashChargeLevel, 0x1010, "Flash Charge Level"),
    (ColorMatrix, 0x1011, "Color Matrix"),
    (BlackLevel, 0x1012, "Black Level"),
    (ColorTemperatureBG, 0x1013, "Color Temperature BG"),
    (ColorTemperatureRG, 0x1014, "Color Temperature RG"),
    (WBMode, 0x1015, "WB Mode"),
    (RedBalance, 0x1017, "Red Balance"),
    (BlueBalance, 0x1018, "Blue Balance"),
    (ColorMatrixNumber, 0x1019, "Color Matrix Number"),
    (SerialNumber, 0x101a, "Serial Number"),
    (ExternalFlashAE1_0, 0x101b, "External Flash AE 1.0"),
    (ExternalFlashAE2_0, 0x101c, "External Flash AE 2.0"),
    (InternalFlashAE1_0, 0x101d, "Internal Flash AE 1.0"),
    (InternalFlashAE2_0, 0x101e, "Internal Flash AE 2.0"),
    (ExternalFlashAE1, 0x101f, "External Flash AE 1"),
    (ExternalFlashAE2, 0x1020, "External Flash AE 2"),
    (InternalFlashAE1, 0x1021, "Internal Flash AE 1"),
    (InternalFlashAE2, 0x1022, "Internal Flash AE 2"),
    (FlashExposureComp, 0x1023, "Flash Exposure Comp"),
    (InternalFlashTable, 0x1024, "Internal Flash Table"),
    (ExternalFlashGValue, 0x1025, "External Flash G Value"),
    (ExternalFlashBounce, 0x1026, "External Flash Bounce"),
    (ExternalFlashZoom, 0x1027, "External Flash Zoom"),
    (ExternalFlashMode, 0x1028, "External Flash Mode"),
    (Contrast, 0x1029, "Contrast"),
    (SharpnessFactor, 0x102a, "Sharpness Factor"),
    (ColorControl, 0x102b, "Color Control"),
    (ValidBits, 0x102c, "Valid Bits"),
    (CoringFilter, 0x102d, "Coring Filter"),
    (OlympusImageWidth, 0x102e, "Olympus Image Width"),
    (OlympusImageHeight, 0x102f, "Olympus Image Height"),
    (SceneDetect, 0x1030, "Scene Detect"),
    (SceneArea, 0x1031, "Scene Area"),
    (SceneDetectData, 0x1033, "Scene Detect Data"),
    (CompressionRatio, 0x1034, "Compression Ratio"),
    (PreviewImageValid, 0x1035, "Preview Image Valid"),
    (PreviewImageStart2, 0x1036, "Preview Image Start 2"),
    (PreviewImageLength2, 0x1037, "Preview Image Length 2"),
    (AFResult, 0x1038, "AF Result"),
    (CCDScanMode, 0x1039, "CCD Scan Mode"),
    (NoiseReduction, 0x103a, "Noise Reduction"),
    (InfinityLensStep, 0x103b, "Infinity Lens Step"),
    (NearLensStep, 0x103c, "Near Lens Step"),
    (LightValueCenter, 0x103d, "Light Value Center"),
    (LightValuePeriphery, 0x103e, "Light Value Periphery"),
    (FieldCount, 0x103f, "Field Count"),
    (Equipment, 0x2010, "Equipment"),
    (CameraSettings, 0x2020, "Camera Settings"),
    (RawDevelopment, 0x2030, "Raw Development"),
    (RawDev2, 0x2031, "Raw Dev 2"),
    (ImageProcessing, 0x2040, "Image Processing"),
    (FocusInfo, 0x2050, "Focus Info"),
    (RawInfo, 0x3000, "Raw Info"),
    (MainInfo, 0x4000, "Main Info"),
    ]
}


// Equipment subdirectory (0x2010)
pub(crate) mod equipment {
    use crate::make_note::maker_tag::{MakerTag, MakerNoteVendor};
    // use crate::make_note::maker_tag::d_undef_as_string;

    generate_maker_tags! {
        vendor: OlympusEquipment,
        tags: [
        (EquipmentVersion, 0x0000, "Equipment Version"),
        (CameraType2, 0x0100, "Camera Type2"),
        (SerialNumber2, 0x0101, "Serial Number"),
        (InternalSerialNumber, 0x0102, "Internal Serial Number"),
        (FocalPlaneDiagonal2, 0x0103, "Focal Plane Diagonal"),
        (BodyFirmwareVersion2, 0x0104, "Body Firmware Version"),
        (LensType, 0x0201, "Lens Type"),
        (LensSerialNumber, 0x0202, "Lens Serial Number"),
        (LensModel, 0x0203, "Lens Model"),
        (LensFirmwareVersion, 0x0204, "Lens Firmware Version"),
        (MaxApertureAtMinFocal, 0x0205, "Max Aperture At Min Focal"),
        (MaxApertureAtMaxFocal, 0x0206, "Max Aperture At Max Focal"),
        (MinFocalLength, 0x0207, "Min Focal Length"),
        (MaxFocalLength, 0x0208, "Max Focal Length"),
        (MaxAperture2, 0x020a, "Max Aperture"),
        (LensProperties, 0x020b, "Lens Properties"),
        (Extender, 0x0301, "Extender"),
        (ExtenderSerialNumber, 0x0302, "Extender Serial Number"),
        (ExtenderModel, 0x0303, "Extender Model"),
        (ExtenderFirmwareVersion, 0x0304, "Extender Firmware Version"),
        (ConversionLens, 0x0403, "Conversion Lens"),
        (FlashType, 0x1000, "Flash Type"),
        (FlashModel2, 0x1001, "Flash Model"),
        (FlashFirmwareVersion, 0x1002, "Flash Firmware Version"),
        (FlashSerialNumber, 0x1003, "Flash Serial Number"),
        ]
    }
} // end equipment module

// CameraSettings subdirectory (0x2020)
pub(crate) mod camera_settings {
    use crate::make_note::maker_tag::{MakerTag, MakerNoteVendor};
    // use crate::make_note::maker_tag::d_undef_as_string;

    generate_maker_tags! {
        vendor: OlympusCameraSettings,
        tags: [
        (CameraSettingsVersion, 0x0000, "Camera Settings Version"),
        (PreviewImageValid2, 0x0100, "Preview Image Valid"),
        (PreviewImageStart3, 0x0101, "Preview Image Start"),
        (PreviewImageLength3, 0x0102, "Preview Image Length"),
        (ExposureMode2, 0x0200, "Exposure Mode"),
        (AELock2, 0x0201, "AE Lock"),
        (MeteringMode2, 0x0202, "Metering Mode"),
        (ExposureShift, 0x0203, "Exposure Shift"),
        (NDFilter, 0x0204, "ND Filter"),
        (MacroMode2, 0x0300, "Macro Mode"),
        (FocusMode2, 0x0301, "Focus Mode"),
        (FocusProcess2, 0x0302, "Focus Process"),
        (AFSearch, 0x0303, "AF Search"),
        (AFAreas, 0x0304, "AF Areas"),
        (AFPointSelected2, 0x0305, "AF Point Selected"),
        (AFFineTune, 0x0306, "AF Fine Tune"),
        (AFFineTuneAdj, 0x0307, "AF Fine Tune Adj"),
        (FocusBracketStepSize, 0x0308, "Focus Bracket Step Size"),
        (AISubjectTrackingMode, 0x0309, "AI Subject Tracking Mode"),
        (AFTargetInfo, 0x030a, "AF Target Info"),
        (SubjectDetectInfo, 0x030b, "Subject Detect Info"),
        (FlashMode2, 0x0400, "Flash Mode"),
        (FlashExposureComp2, 0x0401, "Flash Exposure Comp"),
        (FlashRemoteControl, 0x0403, "Flash Remote Control"),
        (FlashControlMode, 0x0404, "Flash Control Mode"),
        (FlashIntensity, 0x0405, "Flash Intensity"),
        (ManualFlashStrength, 0x0406, "Manual Flash Strength"),
        (WhiteBalance22, 0x0500, "White Balance2"),
        (WhiteBalanceTemperature2, 0x0501, "White Balance Temperature"),
        (WhiteBalanceBracket, 0x0502, "White Balance Bracket"),
        (CustomSaturation, 0x0503, "Custom Saturation"),
        (ModifiedSaturation, 0x0504, "Modified Saturation"),
        (ContrastSetting2, 0x0505, "Contrast Setting"),
        (SharpnessSetting2, 0x0506, "Sharpness Setting"),
        (ColorSpace2, 0x0507, "Color Space"),
        (SceneMode2, 0x0509, "Scene Mode"),
        (NoiseReduction2, 0x050a, "Noise Reduction"),
        (DistortionCorrection2, 0x050b, "Distortion Correction"),
        (ShadingCompensation, 0x050c, "Shading Compensation"),
        (CompressionFactor2, 0x050d, "Compression Factor"),
        (Gradation, 0x050f, "Gradation"),
        (PictureMode, 0x0520, "Picture Mode"),
        (PictureModeSaturation, 0x0521, "Picture Mode Saturation"),
        (PictureModeHue, 0x0522, "Picture Mode Hue"),
        (PictureModeContrast, 0x0523, "Picture Mode Contrast"),
        (PictureModeSharpness, 0x0524, "Picture Mode Sharpness"),
        (PictureModeBWFilter, 0x0525, "Picture Mode BW Filter"),
        (PictureModeTone, 0x0526, "Picture Mode Tone"),
        (NoiseFilter, 0x0527, "Noise Filter"),
        (ArtFilter, 0x0529, "Art Filter"),
        (MagicFilter, 0x052c, "Magic Filter"),
        (PictureModeEffect, 0x052d, "Picture Mode Effect"),
        (ToneLevel, 0x052e, "Tone Level"),
        (ArtFilterEffect, 0x052f, "Art Filter Effect"),
        (ColorCreatorEffect, 0x0532, "Color Creator Effect"),
        (MonochromeProfileSettings, 0x0537, "Monochrome Profile Settings"),
        (FilmGrainEffect, 0x0538, "Film Grain Effect"),
        (ColorProfileSettings, 0x0539, "Color Profile Settings"),
        (MonochromeVignetting, 0x053a, "Monochrome Vignetting"),
        (MonochromeColor, 0x053b, "Monochrome Color"),
        (DriveMode2, 0x0600, "Drive Mode"),
        (PanoramaMode, 0x0601, "Panorama Mode"),
        (ImageQuality22, 0x0603, "Image Quality2"),
        (ImageStabilization2, 0x0604, "Image Stabilization"),
        (StackedImage, 0x0804, "Stacked Image"),
        (ISOAutoSettings, 0x0821, "ISO Auto Settings"),
        (ManometerPressure, 0x0900, "Manometer Pressure"),
        (ManometerReading, 0x0901, "Manometer Reading"),
        (ExtendedWBDetect, 0x0902, "Extended WB Detect"),
        (RollAngle, 0x0903, "Roll Angle"),
        (PitchAngle, 0x0904, "Pitch Angle"),
        (DateTimeUTC, 0x0908, "Date Time UTC"),
        ]
    }
} // end camera_settings module

// RawDevelopment subdirectory (0x2030)
pub(crate) mod raw_development {
    use crate::make_note::maker_tag::{MakerTag, MakerNoteVendor};
    // use crate::make_note::maker_tag::d_undef_as_string;

    generate_maker_tags! {
        vendor: OlympusRawDevelopment,
        tags: [
        (RawDevVersion, 0x0000, "Raw Dev Version"),
        (RawDevExposureBiasValue, 0x0100, "Raw Dev Exposure Bias Value"),
        (RawDevWhiteBalanceValue, 0x0101, "Raw Dev White Balance Value"),
        (RawDevWBFineAdjustment, 0x0102, "Raw Dev WB Fine Adjustment"),
        (RawDevGrayPoint, 0x0103, "Raw Dev Gray Point"),
        (RawDevSaturationEmphasis, 0x0104, "Raw Dev Saturation Emphasis"),
        (RawDevMemoryColorEmphasis, 0x0105, "Raw Dev Memory Color Emphasis"),
        (RawDevContrastValue, 0x0106, "Raw Dev Contrast Value"),
        (RawDevSharpnessValue, 0x0107, "Raw Dev Sharpness Value"),
        (RawDevColorSpace, 0x0108, "Raw Dev Color Space"),
        (RawDevEngine, 0x0109, "Raw Dev Engine"),
        (RawDevNoiseReduction, 0x010a, "Raw Dev Noise Reduction"),
        (RawDevEditStatus, 0x010b, "Raw Dev Edit Status"),
        (RawDevSettings, 0x010c, "Raw Dev Settings"),
        ]
    }
} // end raw_development module

// ImageProcessing subdirectory (0x2040)
pub(crate) mod image_processing {
    use crate::make_note::maker_tag::{MakerTag, MakerNoteVendor};
    // use crate::make_note::maker_tag::d_undef_as_string;

    generate_maker_tags! {
        vendor: OlympusImageProcessing,
        tags: [
        (ImageProcessingVersion, 0x0000, "Image Processing Version"),
        (WB_RBLevels, 0x0100, "WB RB Levels"),
        (WB_RBLevels3000K, 0x0102, "WB RB Levels 3000K"),
        (WB_RBLevels3300K, 0x0103, "WB RB Levels 3300K"),
        (WB_RBLevels3600K, 0x0104, "WB RB Levels 3600K"),
        (WB_RBLevels3900K, 0x0105, "WB RB Levels 3900K"),
        (WB_RBLevels4000K, 0x0106, "WB RB Levels 4000K"),
        (WB_RBLevels4300K, 0x0107, "WB RB Levels 4300K"),
        (WB_RBLevels4500K, 0x0108, "WB RB Levels 4500K"),
        (WB_RBLevels4800K, 0x0109, "WB RB Levels 4800K"),
        (WB_RBLevels5300K, 0x010a, "WB RB Levels 5300K"),
        (WB_RBLevels6000K, 0x010b, "WB RB Levels 6000K"),
        (WB_RBLevels6600K, 0x010c, "WB RB Levels 6600K"),
        (WB_RBLevels7500K, 0x010d, "WB RB Levels 7500K"),
        (WB_GLevel3000K, 0x0113, "WB G Level 3000K"),
        (WB_GLevel3300K, 0x0114, "WB G Level 3300K"),
        (WB_GLevel3600K, 0x0115, "WB G Level 3600K"),
        (WB_GLevel3900K, 0x0116, "WB G Level 3900K"),
        (WB_GLevel4000K, 0x0117, "WB G Level 4000K"),
        (WB_GLevel4300K, 0x0118, "WB G Level 4300K"),
        (WB_GLevel4500K, 0x0119, "WB G Level 4500K"),
        (WB_GLevel4800K, 0x011a, "WB G Level 4800K"),
        (WB_GLevel5300K, 0x011b, "WB G Level 5300K"),
        (WB_GLevel6000K, 0x011c, "WB G Level 6000K"),
        (WB_GLevel6600K, 0x011d, "WB G Level 6600K"),
        (WB_GLevel7500K, 0x011e, "WB G Level 7500K"),
        (WB_GLevel, 0x011f, "WB G Level"),
        (ColorMatrix3, 0x0200, "Color Matrix"),
        (Enhancer2, 0x0300, "Enhancer"),
        (EnhancerValues, 0x0301, "Enhancer Values"),
        (CoringFilter2, 0x0310, "Coring Filter"),
        (CoringValues, 0x0311, "Coring Values"),
        (BlackLevel22, 0x0600, "Black Level2"),
        (GainBase, 0x0610, "Gain Base"),
        (ValidBits2, 0x0611, "Valid Bits"),
        (CropLeft2, 0x0612, "Crop Left"),
        (CropTop2, 0x0613, "Crop Top"),
        (CropWidth, 0x0614, "Crop Width"),
        (CropHeight, 0x0615, "Crop Height"),
        (SensorCalibration, 0x0805, "Sensor Calibration"),
        (NoiseReduction22, 0x1010, "Noise Reduction2"),
        (DistortionCorrection22, 0x1011, "Distortion Correction2"),
        (ShadingCompensation2, 0x1012, "Shading Compensation2"),
        (MultipleExposureMode, 0x101c, "Multiple Exposure Mode"),
        (AspectRatio, 0x1112, "Aspect Ratio"),
        (AspectFrame, 0x1113, "Aspect Frame"),
        (FacesDetected, 0x1200, "Faces Detected"),
        (FaceDetectArea, 0x1201, "Face Detect Area"),
        (MaxFaces, 0x1202, "Max Faces"),
        (FaceDetectFrameSize, 0x1203, "Face Detect Frame Size"),
        (FaceDetectFrameCrop, 0x1207, "Face Detect Frame Crop"),
        (CameraTemperature, 0x1306, "Camera Temperature"),
        (KeystoneCompensation, 0x1900, "Keystone Compensation"),
        (KeystoneDirection, 0x1901, "Keystone Direction"),
        (KeystoneValue, 0x1906, "Keystone Value"),
        (GNDFilterType, 0x2110, "GND Filter Type"),
        ]
    }
} // end image_processing module

// FocusInfo subdirectory (0x2050)
pub(crate) mod focus_info {
    use crate::make_note::maker_tag::{MakerTag, MakerNoteVendor};
    // use crate::make_note::maker_tag::d_undef_as_string;

    generate_maker_tags! {
        vendor: OlympusFocusInfo,
        tags: [
        (FocusInfoVersion, 0x0000, "Focus Info Version"),
        (AutoFocus2, 0x0209, "Auto Focus"),
        (SceneDetect2, 0x0210, "Scene Detect"),
        (SceneArea2, 0x0211, "Scene Area"),
        (SceneDetectData2, 0x0212, "Scene Detect Data"),
        (ZoomStepCount2, 0x0300, "Zoom Step Count"),
        (FocusStepCount2, 0x0301, "Focus Step Count"),
        (FocusStepInfinity, 0x0303, "Focus Step Infinity"),
        (FocusStepNear, 0x0304, "Focus Step Near"),
        (FocusDistance2, 0x0305, "Focus Distance"),
        (AFPoint2, 0x0308, "AF Point"),
        (AFPointDetails, 0x031b, "AF Point Details"),
        (AFInfo, 0x0328, "AF Info"),
        (ExternalFlash2, 0x1201, "External Flash"),
        (ExternalFlashGuideNumber, 0x1203, "External Flash Guide Number"),
        (ExternalFlashBounce2, 0x1204, "External Flash Bounce"),
        (ExternalFlashZoom2, 0x1205, "External Flash Zoom"),
        (InternalFlash2, 0x1208, "Internal Flash"),
        (ManualFlash2, 0x1209, "Manual Flash"),
        (MacroLED, 0x120a, "Macro LED"),
        (SensorTemperature2, 0x1500, "Sensor Temperature"),
        (ImageStabilization22, 0x1600, "Image Stabilization"),
        (AntiShockWaitingTime, 0x2100, "Anti Shock Waiting Time"),
        ]
    }
} // end focus_info module

// RawInfo subdirectory (0x3000)
pub(crate) mod raw_info {
    use crate::make_note::maker_tag::{MakerTag, MakerNoteVendor};
    // use crate::make_note::maker_tag::d_undef_as_string;

    generate_maker_tags! {
        vendor: OlympusRawInfo,
        tags: [
        (RawInfoVersion, 0x0000, "Raw Info Version"),
        (WB_RBLevelsUsed, 0x0100, "WB RB Levels Used"),
        (WB_RBLevelsAuto, 0x0110, "WB RB Levels Auto"),
        (WB_RBLevelsShade, 0x0120, "WB RB Levels Shade"),
        (WB_RBLevelsCloudy, 0x0121, "WB RB Levels Cloudy"),
        (WB_RBLevelsFineWeather, 0x0122, "WB RB Levels Fine Weather"),
        (WB_RBLevelsTungsten, 0x0123, "WB RB Levels Tungsten"),
        (WB_RBLevelsEveningSunlight, 0x0124, "WB RB Levels Evening Sunlight"),
        (WB_RBLevelsDaylightFluor, 0x0130, "WB RB Levels Daylight Fluor"),
        (WB_RBLevelsDayWhiteFluor, 0x0131, "WB RB Levels Day White Fluor"),
        (WB_RBLevelsCoolWhiteFluor, 0x0132, "WB RB Levels Cool White Fluor"),
        (WB_RBLevelsWhiteFluorescent, 0x0133, "WB RB Levels White Fluorescent"),
        (ColorMatrix22, 0x0200, "Color Matrix2"),
        (CoringFilter22, 0x0310, "Coring Filter"),
        (CoringValues2, 0x0311, "Coring Values"),
        (BlackLevel222, 0x0600, "Black Level2"),
        (YCbCrCoefficients, 0x0601, "YCbCr Coefficients"),
        (ValidPixelDepth, 0x0611, "Valid Pixel Depth"),
        (CropLeft22, 0x0612, "Crop Left"),
        (CropTop22, 0x0613, "Crop Top"),
        (CropWidth2, 0x0614, "Crop Width"),
        (CropHeight2, 0x0615, "Crop Height"),
        (LightSource2, 0x1000, "Light Source"),
        (WhiteBalanceComp, 0x1001, "White Balance Comp"),
        (SaturationSetting, 0x1010, "Saturation Setting"),
        (HueSetting, 0x1011, "Hue Setting"),
        (ContrastSetting22, 0x1012, "Contrast Setting"),
        (SharpnessSetting22, 0x1013, "Sharpness Setting"),
        (CMExposureCompensation, 0x2000, "CM Exposure Compensation"),
        (CMWhiteBalance, 0x2001, "CM White Balance"),
        (CMWhiteBalanceComp, 0x2002, "CM White Balance Comp"),
        (CMWhiteBalanceGrayPoint, 0x2010, "CM White Balance Gray Point"),
        (CMSaturation, 0x2020, "CM Saturation"),
        (CMHue, 0x2021, "CM Hue"),
        (CMContrast, 0x2022, "CM Contrast"),
        (CMSharpness, 0x2023, "CM Sharpness"),
        ]
    }
} // end raw_info module

// Wrapper functions to access subdirectory tag info
pub(crate) fn olympus_equipment_tag_name(number: u16) -> Option<&'static str> {
    equipment::tag_name(number)
}

pub(crate) fn olympus_equipment_tag_description(number: u16) -> Option<&'static str> {
    equipment::tag_description(number)
}

pub(crate) fn olympus_equipment_display_value(number: u16, value: &crate::value::Value) -> Option<String> {
    equipment::display_value(number, value)
}

pub(crate) fn olympus_camera_settings_tag_name(number: u16) -> Option<&'static str> {
    camera_settings::tag_name(number)
}

pub(crate) fn olympus_camera_settings_tag_description(number: u16) -> Option<&'static str> {
    camera_settings::tag_description(number)
}

pub(crate) fn olympus_camera_settings_display_value(number: u16, value: &crate::value::Value) -> Option<String> {
    camera_settings::display_value(number, value)
}

pub(crate) fn olympus_raw_development_tag_name(number: u16) -> Option<&'static str> {
    raw_development::tag_name(number)
}

pub(crate) fn olympus_raw_development_tag_description(number: u16) -> Option<&'static str> {
    raw_development::tag_description(number)
}

pub(crate) fn olympus_raw_development_display_value(number: u16, value: &crate::value::Value) -> Option<String> {
    raw_development::display_value(number, value)
}

pub(crate) fn olympus_image_processing_tag_name(number: u16) -> Option<&'static str> {
    image_processing::tag_name(number)
}

pub(crate) fn olympus_image_processing_tag_description(number: u16) -> Option<&'static str> {
    image_processing::tag_description(number)
}

pub(crate) fn olympus_image_processing_display_value(number: u16, value: &crate::value::Value) -> Option<String> {
    image_processing::display_value(number, value)
}

pub(crate) fn olympus_focus_info_tag_name(number: u16) -> Option<&'static str> {
    focus_info::tag_name(number)
}

pub(crate) fn olympus_focus_info_tag_description(number: u16) -> Option<&'static str> {
    focus_info::tag_description(number)
}

pub(crate) fn olympus_focus_info_display_value(number: u16, value: &crate::value::Value) -> Option<String> {
    focus_info::display_value(number, value)
}

pub(crate) fn olympus_raw_info_tag_name(number: u16) -> Option<&'static str> {
    raw_info::tag_name(number)
}

pub(crate) fn olympus_raw_info_tag_description(number: u16) -> Option<&'static str> {
    raw_info::tag_description(number)
}

pub(crate) fn olympus_raw_info_display_value(number: u16, value: &crate::value::Value) -> Option<String> {
    raw_info::display_value(number, value)
}

/// Check for Olympus/OM System subdirectory tags
/// These tags contain nested IFD structures with manufacturer-specific data
/// Note: 0x0200 (SpecialMode) is NOT a subdirectory, it's a regular value
pub(crate) fn is_olympus_subdir(tag: crate::tag::Tag) -> bool {
    matches!(tag.1, 0x2010 | 0x2020 | 0x2030 | 0x2040 | 0x2050 | 0x3000 | 0x4000)
}

/// Get the appropriate MakerNoteVendor for an Olympus subdirectory tag
///
/// Maps Olympus subdirectory tag numbers to their corresponding vendor types.
/// This allows proper vendor-specific tag handling within subdirectories.
///
/// # Arguments
/// * `tag` - The tag pointing to an Olympus subdirectory
/// * `fallback` - Vendor to return if tag is not a recognized subdirectory
///
/// # Returns
/// The appropriate MakerNoteVendor for the subdirectory
pub(crate) fn get_subdir_vendor(tag: crate::tag::Tag, fallback: MakerNoteVendor) -> MakerNoteVendor {
    match tag.1 {
        0x2010 => MakerNoteVendor::OlympusEquipment,
        0x2020 => MakerNoteVendor::OlympusCameraSettings,
        0x2030 => MakerNoteVendor::OlympusRawDevelopment,
        0x2040 => MakerNoteVendor::OlympusImageProcessing,
        0x2050 => MakerNoteVendor::OlympusFocusInfo,
        0x3000 => MakerNoteVendor::OlympusRawInfo,
        _ => fallback,
    }
}

/// Parse Olympus subdirectory IFD
///
/// Olympus subdirectories use IFD type (13), which contains a LONG offset to a nested IFD.
/// The offset is relative to the MakerNote start.
///
/// # Arguments
/// * `data` - The complete parse buffer (after proprietary header removal)
/// * `val` - The Value::Unknown containing the offset position
/// * `tag` - The tag that points to this subdirectory
/// * `offset_correction` - Bytes removed from original MakerNote (for offset adjustment)
/// * `ifd_num` - Current IFD number
/// * `parse_fn` - Callback function to parse the subdirectory IFD
///
/// # Returns
/// Ok(()) if successful, Err if parsing fails
pub(crate) fn parse_olympus_subdir<E, F>(
    data: &[u8],
    val: crate::value::Value,
    _tag: crate::tag::Tag,
    offset_correction: i32,
    ifd_num: u16,
    parse_fn: F,
) -> Result<(), crate::error::Error>
where
    E: crate::endian::Endian,
    F: FnOnce(&[u8], usize, u16) -> Result<usize, crate::error::Error>,
{
    use crate::value::Value;

    // For Olympus, the value is a direct LONG offset (stored in val field)
    // Type 13 (IFD) is not recognized by get_type_info, so we extract it directly
    if let Value::Unknown(_, _, value_pos) = val {
        // value_pos is where the 4-byte offset value is stored
        // Read the raw offset value and use it directly (Olympus uses MakerNote-relative offsets)
        let raw_offset = E::loadu32(data, value_pos as usize) as usize;
        // log::info!(
        //     "Olympus subdirectory tag 0x{:04x}: value_pos={}, raw_offset={}, offset_correction={}",
        //     tag.1,
        //     value_pos,
        //     raw_offset,
        //     offset_correction
        // );

        // Skip subdirectories with offset=0 (no data)
        if raw_offset == 0 {
            // log::info!("Olympus subdirectory tag 0x{:04x} has offset=0, skipping", tag.1);
            return Ok(());
        }

        // The raw_offset from the MakerNote is already MakerNote-relative
        // We need to adjust for the proprietary header we removed
        // Original MakerNote layout: [Olympus header 12 bytes][IFD data...]
        // Our parse_data layout: [IFD data...] (header removed)
        // So offset X in original MakerNote -> (X - 12) in parse_data
        let offset = (raw_offset as i32 - offset_correction) as usize;
        // log::info!("Olympus subdirectory adjusted offset: {}", offset);

        let next_ifd = parse_fn(data, offset, ifd_num)?;
        if next_ifd != 0 {
            // log::warn!(
            //     "Olympus subdirectory tag 0x{:04x} has next IFD at offset {}, ignoring",
            //     tag.1,
            //     next_ifd
            // );
        }
    }

    Ok(())
}
