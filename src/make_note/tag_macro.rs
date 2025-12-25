//
// Common macro for generating vendor-specific MakerNote tags
//

/// Generates vendor-specific MakerNote tag constants and lookup functions.
///
/// This macro creates:
/// - A `tags` module with tag constants
/// - A `tag_name()` function for looking up tag names
/// - A `tag_description()` function for looking up tag descriptions
/// - A `display_value()` function for custom value formatting
///
/// # Example
/// ```ignore
/// use crate::make_note::maker_tag::{MakerTag, MakerNoteVendor};
///
/// generate_maker_tags! {
///     vendor: Panasonic,
///     tags: [
///         (ImageQuality, 0x0001, "Image Quality"),
///         (PhotoStyleName, 0x00d5, "Photo Style Name", d_undef_as_string),
///     ]
/// }
/// ```
#[macro_export]
macro_rules! generate_maker_tags {
    // With custom function prefix
    (
        vendor: $vendor:ident,
        prefix: $prefix:ident,
        tags: [
            $(
                ($name:ident, $num:expr, $desc:expr $(, $dispfn:path)?)
            ),+ $(,)?
        ]
    ) => {
        paste::paste! {
            pub(crate) fn [<$prefix _tag_name>](number: u16) -> Option<&'static str> {
                match number {
                    $(
                        $num => Some(stringify!($name)),
                    )+
                    _ => None,
                }
            }

            pub(crate) fn [<$prefix _tag_description>](number: u16) -> Option<&'static str> {
                match number {
                    $(
                        $num => Some($desc),
                    )+
                    _ => None,
                }
            }

            /// Display value for a tag. Returns None to use default display.
            #[allow(unused_variables)]
            pub(crate) fn [<$prefix _display_value>](number: u16, value: &crate::value::Value) -> Option<String> {
                match number {
                    $(
                        $num => {
                            $(
                                return Some($dispfn(value, None));
                            )?
                            #[allow(unreachable_code)]
                            None
                        }
                    )+
                    _ => None,
                }
            }
        }
    };

    // Without custom prefix (backward compatible)
    (
        vendor: $vendor:ident,
        tags: [
            $(
                ($name:ident, $num:expr, $desc:expr $(, $dispfn:path)?)
            ),+ $(,)?
        ]
    ) => {
        pub mod tags {
            use super::{MakerTag, MakerNoteVendor};

            $(
                #[allow(non_upper_case_globals)]
                #[allow(dead_code)]
                pub const $name: MakerTag = MakerTag::new(MakerNoteVendor::$vendor, $num);
            )+
        }

        pub(crate) fn tag_name(number: u16) -> Option<&'static str> {
            match number {
                $(
                    $num => Some(stringify!($name)),
                )+
                _ => None,
            }
        }

        pub(crate) fn tag_description(number: u16) -> Option<&'static str> {
            match number {
                $(
                    $num => Some($desc),
                )+
                _ => None,
            }
        }

        /// Display value for a tag. Returns None to use default display.
        // If warn with unsued or dead_code, some of vendor not cover all of MakerNoteField trait
        #[allow(unused_variables)]
        pub(crate) fn display_value(number: u16, value: &crate::value::Value) -> Option<String> {
            match number {
                $(
                    $num => {
                        $(
                            return Some($dispfn(value, None));
                        )?
                        #[allow(unreachable_code)]
                        None
                    }
                )+
                _ => None,
            }
        }
    }
}

/// Implements `StructuredMakerNoteData` for simple MakerNote enums.
///
/// This macro is intended **only for enum-based MakerNote tags** that:
/// - Use a primitive integer representation (`u8`, `u16`, or `u32`)
/// - Derive `FromRepr` and `Display` via `strum`
/// - Map the raw MakerNote value directly to an enum discriminant
///
/// The generated implementation:
/// - Parses raw byte data using the specified integer width
/// - Applies endianness as follows:
///   - `Some(true)` or `None` : little-endian
///   - `Some(false)` : big-endian
/// - Converts the parsed value using `FromRepr`
///
/// # Example
/// ```ignore
/// #[derive(Debug, Copy, Clone, Display, FromRepr)]
/// #[repr(u16)]
/// enum PanasonicImageStabilization {
///     OnOptical = 2,
///     Off = 1,
/// }
///
/// impl_simple_enum_make_note_raw_parse!(PanasonicImageStabilization, u16);
/// ```
macro_rules! impl_simple_enum_make_note_raw_parse {
    ($ty:ty, u8) => {
        impl StructuredMakerNoteData for $ty {
            fn raw_parse(data: &[u8], _: Option<bool>) -> Option<Self> {
                <$ty>::from_repr(*data.get(0)?)
            }
        }
    };

    ($ty:ty, u16) => {
        impl StructuredMakerNoteData for $ty {
            fn raw_parse(data: &[u8], le: Option<bool>) -> Option<Self> {
                use core::convert::TryInto;
                let bytes: [u8; 2] = data.get(0..2)?.try_into().ok()?;
                let v = match le {
                    Some(false) => u16::from_be_bytes(bytes),
                    _ => u16::from_le_bytes(bytes),
                };
                <$ty>::from_repr(v)
            }
        }
    };

    ($ty:ty, u32) => {
        impl StructuredMakerNoteData for $ty {
            fn raw_parse(data: &[u8], le: Option<bool>) -> Option<Self> {
                use core::convert::TryInto;
                let bytes: [u8; 4] = data.get(0..4)?.try_into().ok()?;
                let v = match le {
                    Some(false) => u32::from_be_bytes(bytes),
                    _ => u32::from_le_bytes(bytes),
                };
                <$ty>::from_repr(v)
            }
        }
    };
}