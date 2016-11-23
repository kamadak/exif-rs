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
}

/// An enum that indicates how a tag value is interpreted.
///
/// Use `exif::Context` instead of `exif::tag::Context`.  They are the
/// same, but `exif::tag` will become private in the future versions.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Context {
    /// TIFF attributes defined in the TIFF Rev. 6.0 specification.
    Tiff,	// 0th IFD
    /// Exif attributes.
    Exif,	// 0th IFD -- Exif IFD
    /// GPS attributes.
    Gps,	// 0th IFD -- GPS IFD
    /// Interoperability attributes.
    Interop,	// 0th IFD -- Exif IFD -- Interoperability IFD
    /// TIFF fields in the 1st IFD, which represents the thumbnail image.
    Thumb,	// 1st IFD
}

macro_rules! generate_well_known_tag_constants {
    (
        $(
            // Copy the doc attribute to the actual definition.
            $( #[$attr:meta] )*
            ($name:ident, $ctx:ident, $num:expr, $desc:expr)
        ),+,
    ) => (
        $(
            $( #[$attr] )*
            #[allow(non_upper_case_globals)]
            pub const $name: Tag = Tag(Context::$ctx, $num);
        )+
    )
}

// Tag constant names do not follow the Rust naming conventions but
// the Exif field names: camel cases and all-capital acronyms.
generate_well_known_tag_constants!(
    // Exif-specific IFDs [EXIF23 4.6.3].

    /// A pointer to the Exif IFD.  This is used for the internal structure
    /// of Exif data and will not be returned to the user.
    (ExifIFDPointer, Tiff, 0x8769, "Exif IFD pointer"),
    /// A pointer to the GPS IFD.  This is used for the internal structure
    /// of Exif data and will not be returned to the user.
    (GPSInfoIFDPointer, Tiff, 0x8825, "GPS Info IFD pointer"),
    /// A pointer to the interoperability IFD.  This is used for the internal
    /// structure of Exif data and will not be returned to the user.
    (InteropIFDPointer, Exif, 0xa005, "Interoperability IFD pointer"),

    // TIFF attributes [EXIF23 4.6.4].

    // Exif IFD attributes [EXIF23 4.6.5].

    // GPS attributes [EXIF23 4.6.6].

    // Interoperability attributes [EXIF23 4.6.7].
);
