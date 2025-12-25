//
// Copyright (c) 2025 Jinwoo Park (pmnxis@gmail.com).
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

//
// MPF (Multi-Picture Format) support
// Based on CIPA DC-007-2009 standard
//

use std::collections::HashMap;
use crate::endian::{Endian, BigEndian, LittleEndian};
use crate::error::Error;
use crate::tiff::{TIFF_BE, TIFF_LE};

/// MPF identifier code "MPF\0" in APP2 segment
pub const MPF_ID: [u8; 4] = [0x4d, 0x50, 0x46, 0x00];

pub mod mpf_tag;

pub use mpf_tag::{MpfTag, MpfField};

/// Parse MPF data with offset conversion from APP2-relative to absolute file offsets.
///
/// This function parses the MPF TIFF structure and converts all MPEntry image offsets
/// from APP2 segment-relative to absolute file offsets.
///
/// # Arguments
/// * `mpf_data` - The raw MPF data buffer (will be modified in-place)
/// * `app2_offset` - The absolute file offset of the MPF APP2 segment
///
/// # Returns
/// A HashMap of parsed MPF fields indexed by MpfTag
///
/// # Example
/// ```ignore
/// use exif::mpf::parse_mpf;
///
/// let mut mpf_buf = vec![/* MPF data */];
/// let mpf_fields = parse_mpf(&mut mpf_buf, 12345)?;
/// ```
pub fn parse_mpf(mpf_data: &mut [u8], app2_offset: u64) -> Result<HashMap<MpfTag, MpfField>, Error> {
    // Convert MPEntry offsets from APP2-relative to absolute
    convert_mpf_offsets_to_absolute(mpf_data, app2_offset);

    // Parse MPF TIFF structure
    // TODO: Implement full MPF TIFF parsing
    // For now, return empty HashMap as placeholder
    Ok(HashMap::new())
}

/// Convert MPEntry image offsets from APP2-relative to absolute file offsets.
///
/// MPF stores image offsets relative to the APP2 segment, but we need absolute
/// file offsets for extraction. This function modifies the buffer in-place.
///
/// # Arguments
/// * `mpf_buf` - The MPF data buffer to modify
/// * `app2_offset` - The absolute file offset of the MPF APP2 segment
fn convert_mpf_offsets_to_absolute(mpf_buf: &mut [u8], app2_offset: u64) {
    if mpf_buf.len() < 8 {
        return;
    }

    // Determine endianness
    let byte_order = BigEndian::loadu16(mpf_buf, 0);
    match byte_order {
        TIFF_BE => convert_mpf_offsets_endian::<BigEndian>(mpf_buf, app2_offset),
        TIFF_LE => convert_mpf_offsets_endian::<LittleEndian>(mpf_buf, app2_offset),
        _ => {}
    }
}

/// Convert MPEntry offsets with specific endianness.
///
/// This function:
/// 1. Locates the MPEntry tag (0xb002) in the IFD
/// 2. Finds the MPEntry data (16 bytes per image)
/// 3. Converts each ImageDataOffset field from relative to absolute
///
/// # Arguments
/// * `mpf_buf` - The MPF data buffer to modify
/// * `app2_offset` - The absolute file offset of the MPF APP2 segment
fn convert_mpf_offsets_endian<E: Endian + 'static>(mpf_buf: &mut [u8], app2_offset: u64) {
    use std::any::TypeId;

    let ifd_offset = E::loadu32(mpf_buf, 4) as usize;
    if ifd_offset >= mpf_buf.len() {
        return;
    }

    let num_entries = E::loadu16(mpf_buf, ifd_offset) as usize;

    // Find MPEntry tag (0xb002)
    for i in 0..num_entries {
        let entry_offset = ifd_offset + 2 + i * 12;
        if entry_offset + 12 > mpf_buf.len() {
            break;
        }

        let tag = E::loadu16(mpf_buf, entry_offset);
        if tag == 0xb002 {
            // MPEntry tag found
            let field_type = E::loadu16(mpf_buf, entry_offset + 2);
            let count = E::loadu32(mpf_buf, entry_offset + 4) as usize;
            let value_offset = entry_offset + 8;

            if field_type != 7 {
                // Must be UNDEFINED type
                return;
            }

            let data_offset = if count > 4 {
                E::loadu32(mpf_buf, value_offset) as usize
            } else {
                value_offset
            };

            if data_offset + count > mpf_buf.len() {
                return;
            }

            // Convert each MPEntry's image offset to absolute
            // Each MPEntry is 16 bytes: [ImageAttr(4)][ImageSize(4)][ImageDataOffset(4)][DepEntry1(2)][DepEntry2(2)]
            let num_images = count / 16;

            // Check if E is LittleEndian or BigEndian by comparing type IDs
            let is_little_endian = TypeId::of::<E>() == TypeId::of::<LittleEndian>();

            for j in 0..num_images {
                let mp_entry_offset = data_offset + j * 16 + 8; // Offset to ImageDataOffset field
                if mp_entry_offset + 4 <= mpf_buf.len() {
                    let relative_offset = E::loadu32(mpf_buf, mp_entry_offset);
                    let absolute_offset = (app2_offset + relative_offset as u64) as u32;

                    // Write back absolute offset with correct endianness
                    let bytes = if is_little_endian {
                        absolute_offset.to_le_bytes()
                    } else {
                        absolute_offset.to_be_bytes()
                    };

                    mpf_buf[mp_entry_offset..mp_entry_offset + 4].copy_from_slice(&bytes);
                }
            }
            break;
        }
    }
}
