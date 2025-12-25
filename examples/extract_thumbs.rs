use std::fs::File;
use std::io::{BufReader, Read, Seek, SeekFrom};
use std::path::Path;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = std::env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <image_file>", args[0]);
        std::process::exit(1);
    }

    let file_path = &args[1];
    let file = File::open(file_path)?;
    let mut reader = BufReader::new(file);

    println!("Reading image: {}", file_path);
    println!();

    // Parse Exif data (now includes MPF if feature is enabled)
    let exif = exif::Reader::new().read_from_container(&mut reader)?;

    // Get all embedded images (Thumbnail + MakerNote + MPF)
    println!("=== All Embedded Images (via thumbnails()) ===");
    let thumbnails = exif.thumbnails();
    if thumbnails.is_empty() {
        println!("No embedded images found");
    } else {
        for (i, img) in thumbnails.iter().enumerate() {
            println!(
                "Image {}: {} - {} bytes at offset {}",
                i + 1,
                img.source.name(),
                img.length,
                img.offset
            );
        }
    }
    println!();

    // Extract and save all embedded images
    if !thumbnails.is_empty() {
        println!("Extracting embedded images...");

        // Reopen file for extraction
        let file = File::open(file_path)?;
        let mut reader = BufReader::new(file);

        let base_name = Path::new(file_path)
            .file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("image");

        // Track count for each source type to avoid filename collisions
        let mut source_counts = std::collections::HashMap::new();

        for img in thumbnails.iter() {
            let source_name = img.source.name();
            // Increment count for this source type
            let count = source_counts.entry(source_name).or_insert(0);
            *count += 1;

            let output_path = if *count == 1 {
                format!("{}_{}.jpg", base_name, source_name)
            } else {
                format!("{}_{}_{}.jpg", base_name, source_name, count)
            };

            // Ignore extract for primary image
            if img.source == exif::EmbeddedSubImageSource::Primary {
                println!("  * Skip for Primary image {output_path}");
                continue;
            }

            // Extract image data
            match extract_image(&mut reader, img.offset, img.length) {
                Ok(data) => {
                    std::fs::write(&output_path, &data)?;
                    println!("  ✓ Saved {} to: {} ({} bytes)",
                        img.source.name(), output_path, data.len());
                }
                Err(e) => {
                    eprintln!("  ✗ Failed to extract {}: {}", img.source.name(), e);
                }
            }
        }
        println!();
    }

    // Summary
    println!("=== Summary ===");
    println!("Total embedded images found: {}", thumbnails.len());

    #[cfg(feature = "make_note")]
    if let Ok(vendor) = exif.maker_note_vendor() {
        println!("MakerNote vendor: {:?}", vendor);
    }

    println!();
    println!("=== Enabled Features ===");
    println!("mpf: {}", cfg!(feature = "mpf"));
    println!("make_note: {}", cfg!(feature = "make_note"));
    println!();
    println!("What each feature provides:");
    println!("- mpf=false, make_note=false: Exif IFD1 thumbnail only");
    println!("- mpf=true,  make_note=false: Exif thumbnail + MPF images (via thumbnails())");
    println!("- mpf=false, make_note=true:  Exif thumbnail + MakerNote previews (via thumbnails())");
    println!("- mpf=true,  make_note=true:  All images unified in thumbnails() API");

    Ok(())
}

/// Extract image data from a file at the given offset
fn extract_image<R: Read + Seek>(
    reader: &mut R,
    offset: u64,
    length: u32,
) -> Result<Vec<u8>, Box<dyn std::error::Error>> {
    reader.seek(SeekFrom::Start(offset))?;
    let mut buffer = vec![0u8; length as usize];
    reader.read_exact(&mut buffer)?;
    Ok(buffer)
}
