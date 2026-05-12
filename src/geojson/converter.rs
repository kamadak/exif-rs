use crate::{Exif, Field, In, Tag};

#[derive(Debug)]
pub enum ConverterError {
    FailedToGetXYZ,
    MissingField,
    FailedToConvert,
}

/**
 * Retrieve coordinates from exif
 */
pub fn parse_coords_from_exif_fields(
    exif: &Exif,
    get_latitude: bool,
) -> Result<f64, ConverterError> {
    let gps_ref = exif
        .get_field(
            if get_latitude {
                Tag::GPSLatitudeRef
            } else {
                Tag::GPSLongitudeRef
            },
            In::PRIMARY,
        )
        .ok_or(ConverterError::MissingField)?;
    let gps_field = exif
        .get_field(
            if get_latitude {
                Tag::GPSLatitude
            } else {
                Tag::GPSLongitude
            },
            In::PRIMARY,
        )
        .ok_or(ConverterError::MissingField)?;
    let dd = get_exif_xyz(gps_field)
        .and_then(|xyz| convert_exif_dms_to_dd(&xyz, &gps_ref.display_value().to_string()))
        .map_err(|_| ConverterError::FailedToConvert)?;
    Ok(dd)
}

/**
 * Retrieve X, Y and Z value from exif GPS Field
 */
pub fn get_exif_xyz(gps_field: &Field) -> Result<[f64; 3], ConverterError> {
    let xyz = gps_field
        .value
        .rational()
        .and_then(|value| value.get(..3))
        .and_then(|xyz| Some([xyz[0].to_f64(), xyz[1].to_f64(), xyz[2].to_f64()]))
        .ok_or(ConverterError::FailedToGetXYZ)?;
    Ok(xyz)
}

/**
 * Convert degrees minutes seconds to degrees decimals
 */
pub fn convert_exif_dms_to_dd(xyz: &[f64; 3], gps_ref: &str) -> Result<f64, ConverterError> {
    let [degrees, minutes, seconds] = xyz;
    let mut dd: f64 = degrees + (minutes / 60.0) + (seconds / 3600.0);
    // Update sign
    if ["S", "W"].contains(&gps_ref.to_uppercase().as_str()) {
        dd *= -1.0;
    }
    Ok(dd)
}

#[cfg(test)]
mod tests {
    use std::{fs::File, io::BufReader};

    use crate::{
        geojson::converter::{convert_exif_dms_to_dd, get_exif_xyz, parse_coords_from_exif_fields},
        In, Reader, Tag,
    };

    #[test]
    fn test_convert_dms_north() {
        let result = convert_exif_dms_to_dd(&[45., 30., 10.], "N").unwrap();
        assert_eq!(result, 45.50277777777778);
    }

    #[test]
    fn test_convert_dms_south() {
        let result = convert_exif_dms_to_dd(&[45., 30., 10.], "S").unwrap();
        assert_eq!(result, -45.50277777777778);
    }

    #[test]
    fn test_convert_dms_east() {
        let result = convert_exif_dms_to_dd(&[45., 30., 10.], "E").unwrap();
        assert_eq!(result, 45.50277777777778);
    }

    #[test]
    fn test_convert_dms_west() {
        let result = convert_exif_dms_to_dd(&[45., 30., 10.], "W").unwrap();
        assert_eq!(result, -45.50277777777778);
    }

    #[test]
    fn test_get_xyz() {
        let file = File::open("tests/yaminabe.tif").unwrap();
        let exif = Reader::new()
            .read_from_container(&mut BufReader::new(&file))
            .unwrap();
        let latitude_field = exif.get_field(Tag::GPSLatitude, In::PRIMARY).unwrap();
        let result = get_exif_xyz(latitude_field).unwrap();
        assert_eq!(result, [10., 0., 0.]);
    }

    #[test]
    fn test_parse_coords() {
        let file = File::open("tests/yaminabe.tif").unwrap();
        let exif = Reader::new()
            .read_from_container(&mut BufReader::new(&file))
            .unwrap();
        assert!(parse_coords_from_exif_fields(&exif, true).is_err())
    }
}
