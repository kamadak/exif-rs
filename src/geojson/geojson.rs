use std::{collections::HashMap, fmt, vec};

use crate::{geojson::converter::parse_coords_from_exif_fields, Exif};

#[derive(Debug, Clone)]
enum CoordinatesError {
    InvalidLongitude(f64),
    InvalidLatitude(f64),
}

#[derive(Debug, Clone)]
struct PointGeometry {
    r#type: &'static str,
    coordinates: [f64; 2],
}

impl TryFrom<[f64; 2]> for PointGeometry {
    type Error = CoordinatesError;
    fn try_from(coordinates: [f64; 2]) -> Result<Self, Self::Error> {
        let [lon, lat] = coordinates;

        if !(-180.0..=180.0).contains(&lon) {
            return Err(CoordinatesError::InvalidLongitude(lon));
        }

        if !(-90.0..=90.0).contains(&lat) {
            return Err(CoordinatesError::InvalidLatitude(lat));
        }
        Ok(Self {
            r#type: "Point",
            coordinates: [lon, lat],
        })
    }
}

impl fmt::Display for PointGeometry {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{{\"type\":\"{}\",\"coordinates\":[{},{}]}}",
            self.r#type, self.coordinates[0], self.coordinates[1]
        )
    }
}

#[derive(Debug, Clone)]
struct GeoJSON {
    r#type: &'static str,
    geometry: PointGeometry,
    properties: HashMap<String, String>,
}

impl GeoJSON {
    pub fn new(geometry: PointGeometry, properties: HashMap<String, String>) -> Self {
        Self {
            r#type: "Feature",
            geometry: geometry,
            properties: properties,
        }
    }
}

impl fmt::Display for GeoJSON {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut properties = String::from("{");
        for (index, (key, value)) in self.properties.iter().enumerate() {
            if index > 0 {
                properties.push(',');
            }
            properties.push_str(format!("\"{}\":\"{}\"", key, value).as_str())
        }
        properties.push('}');
        write!(
            f,
            "{{\"type\":\"{}\",\"geometry\":{},\"properties\":{}}}",
            self.r#type,
            self.geometry.to_string(),
            properties
        )
    }
}

#[derive(Debug, Clone)]
pub enum ExifCoordinatesError {
    FailedToCreatePoint,
    FailedToGetLatitude,
    FailedToGetLongitude,
}
/// A struct used to parse exif metadata to GeoJSONCollection.
/// # Example
/// ```
///  use exif::{In, Reader, Tag, Value, GeoJSONCollection};
///  let file = std::fs::File::open("tests/exif.tif").unwrap();
///  let exif = Reader::new().read_from_container(
///      &mut std::io::BufReader::new(&file)).unwrap();
///  let geojson = GeoJSONCollection::try_from(&exif).unwrap();
///  let geojson_stringified = geojson.to_string();
///
/// ```
#[derive(Debug, Clone)]
pub struct GeoJSONCollection {
    r#type: &'static str,
    features: Vec<GeoJSON>,
}

impl TryFrom<&Exif> for GeoJSONCollection {
    type Error = ExifCoordinatesError;

    fn try_from(value: &Exif) -> Result<Self, Self::Error> {
        let latitude = parse_coords_from_exif_fields(&value, true)
            .map_err(|_| ExifCoordinatesError::FailedToGetLatitude)?;

        let longitude = parse_coords_from_exif_fields(&value, false)
            .map_err(|_| ExifCoordinatesError::FailedToGetLongitude)?;

        let point = PointGeometry::try_from([longitude, latitude])
            .map_err(|_| ExifCoordinatesError::FailedToCreatePoint)?;
        let mut properties: HashMap<String, String> = HashMap::new();
        for f in value.fields() {
            properties.insert(f.tag.to_string(), f.display_value().to_string());
        }
        let feature = GeoJSON::new(point, properties);
        Ok(Self {
            r#type: "FeatureCollection",
            features: vec![feature],
        })
    }
}

impl fmt::Display for GeoJSONCollection {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut features = String::from("[");
        for feat in &self.features {
            features.push_str(feat.to_string().as_str());
        }
        features.push(']');
        write!(
            f,
            "{{\"type\":\"{}\",\"features\":{}}}",
            self.r#type, features,
        )
    }
}

#[cfg(test)]
mod tests {
    use std::{collections::HashMap, fs::File, io::BufReader};

    use crate::{
        geojson::geojson::{GeoJSON, GeoJSONCollection, PointGeometry},
        Reader,
    };

    #[test]
    fn point_geometry_with_good_coordinates() {
        let coordinates = [2., 79.];
        let pt = PointGeometry::try_from(coordinates);
        assert!(pt.is_ok());
    }

    #[test]
    fn point_geometry_with_wrong_coordinates() {
        let coordinates = [200., 79.];
        let pt = PointGeometry::try_from(coordinates);
        assert!(pt.is_err());
    }

    #[test]
    fn point_geometry_to_string() {
        let coordinates = [2., 79.];
        let pt = PointGeometry::try_from(coordinates).unwrap();
        assert_eq!(
            pt.to_string(),
            "{\"type\":\"Point\",\"coordinates\":[2,79]}"
        );
    }

    #[test]
    fn geojson_empty() {
        let geojson = GeoJSON::new(PointGeometry::try_from([0., 0.]).unwrap(), HashMap::new());
        assert_eq!(geojson.geometry.coordinates.len(), 2);
        assert_eq!(geojson.geometry.coordinates.get(0), Some(&0.));
        assert_eq!(geojson.properties.len(), 0);
        assert_eq!(geojson.r#type, "Feature".to_owned());
    }

    #[test]
    fn geojson() {
        let mut properties = HashMap::<String, String>::new();
        properties.insert("GPSLatitude".to_owned(), "39 deg 3 min 4.84 sec".to_owned());
        properties.insert("Acceleration".to_owned(), "100".to_owned());
        let geojson_struct = GeoJSON::new(PointGeometry::try_from([179., 0.]).unwrap(), properties);
        assert_eq!(geojson_struct.geometry.coordinates.len(), 2);
        assert_eq!(geojson_struct.geometry.coordinates.get(0), Some(&179.));
        assert_eq!(geojson_struct.properties.len(), 2);
        assert_eq!(
            geojson_struct.to_string(),
            "{\"type\":\"Feature\",\"geometry\":{\"type\":\"Point\",\"coordinates\":[179,0]},\"properties\":{\"GPSLatitude\":\"39 deg 3 min 4.84 sec\",\"Acceleration\":\"100\"}}"
        );
    }

    #[test]
    fn geojson_collection() {
        let file = File::open("tests/yaminabe.tif").unwrap();
        let exif = Reader::new()
            .read_from_container(&mut BufReader::new(&file))
            .unwrap();
        assert!(GeoJSONCollection::try_from(&exif).is_err());
    }
}
