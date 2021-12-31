var elev = ee.Image("CGIAR/SRTM90_V4")

// Get information about the CHIRPS projection.
var chirps = ee.Image(ee.ImageCollection("UCSB-CHG/CHIRPS/DAILY").first())
var chirpsProjection = elev.projection();
print('CHIRPS projection:', chirpsProjection);

var elev_std = elev
    .reduceResolution({
      reducer: ee.Reducer.stdDev()
    })
    // Request the data at the scale and projection of the CHIRPS image.
    .reproject({
      crs: chirpsProjection
    });

Map.addLayer(elev_std)



var elev = ee.Image("CGIAR/SRTM90_V4")

// Get information about the CHIRPS projection.
var chirps = ee.Image(ee.ImageCollection("UCSB-CHG/CHIRPS/DAILY").first())
var chirpsProjection = elev.projection();
print('CHIRPS projection:', chirpsProjection);

var elev_std = elev
    .reduceResolution({
      reducer: ee.Reducer.stdDev()
    })
    // Request the data at the scale and projection of the CHIRPS image.
    .reproject({crs: {
      "crs": "EPSG:4326",
      "transform": [
        0.05,
        0,
        -180,
        0,
        -0.05,
        60
      ]}}
    );
    
Map.addLayer(chirps)
Map.addLayer(elev_std)

Export.image.toDrive({image: elev_std,
                folder: 'elev_std',
                fileNamePrefix: 'elev_std',
                region: ee.Geometry.Polygon([[[-20, 25],[-20, -35],[55, -35],[55, 25]]]),
                fileFormat: "GEOTIFF"
})

