// Export NDVI for 2019

var avhrr = ee.ImageCollection('NOAA/CDR/AVHRR/NDVI/V5').filterDate('2018-01-01', '2018-12-31');

var months = ee.List.sequence(1, 12);

// Group by month, and then reduce within groups by mean();
// the result is an ImageCollection with one image for each
// month.
var byMonth = ee.ImageCollection.fromImages(
      months.map(function (m) {
        return avhrr.filter(ee.Filter.calendarRange(m, m, 'month'))
                    .select('NDVI').mean()
                    .set('month', m);
}));
print(byMonth);

var geometry2 = 
    ee.Geometry.Polygon(
        [[[-180, 50],
          [-180, -50],
          [180, -50],
          [180, 50]]], null, false);


Map.addLayer(byMonth.first());
Export.image.toDrive({image: byMonth.toBands(), 
                      description: 'NDVItask3', 
                      folder: 'NDVImonthly3',
                      fileNamePrefix: 'NDVImonthly3',
                      region: geometry2,
                      scale: 15000
})
