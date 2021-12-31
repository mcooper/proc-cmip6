import ee
import pandas as pd
import math
from calendar import monthrange

ee.Initialize()

dat = pd.read_csv('~/maldat/train/map_data.csv')

dat = dat.sort_values(by=['year', 'month'], ascending=True)

allres = []
for y in dat['year'].unique():
    
    print(y)
    sel = dat[dat['year'] == y]
    
    for m in sel['month'].unique():
        print(m)
        sel2 = sel[sel['month'] == m]
        points = []
        
        for i,v in sel2.iterrows():
            geom = ee.Geometry.Point(v["longitude"], v["latitude"])
            feat = ee.Feature(geom, {'id': str(v['id']), 'year': str(y), 'month': str(m)})
            points.append(feat)
        
        fc = ee.FeatureCollection(points)
        dates = [str(y) + str(m + 100)[1:3] + str(100 + d)[1:3] for d in range(1, monthrange(y, m)[1] + 1)]
        
        for d in dates:
            print(d)
            
            try:
                img = ee.Image('NOAA/CDR/AVHRR/NDVI/V5/' + d)
                res = img.reduceRegions(collection = fc, reducer=ee.Reducer.first()).getInfo()
                allres.append([x['properties'] for x in res['features']])
            
            except ee.ee_exception.EEException:
                print("Missing data for " + d)

allresflat = [item for sublist in allres for item in sublist]
pd.DataFrame(allresflat).to_csv('~/maldat/tmp/ndvi_raw.csv', index=False)

