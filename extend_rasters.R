library(raster)
library(dplyr)

setwd('~/maldat/covars/0.25dd/')

fs <- expand.grid(list(scen=c('SSP1', 'SSP2', 'SSP3', 'SSP5'),
                       var=c('pr', 'tas', 'tasmax', 'tasmin'),
                        year=2015:2100,
                        month=substr(101:112, 2, 3),
                        model=c("ACCESS-CM2", "ACCESS-ESM1-5", "BCC-CSM2-MR", "CanESM5",
                               "CMCC-ESM2", "IPSL-CM6A-LR", "MRI-ESM2-0")))

ref <- raster('elevation.tif')

for (i in 1:nrow(fs)){
  print(i/nrow(fs))
  rastpath <- paste0('~/maldat/covars/0.25dd/', fs$scen[i], '/', fs$year[i], '/', fs$month[i], 
                     '/', fs$model[i], '/', fs$var[i], '.tif')
  r <- raster(rastpath)
  r <- extend(r, ref)
  writeRaster(r, rastpath, format='GTiff', overwrite=T)
}

