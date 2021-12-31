library(raster)
library(ncdf4)
library(lubridate)

setwd('~/maldat/raw/cru_ts_4/')

########################################
# Tmax
########################################
s <- stack('cru_ts4.03.1901.2018.tmx.dat.nc')

n <- names(s)[grepl(paste0(1981:2018, collapse='|'), names(s))]

for (name in n){
  print(name)
  sel <- s[[name]]
  writeRaster(sel, paste0('TMX', substr(name, 2, 8), '.tif'), format='GTiff', overwrite=T)
}

########################################
# Tmin
########################################
s <- stack('cru_ts4.03.1901.2018.tmn.dat.nc')

n <- names(s)[grepl(paste0(1981:2018, collapse='|'), names(s))]

for (name in n){
  print(name)
  sel <- s[[name]]
  writeRaster(sel, paste0('TMN', substr(name, 2, 8), '.tif'), format='GTiff', overwrite=T)
}

########################################
# Tave
########################################
s <- stack('cru_ts4.03.1901.2018.tmp.dat.nc')

n <- names(s)[grepl(paste0(1981:2018, collapse='|'), names(s))]

for (name in n){
  print(name)
  sel <- s[[name]]
  writeRaster(sel, paste0('TMP', substr(name, 2, 8), '.tif'), format='GTiff', overwrite=T)
}

