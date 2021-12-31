library(raster)

ref <- raster('~/maldat/covars/0.25dd/elevation.tif')

########################
# Process CRU temp data
########################

setwd('~/maldat/raw/cru_ts_4/afr')

fs <- list.files(pattern=paste0(2010:2018, collapse='|'))

for (f in fs){
  print(f)
  r <- raster(f)
  var <- ifelse(substr(f, 1, 3) == 'TMN', 'tasmin',
                ifelse(substr(f, 1, 3) == 'TMX', 'tasmax',
                       ifelse(substr(f, 1, 3) == 'TMP', 'tas', stop("WTF"))))
  year <- substr(f, 4, 7)
  month <- substr(f, 9, 10)
  res <- resample(r, ref, filename=paste0('~/maldat/covars/0.25dd/historic/', year, '/', 
                                          month, '/', var, '.tif'),
                  format='GTiff', overwrite=T)
}

###################################
# Process CHIRPs Rainfall data
###################################

setwd('~/maldat/raw/chirps/afr')

fs <- list.files(pattern=paste0(2010:2018, collapse='|'))

for (f in fs){
  print(f)
  r <- raster(f)
  r[r==-9999] <- NA
  var <- 'pr'
  year <- substr(f, 13, 16)
  month <- substr(f, 18, 19)
  res <- resample(r, ref, filename=paste0('~/maldat/covars/0.25dd/historic/', year, '/', 
                                          month, '/', var, '.tif'),
                  format='GTiff', overwrite=T)
}
