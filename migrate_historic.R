library(raster)
library(tidyverse)

#########################
# Migrate Precip Data
#########################
df <- data.frame(orig=list.files('~/maldat/raw/chirps/afr', full.names=T))

df$year <- substr(df$orig, 50, 53)
df$month <- substr(df$orig, 55, 56)

df <- df %>%
  filter(year %in% as.character(2010:2018))

for (i in 1:nrow(df)){
  print(i)
  r <- raster(df$orig[i])
  r[r == -9999] <- NA
  writeRaster(r, paste0('~/maldat/covars/0.05dd/historic/', 
                        df$year[i], '/', df$month[i], '/pr.tif'),
              overwrite=T)
}

#########################
# Migrate Temp Data
#########################
df <- data.frame(orig=list.files('~/maldat/raw/cru_ts_4/afr/', full.names=T))

df$year <- substr(df$orig, 44, 47)
df$month <- substr(df$orig, 49, 50)
df$var <- substr(df$orig, 41, 43)
df$var <- case_when(df$var == 'TMX' ~ 'tasmax',
                    df$var == 'TMN' ~ 'tasmin',
                    df$var == 'TMP' ~ 'tas')

df <- df %>%
  filter(year %in% as.character(2010:2018))

ref <- raster('~/maldat/covars/0.05dd/elevation.tif')

for (i in 1:nrow(df)){
  print(i)
  r <- raster(df$orig[i])
  res <- resample(r, ref)
  res[is.na(ref)] <- NA
  writeRaster(res, paste0('~/maldat/covars/0.05dd/historic/',
                        df$year[i], '/', df$month[i], '/', df$var[i], '.tif'),
              overwrite=T)
}
