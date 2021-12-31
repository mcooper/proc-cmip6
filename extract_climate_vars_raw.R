library(gdalUtils)
library(raster)
library(dplyr)
library(SPEI)
library(lubridate)
library(zoo)
library(foreach)
library(doParallel)

setwd('~/maldat/raw/')

dat <- read.csv('~/maldat/train/map_data.csv')
dat$date_cmc <- 12 * (dat$year - 1900) + dat$month

#Subset to Africa
dat <- dat %>%
  filter(longitude > -17, longitude < 54,
         latitude > -35, latitude < 25)

sp <- SpatialPointsDataFrame(coords=dat[ c('longitude', 'latitude')], data = dat)

r <- raster('chirps/chirps-v2.0.1981.01.tif')
codes <- raster(matrix(seq(1, ncell(r)), nrow=nrow(r), ncol=ncol(r)), xmx=xmax(r), xmn=xmin(r), ymx=ymax(r), ymn=ymin(r))
codes[r==-9999] <- NA

sp@data$tmpcode <- extract(codes, sp)

#Deal with points near a coast, coming up NA
spna <- sp[is.na(sp@data$tmpcode) , ]
spna$tmpcode <- NULL
badcoords <- unique(spna@coords)
tmpcode <- apply(X = badcoords, MARGIN = 1, FUN = function(xy) codes@data@values[which.min(replace(distanceFromPoints(codes, xy), is.na(codes), NA))])
badcoords <- cbind.data.frame(badcoords, tmpcode)
spna <- merge(spna@data, badcoords)
sp <- bind_rows(spna, sp@data[!is.na(sp@data$tmpcode), ])

rll <- rasterToPoints(codes) %>% data.frame
rll <- rll[rll$layer %in% sp$tmpcode, ]

#Read in precip data
precip_in_folder <- 'chirps/'
precip_vrt_file <- extension(rasterTmpFile(), 'ivrt')
precip_files <- list.files(precip_in_folder, pattern='tif$')
gdalbuildvrt(paste0(precip_in_folder, precip_files), precip_vrt_file, separate=TRUE, verbose=T, overwrite=TRUE)

#read in tmx data
tmx_in_folder <- 'cru_ts_4/'
tmx_vrt_file <- extension(rasterTmpFile(), 'ivrt')
tmx_files <- list.files(tmx_in_folder, pattern='TMX.*tif$')
gdalbuildvrt(paste0(tmx_in_folder, tmx_files), tmx_vrt_file, separate=T, verbose=T, overwrite=T)

#read in tmn data
tmn_in_folder <- 'cru_ts_4/'
tmn_vrt_file <- extension(rasterTmpFile(), 'ivrt')
tmn_files <- list.files(tmn_in_folder, pattern='TMN.*tif$')
gdalbuildvrt(paste0(tmn_in_folder, tmn_files), tmn_vrt_file, separate=T, verbose=T, overwrite=T)

#read in tmp data
tmp_in_folder <- 'cru_ts_4/'
tmp_vrt_file <- extension(rasterTmpFile(), 'ivrt')
tmp_files <- list.files(tmp_in_folder, pattern='TMP.*tif$')
gdalbuildvrt(paste0(tmp_in_folder, tmp_files), tmp_vrt_file, separate=T, verbose=T, overwrite=T)

cl <- makeCluster(detectCores() - 1, outfile = '')
registerDoParallel(cl)

df <- foreach(n=1:nrow(rll), .packages=c('raster', 'gdalUtils', 'dplyr', 'SPEI'), .combine=bind_rows) %dopar% {
  
  precip <- gdallocationinfo(precip_vrt_file, rll$x[n], rll$y[n], wgs84=TRUE, valonly=TRUE) %>%
    as.numeric %>%
    .[1:456]
  
  tmx <- gdallocationinfo(tmx_vrt_file, rll$x[n], rll$y[n], wgs84=TRUE, valonly=TRUE) %>%
    as.numeric
  
  tmn <- gdallocationinfo(tmn_vrt_file, rll$x[n], rll$y[n], wgs84=TRUE, valonly=TRUE) %>%
    as.numeric

  tmp <- gdallocationinfo(tmp_vrt_file, rll$x[n], rll$y[n], wgs84=TRUE, valonly=TRUE) %>%
    as.numeric
  
  PET <- hargreaves(tmn, tmx, lat=rll$y[n], Pre=precip) %>%
    as.vector
  
  interview <- data.frame(tmpcode=rll$layer[n],
                          date_cmc=973:1428,
                          pr=precip,
                          tasmax=tmx,
                          tasmin=tmn,
                          tas=tmp, 
                          PET=PET)
  
  ids <- sp$id[sp$tmpcode == rll$layer[n]]

  new <- bind_rows(lapply(ids, function(x){interview %>% mutate(id = x)})) %>%
    select(-tmpcode) %>%
    mutate_if(is.numeric, function(x){round(x, 1)})

  cat(n, round(n/nrow(rll)*100, 4), 'percent done\n') 
  write.csv(new, paste0('../tmp/', n), row.names=F)

  new
}

write.csv(df, '~/maldat/train/climate_raw.csv', row.names=F)

system('~/telegram.sh "Done!"')
