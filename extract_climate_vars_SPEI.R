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


getMonthZ <- function(x){
  m <- matrix(x, ncol=12, byrow=T)
  m <- apply(m, 2, function(x){(x - mean(x))/sd(x)})
  as.vector(t(m))
}


cl <- makeCluster(4, outfile = '')
registerDoParallel(cl)

df <- foreach(n=1:nrow(rll), .packages=c('raster', 'lubridate', 'gdalUtils', 'SPEI', 'dplyr', 'zoo'), .combine=bind_rows) %dopar% {
  
  precip <- gdallocationinfo(precip_vrt_file, rll$x[n], rll$y[n], wgs84=TRUE, valonly=TRUE) %>%
    as.numeric %>%
    .[1:456]
  
  tmx <- gdallocationinfo(tmx_vrt_file, rll$x[n], rll$y[n], wgs84=TRUE, valonly=TRUE) %>%
    as.numeric
  
  tmn <- gdallocationinfo(tmn_vrt_file, rll$x[n], rll$y[n], wgs84=TRUE, valonly=TRUE) %>%
    as.numeric
  
  PET <- hargreaves(tmn, tmx, lat=rll$y[n], Pre=precip) %>%
    as.vector
  
  s <- precip - PET
  
  interview <- data.frame(tmpcode=rll$layer[n],
                          date_cmc=973:1428,
                          spei01=as.numeric(spei(s, 01, na.rm=TRUE)$fitted),
                          spei02=as.numeric(spei(s, 02, na.rm=TRUE)$fitted),
                          spei03=as.numeric(spei(s, 03, na.rm=TRUE)$fitted),
                          spei06=as.numeric(spei(s, 06, na.rm=TRUE)$fitted),
                          spei12=as.numeric(spei(s, 12, na.rm=TRUE)$fitted),
                          spei24=as.numeric(spei(s, 24, na.rm=TRUE)$fitted),
                          spei36=as.numeric(spei(s, 36, na.rm=TRUE)$fitted),
                          tmxZ=getMonthZ(tmx),
                          tmnZ=getMonthZ(tmn))

  interview=cbind(interview, data.frame(tmxZprev=c(0, interview$tmzZ[1:455]),
                                        tmnZprev=c(0, interview$tmnZ[1:455]),
                                        spei01prev=c(0, interview$spei01[1:455]),
                                        spei02prev=c(0, interview$spei02[1:455]),
                                        spei03prev=c(0, interview$spei03[1:455])))
  
  meanannual <- data.frame(tmpcode=rll$layer[n],
                           mean_annual_precip=mean(precip, na.rm=T)*12,
                           mean_tmx=mean(tmx),
                           mean_tmn=mean(tmn))
  
  sel <- sp[sp$tmpcode == rll$layer[n], ]
  sel <- Reduce(function(x, y){merge(x,y,all.x=T,all.y=F)}, list(sel, interview, meanannual)) %>%
    select(-tmpcode)
  cat(n, round(n/nrow(rll)*100, 4), 'percent done\n') 
  write.csv(sel, paste0('../tmp/', n), row.names=F)

  sel
}

setwd('~/maldat/tmp')

res <- lapply(list.files(), read.csv) %>%
  bind_rows %>%
  select(id, matches("spei|tmp|precip|temp"))

write.csv(res, '~/maldat/train/climate.csv', row.names=F)

system('~/telegram.sh "Done!"')
