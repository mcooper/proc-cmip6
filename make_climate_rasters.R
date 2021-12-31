library(gdalUtils)
library(raster)
library(dplyr)
library(SPEI)
library(lubridate)
library(zoo)
library(foreach)
library(doParallel)

setwd('~/maldat/raw/')

ref <- raster('~/maldat/covars/10/static/elevation.tif') %>%
  crop(extent(-180, 180, -50, 50))
r <- raster('chirps/chirps-v2.0.1981.01.tif')
r[r == -9999] <- NA

r <- resample(r, ref)

rll <- rasterToPoints(r) %>% data.frame

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

cl <- makeCluster(detectCores(), outfile = '')
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

  interview <- data.frame(latitude=rll$y[n],
                          longitude=rll$x[n],
                          year=rep(1981:2018, each=12),
                          month=rep(1:12, times=38),
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
  
  interview$mean_annual_precip=mean(precip)
  interview$mean_tmx=mean(tmx)
  interview$mean_tmn=mean(tmn)
  
  sel <- interview %>% filter(year == 2018)
  cat(n, round(n/nrow(rll)*100, 4), 'percent done\n') 
  write.csv(sel, paste0('../tmp/', n), row.names=F)

  sel
}

setwd('~/maldat/tmp/')

res <- lapply(list.files(), read.csv) %>%
  bind_rows

for (i in 1:12){
  for (v in c("spei01", "spei02",
              "spei03", "spei06", "spei12", "spei24", "spei36", "tmxZ", "tmxZprev",
              "tmnZ", "tmnZprev",
              "spei01prev", "spei02prev", "spei03prev", "mean_annual_precip",
              "mean_tmn", "mean_tmx")){

    sel <- res %>% 
      filter(month == i,
             mean_annual_precip > 0)

    sel <- sel[ , c('longitude', 'latitude', v)]

    r <- resample(rasterFromXYZ(xyz=sel), ref)

    writeRaster(r, paste0('~/maldat/covars/10/2018/', month.abb[i], '/', v, '.tif'),
                format='GTiff', overwrite=T)

  }
}

system('sudo poweroff')



