# Tested on a machine with 60GB

library(data.table)
library(raster)
library(lubridate)
library(qmap)
library(doParallel)

rasterOptions(maxmemory=4e10)

cl <- makeCluster(4, outfile = '')
registerDoParallel(cl)

################################################
# Read in sample data to get lat-long values
ref <- raster('~/maldat/covars/0.25dd/elevation.tif')
dat <- data.frame(rasterToPoints(raster('~/maldat/covars/0.25dd/elevation.tif')))
dat[ , 3] <- NULL

#######################
# Read in observations
obs_raw <- data.frame(rasterToPoints(stack(list.files('~/maldat/raw/cru_ts_4/afr/', full.names=T, pattern='TMX'))))
obs_raw <- melt(setDT(obs_raw), id.vars = c('x', 'y'), variable.name='date')
obs_raw$date <- ymd(paste0(substr(obs_raw$date, 4, 10) , '-01'))

#################################################
# Interpolate observations to match resolution
# of the model (0.25dd)
obs <- foreach(i=1:nrow(dat), .packages=c('data.table'), 
                   .combine=rbind) %dopar% {
  x <- dat$x[i]
  y <- dat$y[i]

  # Get four nearest cells from SSP data
  obs_x_mx <- min(obs_raw$x[obs_raw$x >= x])
  obs_x_mn <- max(obs_raw$x[obs_raw$x < x])
  obs_y_mx <- min(obs_raw$y[obs_raw$y >= y])
  obs_y_mn <- max(obs_raw$y[obs_raw$y < y])
  obs_sel <- obs_raw[(obs_raw$x %in% c(obs_x_mx, obs_x_mn)) & (obs_raw$y %in% c(obs_y_mx, obs_y_mn))]

  dist_df <- expand.grid(list(x=c(obs_x_mx, obs_x_mn), y=c(obs_y_mx, obs_y_mn)))

  dist_df$x2 <- (dist_df$x - obs_x_mn)/(obs_x_mx - obs_x_mn)
  dist_df$y2 <- (dist_df$y - obs_y_mn)/(obs_y_mx - obs_y_mn)

  x2 <- (x - obs_x_mn)/(obs_x_mx - obs_x_mn)
  y2 <- (y - obs_y_mn)/(obs_y_mx - obs_y_mn)

  dist_df$w <- ifelse(dist_df$x2 == 1, x2, 1 - x2)*ifelse(dist_df$y2 == 1, y2, 1 - y2)

  dist_df$x2 <- NULL
  dist_df$y2 <- NULL

  obs_sel <- merge(obs_sel, dist_df, by=c('x', 'y')) 

  # Rescale weights for instances with coasts
  obs_sel[ ,w := w/sum(w), by=date]

  obs_sel <- obs_sel[ , .(value = sum(value*w)), .(date)]

  obs_sel$x <- x
  obs_sel$y <- y

  if (i %% 5000 == 0){
    cat(i, round(i/nrow(dat)*100, 4), 'percent done\n') 
  }

  obs_sel
}

obs <- obs[!is.na(obs$value), ]
fwrite(obs, '~/maldat/tmp/obs_tasmax.csv', row.names=F)

#obs <- fread( '~/maldat/tmp/obs_tasmax.csv')

#########################
# Get list of all models
models <- list.files('~/maldat/raw/CMIP6/', pattern='historical.*tasmax.nc')
models <- substr(models, 12, nchar(models) - 10)

#Remake cluster with fewer cores for memory
for (model in models){
  print(paste0(Sys.time(), ' Starting on ', model))

  ###########################
  # Read in SSP data
  # Convert to points
  # Combine
  # Convert kg/m-2/s-1 to mm/month
  
  sspStackToPoints <- function(fs, variable='variable', value='value'){
    DT <- data.table(rasterToPoints(brick(fs, stopIfNotEqualSpaced=F)))
    DT <- melt(DT, id.vars = c('x', 'y'), variable.name=variable,
               value.name=value)
    DT$date <- ymd(paste0(substr(DT$date, 2, 8) , '-01'))
    DT
  }
  
  hst <- sspStackToPoints(paste0('~/maldat/raw/CMIP6/historical_', model, '_tasmax.nc'),
                          'date', 'hst')
  ix <- hst$date %in% obs$date
  hst <- hst[ix]

  # Skip cases where no temporal match between historical observations and simulations
  if (nrow(hst) == 0){
    next
  }
  
  ssp1 <- sspStackToPoints(paste0('~/maldat/raw/CMIP6/ssp126_', model, '_tasmax.nc'), 'date', 'ssp1')
  ssp2 <- sspStackToPoints(paste0('~/maldat/raw/CMIP6/ssp245_', model, '_tasmax.nc'), 'date', 'ssp2')
  ssp3 <- sspStackToPoints(paste0('~/maldat/raw/CMIP6/ssp370_', model, '_tasmax.nc'), 'date', 'ssp3')
  ssp5 <- sspStackToPoints(paste0('~/maldat/raw/CMIP6/ssp585_', model, '_tasmax.nc'), 'date', 'ssp5')
  
  ssp <- Reduce(merge, list(ssp1, ssp2, ssp3, ssp5))
  
  #Crop SSP array to first value outside range of OBS data
  ssp <- ssp[(ssp$x <= min(ssp$x[ssp$x > max(obs$x)])) & 
             (ssp$x >= max(ssp$x[ssp$x < min(obs$x)])) & 
             (ssp$y <= min(ssp$y[ssp$y > max(obs$y)])) & 
             (ssp$y >= max(ssp$y[ssp$y < min(obs$y)])), ]

  ix <- (hst$x < max(obs$x) + 2) & 
        (hst$x > min(obs$x) - 2) & 
        (hst$y < max(obs$y) + 2) & 
        (hst$y > min(obs$y) - 2)
  hst <- hst[ix]
  
  #Convert K to C
  ssp$ssp1 <- ssp$ssp1 - 273.15
  ssp$ssp2 <- ssp$ssp2 - 273.15
  ssp$ssp3 <- ssp$ssp3 - 273.15
  ssp$ssp5 <- ssp$ssp5 - 273.15
  hst$hst <- hst$hst - 273.15
  
  #########################################
  # Iterate over chirps points
  # Perform grid-cell-level bias correction
  
  #Get all xy values
  xy <- unique(obs[ , c('x', 'y')])
  ssp_xy <- unique(ssp[ , c('x', 'y')])
  
  ssp_res <- foreach(i=1:nrow(xy), .packages=c('lubridate', 'qmap', 'data.table'), 
                     .combine=rbind) %dopar%{
    x <- xy$x[i]
    y <- xy$y[i]
    obs_sel <- data.frame(obs)[(obs$x == x) & (obs$y == y), ]

    # Get four nearest cells from SSP data
    ssp_x_mx <- min(ssp_xy$x[ssp_xy$x >= x])
    ssp_x_mn <- max(ssp_xy$x[ssp_xy$x < x])
    ssp_y_mx <- min(ssp_xy$y[ssp_xy$y >= y])
    ssp_y_mn <- max(ssp_xy$y[ssp_xy$y < y])
    ix <- (hst$x %in% c(ssp_x_mx, ssp_x_mn)) & (hst$y %in% c(ssp_y_mx, ssp_y_mn))
    hst_sel <- hst[ix]
    ssp_sel <- ssp[(ssp$x %in% c(ssp_x_mx, ssp_x_mn)) & (ssp$y %in% c(ssp_y_mx, ssp_y_mn))]

    dist_df <- expand.grid(list(x=c(ssp_x_mx, ssp_x_mn), y=c(ssp_y_mx, ssp_y_mn)))

    dist_df$x2 <- (dist_df$x - ssp_x_mn)/(ssp_x_mx - ssp_x_mn)
    dist_df$y2 <- (dist_df$y - ssp_y_mn)/(ssp_y_mx - ssp_y_mn)

    x2 <- (x - ssp_x_mn)/(ssp_x_mx - ssp_x_mn)
    y2 <- (y - ssp_y_mn)/(ssp_y_mx - ssp_y_mn)

    dist_df$w <- ifelse(dist_df$x2 == 1, x2, 1 - x2)*ifelse(dist_df$y2 == 1, y2, 1 - y2)

    dist_df$x2 <- NULL
    dist_df$y2 <- NULL

    hst_sel <- merge(hst_sel, dist_df, by=c('x', 'y')) 
    hst_sel <- hst_sel[ , .(hst = sum(hst*w)), .(date)]

    ssp_sel <- merge(ssp_sel, dist_df, by=c('x', 'y')) 
    ssp_sel <- ssp_sel[ , .(ssp1 = sum(ssp1*w),
                            ssp2 = sum(ssp2*w),
                            ssp3 = sum(ssp3*w),
                            ssp5 = sum(ssp5*w)), .(date)]

    obs_mean <- mean(obs_sel$value)
    obs_stdv <- sd(obs_sel$value)

    mod_mean <- mean(hst_sel$hst)
    mod_stdv <- sd(hst_sel$hst)

    #Use distribution standardizing method from
    #https://stats.stackexchange.com/questions/46429/transform-data-to-desired-mean-and-standard-deviation
    ssp_sel$x <- xy$x[i]
    ssp_sel$y <- xy$y[i]
    ssp_sel$ssp1 <- obs_mean + (ssp_sel$ssp1 - mod_mean) * (obs_stdv/mod_stdv)
    ssp_sel$ssp2 <- obs_mean + (ssp_sel$ssp2 - mod_mean) * (obs_stdv/mod_stdv)
    ssp_sel$ssp3 <- obs_mean + (ssp_sel$ssp3 - mod_mean) * (obs_stdv/mod_stdv)
    ssp_sel$ssp5 <- obs_mean + (ssp_sel$ssp5 - mod_mean) * (obs_stdv/mod_stdv)

    if (i %% 5000 == 0){
      cat(Sys.time(), i, round(i/nrow(xy)*100, 4), 'percent done\n') 
    }

    ssp_sel
  }

  ################################################
  # Write results as Tifs in appropriate folders

  for (d in as.character(unique(ssp_res$date))){
    ssp_res_date <- ssp_res[ssp_res$date == d, ]

    y <- substr(d, 1, 4)
    m <- substr(d, 6, 7)

    for (s in c('SSP1', 'SSP2', 'SSP3', 'SSP5')){
      vars <- c('x', 'y', tolower(s))
      r <- rasterFromXYZ(ssp_res_date[ , ..vars], 
                         crs='+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs',
                         res=0.25)
      writeRaster(extent(r, ref), paste0('~/maldat/covars/0.25dd/', 
                            s, '/', y, '/', m, '/', model, '/tasmax.tif'),
                  format='GTiff', overwrite=T)
    }
  }
  gc()

  system(paste0('telegram "Done processing ', model, '"'))
}


system('telegram "Done with all"')
system('sudo poweroff')

