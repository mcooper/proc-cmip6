#Run first in shell

sudo mkdir /mnt/worldclim
sudo chown mattcoop /mnt/worldclim
cp -r ~/mortalityblob/worldclim/static/GLWD-level3/glwd_3 /mnt/worldclim/
cp ~/mortalityblob/worldclim/static/wc2.1_2.5m_elev.tif /mnt/worldclim/

#Now run in R
library(raster)
library(tidyverse)
library(foreach)
library(doParallel)

#https://www.worldwildlife.org/publications/global-lakes-and-wetlands-database-lakes-and-wetlands-grid-level-3
lakes <- raster('~/maldat/raw/glwd_3/hdr.adf')

ref <- raster('~/maldat/raw/worldclim_elevation/wc2.1_2.5m_elev.tif') %>%
  crop(extent(-18, 53, -36, 25))

lakes <- raster::resample(lakes, ref, method='ngb')

types <- c("lake", "reservoir", "river", "fresh_marsh_floodplain",
           "swamp_forest", "coastal_wetland",
           "brackish_wetland", "peatland", "intermittent", 
           "50_100_wetland", "25_50_wetland", "0_25_wetland")

cl <- makeCluster(9, outfile = '')
registerDoParallel(cl)

foreach(i=1:9, .packages=c('raster')) %dopar% {
  print(types[i])
  sel <- lakes == i
  sel[sel == 0] <- NA
  d <- distance(sel, doEdge=TRUE, 
                filename=paste0('~/maldat/covars/2.5/static/distance_to_',
                                types[i], '.tif'),
                format="GTiff")
  aggregate(d, fact=4, fun=mean, na.rm=T, 
            filename=paste0('~/maldat/covars/10/static/distance_to_',
                                types[i], '.tif'),
                format="GTiff")

}
