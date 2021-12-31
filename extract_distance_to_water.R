#Now run in R
library(raster)
library(tidyverse)

setwd('~/mortalityblob/worldclim/pred/2.5/')

rasts <- list.files(pattern='distance')

mal <- read.csv('~/mortalityblob/malaria/malaria_geo.csv')

sp <- SpatialPoints(mal[ , c('longitude', 'latitude')])

for (rast in rasts){
  r <- raster(rast)

  mal[ , gsub('.tif', '', rast)] <- raster::extract(r, sp)
}

write.csv(mal, '~/mortalityblob/worldclim/training/distance_to_water.csv', row.names=F)

system('~/telegram.sh "Done with distance")
