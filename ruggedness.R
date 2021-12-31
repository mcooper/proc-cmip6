library(raster)
library(tidyverse)
library(rnaturalearth)

mal <- read.csv('~/maldat/train/map_data.csv')

elev <- raster('~/maldat/raw/worldclim_elevation/wc2.1_2.5m_elev.tif')

######################################
# Extract climate for mal geo
######################################
mal <- SpatialPointsDataFrame(coords=mal[ , c('longitude', 'latitude')], 
                              data=mal,
                              proj4string=elev@crs)

mal@data$elevation <- raster::extract(elev, mal)
mal@data$elevation[is.na(mal@data$elevation)] <- 0

write.csv(mal@data[ , c('id', 'elevation')], '~/maldat/train/elevation.csv', row.names=F)

######################################
# Crop and resample elevation to 0.25
######################################
elev25 <- crop(elev, extent(c(xmin=-20, xmax = 55, ymin = -35, ymax = 25)))
elev25 <- aggregate(elev25, fact=6, fun=mean)

#Mask out countries outside of SSA + Yemen
cty <- ne_countries()
cty$ssa <- (cty$region_wb == 'Sub-Saharan Africa') | (cty$iso_a3 %in% c("YEM", "DJI"))
cty <- rasterize(cty, elev25, field='ssa')

Mode <- function(x, na.rm=TRUE) {
  x <- x[!is.na(x)]
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#Grow raster to mask coasts
for (i in 1:5){
  cty <- focal(cty, w=matrix(rep(1, 9), ncol=3), fun=Mode, na.rm=T)
}

elev25[cty == 0 | is.na(cty)] <- NA

# Write 0.25dd rasters
writeRaster(elev25, '~/maldat/covars/0.25dd/elevation.tif', format='GTiff', overwrite=T)

######################################
# Crop and resample elevation to 0.05
######################################
elev05 <- resample(elev, raster('~/maldat/raw/chirps/afr/chirps-v2.0.1981.01.tif'))

#Mask out countries outside of SSA + Yemen
cty <- ne_countries()
cty$ssa <- (cty$region_wb == 'Sub-Saharan Africa') | (cty$iso_a3 %in% c("YEM", "DJI"))
cty <- rasterize(cty, elev05, field='ssa')

Mode <- function(x, na.rm=TRUE) {
  x <- x[!is.na(x)]
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#Grow raster to mask coasts
for (i in 1:5){
  cty <- focal(cty, w=matrix(rep(1, 9), ncol=3), fun=Mode, na.rm=T)
}

elev05[cty == 0 | is.na(cty)] <- NA

# Write 0.25dd rasters
writeRaster(elev05, '~/maldat/covars/0.25dd/elevation.tif', format='GTiff', overwrite=T)
