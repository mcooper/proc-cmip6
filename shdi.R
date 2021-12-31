library(sf)
library(tidyverse)
library(raster)
library(sp)

dat <- read.csv('~/maldat/train/map_data.csv')

gdlsp <- read_sf('~/maldat/raw/gdl_shdi', 'GDL Shapefiles V4')

shdi <- read.csv('~/maldat/raw/gdl_shdi/SHDI Complete 4.0 (1).csv')

#######################################
# Extract
#########################################

all_mal <- data.frame()
for (i in 1990:2018){
  print(i)
  if (i == 1990){
    mal_sel <- dat %>%
      filter(year <= 1990)
  }else if (i == 2018){
    mal_sel <- dat %>%
      filter(year >= 2018)
  }else{
    mal_sel <- dat %>%
      filter(year == i)
  }

  shdi_sel <- shdi %>%
    filter(year == i)

  mal_sel_sp <- mal_sel %>%
    st_as_sf(coords=c('longitude', 'latitude'), crs=st_crs(gdlsp))

  gdlspm <- merge(gdlsp %>% dplyr::select(GDLCODE=GDLcode), shdi_sel)

  ix <- st_nearest_feature(mal_sel_sp, gdlspm)
  mal_sel[ , c('shdi_lifexp', 'shdi_gnic', 'shdi_esch', 'shdi_msch', 'shdi_pop')] <-
    st_drop_geometry(gdlspm[ix, c('lifexp', 'gnic', 'esch', 'msch', 'pop')])

  all_mal <- bind_rows(all_mal,
                       mal_sel %>% 
                         dplyr::select(id, shdi_lifexp, shdi_gnic, shdi_esch, shdi_msch, shdi_pop))
}

write.csv(all_mal, '~/maldat/train/shdi.csv', row.names=F)

###############################
# Write Raster
##############################

ref <- raster('~/maldat/covars/10/2018/Apr/mean_annual_precip.tif')

for(v in c('lifexp', 'gnic', 'esch', 'msch', 'pop')){
  print(v)
  #Use 2018 values from last iteration of Extract for loop
  rasterize(as_Spatial(gdlspm), ref, field=v,
            filename=paste0('~/maldat/covars/10/2018/shdi_', v, '.tif'), format='GTiff')
}
