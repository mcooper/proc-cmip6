setwd('~/maldat/raw/')

library(zoo)
library(tidyverse)
library(countrycode)
library(rnaturalearth)
library(sf)
library(raster)

##################################################################################
# Read in SSP Data
##############################################################################

# From http://dataexplorer.wittgensteincentre.org/wcde-v2/
ssp <- read_csv('SSPs/wcde_data.csv', skip=8) %>%
  mutate(ISO3 = countrycode(Area, 'country.name', 'iso3c'),
         YEAR = as.numeric(str_extract(Period, '\\d+')) + 1) %>%
  dplyr::select(-Area, -Period) %>%
  rename(LIFEXP=Years, SSP=Scenario) %>%
  group_by(SSP, ISO3, YEAR) %>%
  # Assume male and females are 50/50, take mean of lifeexps
  summarize(LIFEXP=mean(LIFEXP)) %>%
  filter(!is.na(ISO3))

#Use SSP1 for SSP5 and SSP3 for SSP4
ssp2 <- bind_rows(ssp,
                 ssp %>% 
                   filter(SSP == 'SSP1') %>%
                   mutate(SSP = 'SSP5'),
                 ssp %>%
                   filter(SSP == 'SSP3') %>%
                   mutate(SSP = 'SSP4'))

# Fill in missing years
ssp3 <- ssp2 %>%
  group_by(SSP, ISO3) %>%
  complete(YEAR=1951:2100) %>%
  mutate(LIFEXP = na.locf(na.approx(LIFEXP, na.rm=F)),
         LIFEXP_inc = LIFEXP - lag(LIFEXP))

##################################
# Read in 
sf <- read_sf('gdl_shdi', 'GDL Shapefiles V4 Edit') %>%
  rename(GDLCODE=GDLcode)
#sf <- sf[!is.na(st_dimension(sf)), ]

shdi <- read.csv('gdl_shdi/SHDI Complete 4.0.csv') %>%
  filter(GDLCODE %in% sf$GDLCODE) %>%
  mutate(ISO3=substr(GDLCODE, 1, 3),
         YEAR=year) %>%
  group_by(ISO3, YEAR) %>%
  summarize(lifexp = weighted.mean(lifexp, w=pop))

#################################
# Combine and carry forward

m <- merge(ssp3, shdi, all=T) %>%
  filter(!is.na(SSP))

grow <- function(var, rate){
  minv <- max(which(!is.na(var)))
  for (i in (minv + 1):length(var)){
    var[i] <- var[i - 1] + rate[i]
  }
  var
}

m2 <- m %>%
  group_by(SSP, ISO3) %>%
  filter(!all(is.na(lifexp))) %>%
  mutate(lifexp = grow(lifexp, LIFEXP_inc)) %>%
  dplyr::select(-LIFEXP, -LIFEXP_inc) %>%
  filter(!is.na(lifexp))

##############################################
# Get regional differences from national mean,
# Apply to future estimates
##############################################

shdi2 <- read.csv('gdl_shdi/SHDI Complete 4.0.csv') %>%
  filter(GDLCODE %in% sf$GDLCODE,
         year == 2018) %>%
  dplyr::select(lifexp, GDLCODE, ISO3=iso_code) %>%
  group_by(ISO3) %>%
  mutate(lifexp_reg = lifexp - mean(lifexp)) %>%
  dplyr::select(-lifexp)

new <- merge(m2, shdi2) %>%
  mutate(lifexp = lifexp + lifexp_reg)


##################################
# Write Rasters
##################################
ref25 <- raster('~/maldat/covars/0.25dd/elevation.tif')
ref05 <- raster('~/maldat/covars/0.05dd/elevation.tif')
sf <- sf[!is.na(st_dimension(sf)), ]

ref <- raster('~/maldat/raw/CMIP6/daily/ssp126_BCC-CSM2-MR_pr.nc')

# Write historic 2010-2018 data at 0.05dd
for (y in 2010:2018){
  sel <- merge(sf, 
               new %>% 
                filter(YEAR == y,
                       SSP == 'SSP1'))
  rasterize(as(sel, Class='Spatial'), ref05, field='lifexp',
            filename=paste0('~/maldat/covars/0.05dd/historic/', y, '/lifexp.tif'),
            format='GTiff', overwrite=T)
  print(y)
}

# Write historic 2010-2018 data at 0.25dd
for (y in 2010:2018){
  sel <- merge(sf, 
               new %>% 
                filter(YEAR == y,
                       SSP == 'SSP1'))
  rasterize(as(sel, Class='Spatial'), ref25, field='lifexp',
            filename=paste0('~/maldat/covars/0.25dd/historic/', y, '/lifexp.tif'),
            format='GTiff', overwrite=T)
  print(y)
}

# Write future SSP data
for (y in 2015:2100){
  for (s in c('SSP1', 'SSP2', 'SSP3', 'SSP5')){
    sel <- merge(sf, 
                 new %>% 
                  filter(YEAR == y,
                         SSP == s))
    rasterize(as(sel[as.numeric(st_area(sel)) > 0, ], Class='Spatial'), ref, field='lifexp',
              filename=paste0('~/maldat/covars/flood/', s, '_', y, 'lifexp.tif'),
              format='GTiff', overwrite=T)
    print(s)
  }
  print(y)
}
