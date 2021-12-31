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

# Mean years of education for people over 25
mye <- read_csv('SSPs/SspDb_country_data_2013-06-12.csv') %>%
  filter(grepl('Education', VARIABLE)) %>%
  rename(ISO3=REGION, SSP=SCENARIO) %>%
  dplyr::select(-MODEL, -UNIT) %>%
  gather(YEAR, VALUE, -ISO3, -SSP, -VARIABLE) %>%
  filter(!is.na(VALUE)) %>%
  mutate(AGE = as.numeric(str_extract(VARIABLE, '\\d+')),
         VARIABLE = case_when(grepl("No Ed", VARIABLE) ~ 0,
                              grepl("Primary", VARIABLE) ~ 5,
                              grepl("Secondary", VARIABLE) ~ 10,
                              grepl("Tertiary", VARIABLE) ~ 15),
         YEAR = as.numeric(YEAR),
         SSP = substr(SSP, 1, 4)) %>%
  filter(AGE >= 25) %>%
  group_by(SSP, ISO3, YEAR) %>%
  summarize(MYE=weighted.mean(VARIABLE, w=VALUE))

eye <- read_csv('SSPs/SspDb_country_data_2013-06-12.csv') %>%
  filter(grepl('Education', VARIABLE)) %>%
  rename(ISO3=REGION, SSP=SCENARIO) %>%
  dplyr::select(-MODEL, -UNIT) %>%
  gather(YEAR, VALUE, -ISO3, -SSP, -VARIABLE) %>%
  filter(!is.na(VALUE)) %>%
  mutate(AGE = as.numeric(str_extract(VARIABLE, '\\d+')),
         VARIABLE = case_when(grepl("No Ed", VARIABLE) ~ 0,
                              grepl("Primary", VARIABLE) ~ 5,
                              grepl("Secondary", VARIABLE) ~ 10,
                              grepl("Tertiary", VARIABLE) ~ 15),
         YEAR = as.numeric(YEAR),
         SSP = substr(SSP, 1, 4)) %>%
  filter(AGE == 20) %>%
  group_by(SSP, ISO3, YEAR) %>%
  summarize(EYE=weighted.mean(VARIABLE, w=VALUE))

ye <- merge(eye, mye) %>%
  group_by(SSP, ISO3) %>%
  complete(YEAR = 2010:2100) %>%
  mutate(EYE = na.approx(EYE),
         MYE = na.approx(MYE))

###########################################
# Get mean EYE and MYE at national level
#######################################
sf <- read_sf('gdl_shdi', 'GDL Shapefiles V4 Edit') %>%
  rename(GDLCODE=GDLcode)
#sf <- sf[!is.na(st_dimension(sf)), ]

shdi <- read.csv('gdl_shdi/SHDI Complete 4.0.csv') %>%
  filter(GDLCODE %in% sf$GDLCODE) %>%
  mutate(ISO3=substr(GDLCODE, 1, 3),
         YEAR=year) %>%
  group_by(ISO3, YEAR) %>%
  summarize(msch = weighted.mean(msch, w=pop),
            esch = weighted.mean(esch, w=pop))

#####################################################
# Convert years of ed into raw change over time,
# Combine with SHDI to get standardized trajectories
#####################################################

ye <- ye %>%
  group_by(SSP, ISO3) %>%
  mutate(EYE_increase = EYE - lag(EYE),
         MYE_increase = MYE - lag(MYE))

#Use CAR values for South Sudan
ye <- bind_rows(ye,
                  ye %>%
                    filter(ISO3 == 'CAF') %>%
                    mutate(ISO3 = 'SSD'))

grow <- function(var, rate){
  minv <- max(which(!is.na(var)))
  for (i in (minv + 1):length(var)){
    var[i] <- var[i - 1] + rate[i]
  }
  var
}

comb <- merge(ye, shdi, all.x=T, all.y=F) %>%
  group_by(SSP, ISO3) %>%
  filter(!all(is.na(msch))) %>%
  mutate(esch = grow(esch, EYE_increase),
         msch = grow(msch, MYE_increase)) %>%
  dplyr::select(-matches('EYE|MYE'))

##############################################
# Get regional differences from national mean,
# Apply to future estimates
##############################################

shdi2 <- read.csv('gdl_shdi/SHDI Complete 4.0.csv') %>%
  filter(GDLCODE %in% sf$GDLCODE,
         year == 2018) %>%
  dplyr::select(msch, esch, GDLCODE, ISO3=iso_code) %>%
  group_by(ISO3) %>%
  mutate(msch_reg = msch - mean(msch),
         esch_reg = esch - mean(esch)) %>%
  dplyr::select(-esch, -msch)

new <- merge(comb, shdi2) %>%
  mutate(esch = esch + esch_reg,
         msch = msch + msch_reg)


##################################
# Write Rasters
##################################
ref <- raster('~/maldat/covars/0.25dd/SSP1/2015/Apr/ACCESS-CM2_pr.tif')
sf <- sf[!is.na(st_dimension(sf)), ]

# Write historic 2010-2018 data
for (y in 2010:2018){
  sel <- merge(sf, 
               new %>% 
                filter(YEAR == y,
                       SSP == 'SSP1'))
  rasterize(as(sel, Class='Spatial'), ref, field='msch',
            filename=paste0('~/maldat/covars/0.25dd/historic/', y, '/msch.tif'),
            format='GTiff', overwrite=T)
  rasterize(as(sel, Class='Spatial'), ref, field='esch',
            filename=paste0('~/maldat/covars/0.25dd/historic/', y, '/esch.tif'),
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
    rasterize(as(sel, Class='Spatial'), ref, field='msch',
              filename=paste0('~/maldat/covars/0.25dd/', s, '/', y, '/msch.tif'),
              format='GTiff', overwrite=T)
    rasterize(as(sel, Class='Spatial'), ref, field='esch',
              filename=paste0('~/maldat/covars/0.25dd/', s, '/', y, '/esch.tif'),
              format='GTiff', overwrite=T)
    print(s)
  }
  print(y)
}
