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

# Use OECD model data (it has the most countries)
# Get GPD over time, then annual rate of growth
ssp <- read_csv('SSPs/SspDb_country_data_2013-06-12.csv') %>%
  filter(VARIABLE %in% c('GDP|PPP', 'Population'), MODEL=='OECD Env-Growth') %>%
  rename(ISO3=REGION, SSP=SCENARIO) %>%
  dplyr::select(-MODEL, -UNIT) %>%
  gather(YEAR, VALUE, -ISO3, -SSP, -VARIABLE) %>%
  spread(VARIABLE, VALUE) %>%
  rename(GDP = `GDP|PPP`,
         POP = Population) %>%
  mutate(YEAR = as.numeric(YEAR),
         SSP = substr(SSP, 1, 4)) %>%
  na.omit() %>%
  group_by(ISO3, SSP) %>%
  complete(YEAR=2010:2100) %>%
  mutate(GDP=na.approx(GDP)*1000000000,
         GDP_rate=GDP/lag(GDP),
         POP=na.approx(POP)*1000000000,
         POP_rate=POP/lag(POP)) %>%
  dplyr::select(-GDP, -POP)

# Add impacts of COVID on 2020 growth rate
covid <- read.csv('worldbank/covid_impact.csv') %>%
  gather(YEAR, roc_pred, -ISO3) %>%
  mutate(YEAR = as.numeric(substr(YEAR, 2, 5)),
         roc_pred = 1 + roc_pred/100) %>%
  filter(YEAR %in% c(2020, 2021))

ssp <- merge(ssp, covid, all.x=T) %>%
  mutate(GDP_rate = ifelse(YEAR == 2020 & !is.na(roc_pred), roc_pred, GDP_rate)) %>%
  dplyr::select(-roc_pred)

#Assume South Sudan follows identical trajectories to CAR
ssp <- bind_rows(ssp,
                 ssp %>%
                   filter(ISO3 == 'CAF') %>%
                   mutate(ISO3 = 'SSD'))

###########################################################################################
# Adjust SSP projections to match latest data from GDL
########################################################################################

sf <- read_sf('gdl_shdi', 'GDL Shapefiles V4 Edit') %>%
  rename(GDLCODE=GDLcode)
sf <- sf[!is.na(st_dimension(sf)), ]

shdi <- read.csv('gdl_shdi/SHDI Complete 4.0.csv') %>%
  filter(GDLCODE %in% sf$GDLCODE) %>%
  mutate(gni_percap_2011=gnic*1000,
         pop=pop*1000000,
         gni=gni_percap_2011*pop,
         ISO3=substr(GDLCODE, 1, 3)) %>%
  dplyr::select(ISO3, GDLCODE, YEAR=year, gni, pop) %>%
  group_by(ISO3, YEAR) %>%
  summarize(pop = sum(pop),
            gni = sum(gni)) %>%
  filter(YEAR >= 2010)

ssp <- merge(ssp, shdi, all.x=T)

grow <- function(var, rate){
  minv <- max(which(!is.na(var)))
  for (i in (minv + 1):length(var)){
    var[i] <- var[i - 1]*rate[i]
  }
  var
}

cty <- ne_countries(returnclass='sf')

ssp <- ssp %>%
  #Subset to Africa
  filter(ISO3 %in% cty$iso_a3[cty$continent == 'Africa']) %>%
  group_by(ISO3, SSP) %>%
  arrange(YEAR) %>%
  mutate(gni = grow(gni, GDP_rate),
         pop = grow(pop, POP_rate)) %>%
  dplyr::select(-GDP_rate, -POP_rate)

#######################################
# Get most recent population shares
#######################################
#https://globaldatalab.org/shdi/download_files/
#https://globaldatalab.org/assets/2020/03/SHDI%20Complete%204.0%20%281%29.csv
shdi <- read.csv('gdl_shdi/SHDI Complete 4.0.csv') %>%
  filter(GDLCODE %in% sf$GDLCODE) %>%
  mutate(gni_percap_2011=gnic*1000,
         pop=pop*1000000,
         gni=gni_percap_2011*pop,
         ISO3=substr(GDLCODE, 1, 3)) %>%
  dplyr::select(ISO3, GDLCODE, YEAR=year, gni, pop) %>%
  group_by(ISO3, YEAR) %>%
  mutate(pop_share = pop/sum(pop),
         gni_share = gni/sum(gni)) %>%
  ungroup %>%
  filter(YEAR == 2018) %>%
  dplyr::select(ISO3, GDLCODE, pop_share, gni_share)

ssp <- merge(ssp, shdi) %>%
  mutate(pop = pop*pop_share,
         gni = gni*gni_share,
         gni_percap = gni/pop) %>%
  dplyr::select(-pop_share, -gni_share)
         
##################################
# Write Rasters
##################################
ref25 <- raster('~/maldat/covars/0.25dd/elevation.tif')
ref05 <- raster('~/maldat/covars/0.05dd/elevation.tif')

# Write historic 2010-2018 data at 0.05dd
for (y in 2010:2018){
  sel <- merge(sf, 
               ssp %>% 
                filter(YEAR == y,
                       SSP == 'SSP1'))
  r <- rasterize(as(sel, Class='Spatial'), ref05, field='gni_percap',
                 filename=paste0('~/maldat/covars/0.05dd/historic/', y, '/gni_percap.tif'),
                 format='GTiff', overwrite=T)
  print(y)
}

# Write historic 2010-2018 data at 0.25dd
for (y in 2010:2018){
  sel <- merge(sf, 
               ssp %>% 
                filter(YEAR == y,
                       SSP == 'SSP1'))
  r <- rasterize(as(sel, Class='Spatial'), ref25, field='gni_percap',
                 filename=paste0('~/maldat/covars/0.25dd/historic/', y, '/gni_percap.tif'),
                 format='GTiff', overwrite=T)
  print(y)
}

# Write future SSP data
for (y in 2015:2100){
  for (s in c('SSP1', 'SSP2', 'SSP3', 'SSP5')){
    sel <- merge(sf, 
                 ssp %>% 
                  filter(YEAR == y,
                         SSP == s))
    r <- rasterize(as(sel, Class='Spatial'), ref25, field='gni_percap',
                   filename=paste0('~/maldat/covars/0.25dd/', s, '/', y, '/gni_percap.tif'),
                   format='GTiff', overwrite=T)
    print(s)
  }
  print(y)
}




























