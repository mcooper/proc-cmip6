library(tidyverse)
library(sf)

ndvi <- read.csv('~/maldat/tmp/ndvi_raw.csv')
ll <- read.csv('~/maldat/train/map_data.csv')

makeQA <- function(x){
  bit <- intToBits(x)

  data.frame(cloudy=bit[2]=='01',
             cloud.shadow=bit[3]=='01',
             over.water=bit[4]=='01',
             over.sunglint=bit[5]=='01',
             dense.veget=bit[6]=='01',
             at.night=bit[7]=='01',
             ch1.5.valid=bit[8]=='01',
             ch1.invalid=bit[9]=='01',
             ch2.invalid=bit[10]=='01',
             ch3.invalid=bit[11]=='01',
             ch4.invalid=bit[12]=='01',
             ch5.invalid=bit[13]=='01',
             rho3.invalid=bit[14]=='01',
             brdf.invalid=bit[15]=='01',
             polar=bit[16]=='01')
}

all <- lapply(X=ndvi$QA, makeQA)
allb <- bind_rows(all)

ndvic <- cbind(ndvi, allb)

ndvill <- merge(ndvic, ll %>% select(id, latitude, longitude) %>% unique)

# #Explore Data
# ndvillgp <- ndvill %>%
#   group_by(code, latitude, longitude) %>%
#   summarize(ndvina=mean(TIMEOFDAY, na.rm=T))
# 
# ggplot(ndvillgp) + 
#   geom_point(aes(y=latitude, x=longitude, color=ndvina))
# 
# sel <- ndvill %>%
#   filter(latitude < -4, latitude > -6, longitude < 20) %>

###############################
# Remove:
#  - Cloudy
#  - Over Water
#  - is.na(ndvi)

ndvillgp <- ndvill %>%
  #Set NDVI to NA where cloudy
  mutate(NDVI=ifelse(cloudy | over.water | over.sunglint | at.night, NA, NDVI)) %>%
  group_by(latitude, longitude, id, year, month) %>%
  summarize(NDVI=median(NDVI, na.rm=T)) %>%
  st_as_sf(coords=c("longitude", "latitude"), remove=F)

ndvi_nas <- ndvillgp %>%
  filter(is.na(NDVI))
ndvi_nona <- ndvillgp %>%
  filter(!is.na(NDVI))

ds <- st_distance(ndvi_nona, ndvi_nas)
ndvi_nas$NDVI <- ndvi_nona$NDVI[apply(ds, MARGIN=2, which.min)]

ndvi_final <- bind_rows(ndvi_nas, ndvi_nona) %>%
  st_drop_geometry

ggplot(ndvi_final) + 
  geom_point(aes(x=longitude, y=latitude, color=NDVI)) + 
  scale_color_gradientn(colors=ihme_cols)

write.csv(ndvi_final %>% select(-latitude, -longitude, -year, -month), 
          '~/maldat/tmp/ndvi_raw.csv', row.names=F)




















