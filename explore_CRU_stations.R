library(raster)
library(ncdf4)
library(stringr)


d  <- readLines('~/maldat/raw/cru_ts_4/cru_ts4.03.2011.2018.tmn.stn')

split <- function(x){
  a <- str_split(x, ' ')[[1]]
  a <- a[a!= '']
  as.numeric(a)
}

n <- lapply(d, split)

s <- do.call(rbind, n[(34560 - 360):34560])

r <- raster(s)
r[r == -999] <- NA
plot(r)
