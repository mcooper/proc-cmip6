setwd('~/maldat/covars/0.25dd')

library(tidyverse)

fs <- system('find . -maxdepth 4 -mindepth 4 | grep SSP.*tif', intern=T)

df <- data.frame(file=fs) %>%
  mutate(var = case_when(grepl('tasmax', file) ~ 'tasmax',
                         grepl('tasmin', file) ~ 'tasmin',
                         TRUE ~ 'tas'),
         filename = basename(file),
         dirname = dirname(file),
         pattern = paste0('_', var, '.tif'),
         model = mapply(gsub, pattern, '', filename))

for (i in 1:nrow(df)){
  file.rename(from=df$file[i],
              to=paste0(df$dirname[i], '/', df$model[i], '/', df$var[i], '.tif'))
}
  


