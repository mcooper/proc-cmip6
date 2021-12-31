###########################
# Make lagged symlinks
# Have to use R for easy group_by logic
#################################3

library(tidyverse)
library(doParallel)

cl <- makeCluster(12, outfile = '')
registerDoParallel(cl)

setwd('~/maldat/covars/')

fs <- data.frame(file=list.files(pattern='tasmin.tif|pr.tif|tas.tif|tasmax.tif', 
                                 recursive=T)) %>%
  mutate(scen = str_extract(file, '(?<=dd/).*(?=/..../)'),
         res = str_extract(file, '[^/]*'),
         year = str_extract(file, '\\d{4}'),
         month = str_extract(file, '(?<=/)\\d{2}(?=/)'),
         basename = basename(file),
         model = str_extract(file, '(?<=/../).*(?=/)')) %>%
  mutate(file01 = gsub('.tif', '_01.tif', lead(file, 1)),
         file02 = gsub('.tif', '_02.tif', lead(file, 2)),
         file03 = gsub('.tif', '_03.tif', lead(file, 3)),
         file04 = gsub('.tif', '_04.tif', lead(file, 4)),
         file05 = gsub('.tif', '_05.tif', lead(file, 5)),
         file06 = gsub('.tif', '_06.tif', lead(file, 6)),
         file07 = gsub('.tif', '_07.tif', lead(file, 7)),
         file08 = gsub('.tif', '_08.tif', lead(file, 8)),
         file09 = gsub('.tif', '_09.tif', lead(file, 9)),
         file10 = gsub('.tif', '_10.tif', lead(file, 10)),
         file11 = gsub('.tif', '_11.tif', lead(file, 11)),
         file12 = gsub('.tif', '_12.tif', lead(file, 12)),
         file13 = gsub('.tif', '_13.tif', lead(file, 13)),
         file14 = gsub('.tif', '_14.tif', lead(file, 14)),
         file15 = gsub('.tif', '_15.tif', lead(file, 15)),
         file16 = gsub('.tif', '_16.tif', lead(file, 16)),
         file17 = gsub('.tif', '_17.tif', lead(file, 17)),
         file18 = gsub('.tif', '_18.tif', lead(file, 18)),
         file19 = gsub('.tif', '_19.tif', lead(file, 19)),
         file20 = gsub('.tif', '_20.tif', lead(file, 20)),
         file21 = gsub('.tif', '_21.tif', lead(file, 21)),
         file22 = gsub('.tif', '_22.tif', lead(file, 22)),
         file23 = gsub('.tif', '_23.tif', lead(file, 23)))

foreach(i=1:nrow(fs)) %dopar% {
  cat(i/nrow(fs), '\n')
  if (!is.na(fs$file01[i])){
    system(paste0('ln -sf /home/mattcoop/maldat/covars/',
                  fs$file[i], ' /home/mattcoop/maldat/covars/', fs$file01[i]))
  }
  if (!is.na(fs$file02[i])){
    system(paste0('ln -sf /home/mattcoop/maldat/covars/',
                  fs$file[i], ' /home/mattcoop/maldat/covars/', fs$file02[i]))
  }
  if (!is.na(fs$file03[i])){
    system(paste0('ln -sf /home/mattcoop/maldat/covars/',
                  fs$file[i], ' /home/mattcoop/maldat/covars/', fs$file03[i]))
  }
  if (!is.na(fs$file04[i])){
    system(paste0('ln -sf /home/mattcoop/maldat/covars/',
                  fs$file[i], ' /home/mattcoop/maldat/covars/', fs$file04[i]))
  }
  if (!is.na(fs$file05[i])){
    system(paste0('ln -sf /home/mattcoop/maldat/covars/',
                  fs$file[i], ' /home/mattcoop/maldat/covars/', fs$file05[i]))
  }
  if (!is.na(fs$file06[i])){
    system(paste0('ln -sf /home/mattcoop/maldat/covars/',
                  fs$file[i], ' /home/mattcoop/maldat/covars/', fs$file06[i]))
  }
  if (!is.na(fs$file07[i])){
    system(paste0('ln -sf /home/mattcoop/maldat/covars/',
                  fs$file[i], ' /home/mattcoop/maldat/covars/', fs$file07[i]))
  }
  if (!is.na(fs$file08[i])){
    system(paste0('ln -sf /home/mattcoop/maldat/covars/',
                  fs$file[i], ' /home/mattcoop/maldat/covars/', fs$file08[i]))
  }
  if (!is.na(fs$file09[i])){
    system(paste0('ln -sf /home/mattcoop/maldat/covars/',
                  fs$file[i], ' /home/mattcoop/maldat/covars/', fs$file09[i]))
  }
  if (!is.na(fs$file10[i])){
    system(paste0('ln -sf /home/mattcoop/maldat/covars/',
                  fs$file[i], ' /home/mattcoop/maldat/covars/', fs$file10[i]))
  }
  if (!is.na(fs$file11[i])){
    system(paste0('ln -sf /home/mattcoop/maldat/covars/',
                  fs$file[i], ' /home/mattcoop/maldat/covars/', fs$file11[i]))
  }
  if (!is.na(fs$file12[i])){
    system(paste0('ln -sf /home/mattcoop/maldat/covars/',
                  fs$file[i], ' /home/mattcoop/maldat/covars/', fs$file12[i]))
  }
  if (!is.na(fs$file13[i])){
    system(paste0('ln -sf /home/mattcoop/maldat/covars/',
                  fs$file[i], ' /home/mattcoop/maldat/covars/', fs$file13[i]))
  }
  if (!is.na(fs$file14[i])){
    system(paste0('ln -sf /home/mattcoop/maldat/covars/',
                  fs$file[i], ' /home/mattcoop/maldat/covars/', fs$file14[i]))
  }
  if (!is.na(fs$file15[i])){
    system(paste0('ln -sf /home/mattcoop/maldat/covars/',
                  fs$file[i], ' /home/mattcoop/maldat/covars/', fs$file15[i]))
  }
  if (!is.na(fs$file16[i])){
    system(paste0('ln -sf /home/mattcoop/maldat/covars/',
                  fs$file[i], ' /home/mattcoop/maldat/covars/', fs$file16[i]))
  }
  if (!is.na(fs$file17[i])){
    system(paste0('ln -sf /home/mattcoop/maldat/covars/',
                  fs$file[i], ' /home/mattcoop/maldat/covars/', fs$file17[i]))
  }
  if (!is.na(fs$file18[i])){
    system(paste0('ln -sf /home/mattcoop/maldat/covars/',
                  fs$file[i], ' /home/mattcoop/maldat/covars/', fs$file18[i]))
  }
  if (!is.na(fs$file19[i])){
    system(paste0('ln -sf /home/mattcoop/maldat/covars/',
                  fs$file[i], ' /home/mattcoop/maldat/covars/', fs$file19[i]))
  }
  if (!is.na(fs$file20[i])){
    system(paste0('ln -sf /home/mattcoop/maldat/covars/',
                  fs$file[i], ' /home/mattcoop/maldat/covars/', fs$file20[i]))
  }
  if (!is.na(fs$file21[i])){
    system(paste0('ln -sf /home/mattcoop/maldat/covars/',
                  fs$file[i], ' /home/mattcoop/maldat/covars/', fs$file21[i]))
  }
  if (!is.na(fs$file22[i])){
    system(paste0('ln -sf /home/mattcoop/maldat/covars/',
                  fs$file[i], ' /home/mattcoop/maldat/covars/', fs$file22[i]))
  }
  if (!is.na(fs$file23[i])){
    system(paste0('ln -sf /home/mattcoop/maldat/covars/',
                  fs$file[i], ' /home/mattcoop/maldat/covars/', fs$file23[i]))
  }
}

