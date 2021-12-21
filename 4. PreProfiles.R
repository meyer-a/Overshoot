# Code used to generate pre-profiles, i.e. transform the raw results into a summary with the data used in further analyses

library(stringr)
library(Gmisc)
library(dplyr)
library(parallel)

rm(list = ls())

if(getwd()=="C:/Users/pc/Dropbox/Analyses/ColExt") {
  path <- "C:/Users/pc/Dropbox/rangeHorizonsData/Andreas_data/Horizons_model_output/"
  source("functions/getProfiles_v2.R")
  source("functions/getExposureYears.R")
  parallel <- "yes"
}

if(getwd()=="/research-home/ameyer") {
  path <- "../../nfs/geoengineering-data/rangeHorizonsData/Andreas_data/Overshoot/"
  source("../../nfs/geoengineering-data/Analyses/Overshoot/functions/getProfiles_v2.R")
  source('../../nfs/geoengineering-data/Analyses/Overshoot/functions/getExposureYears.R')
  parallel <- "no"
  
}


file.names <- list.files(paste0(path, "rawResults/"), rec=T)
file.names <- grep("1850_2015", file.names, value = T)


files <- list.files(paste0(path, "rawResults/"), rec=T, full.names=T)
files <- grep("1850_2015", files, value = T)


for(i in 1:length(files)){
  
  # load file and turn into a df
  load(files[i])
  
  rawResults <- as.data.frame(fastDoCall(rbind, rawResults))
  
  gc()
  
  
  rawResults <- na.omit(rawResults)
  
  rawResults <- split(rawResults, rawResults$WorldID)
  
  gc()
  
  # create folder to save file
  my.names <- str_split(file.names[i], pattern = "/")
  path2 <- paste0(path, "preProfiles/",my.names[[1]][1],"/",my.names[[1]][2])

  if(!file.exists(path2)) dir.create(path2, recursive = TRUE)
  
  # calculate profiles

    system.time({
      preProfiles <- lapply(rawResults, getProfiles)

      
      rm(rawResults)
      gc()
      
      
    })
  
  
  save(preProfiles, file = paste0(path2,"/",my.names[[1]][3]))
  
  print(paste(my.names[[1]][3],"saved!"))
  
}



