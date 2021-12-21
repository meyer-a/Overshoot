# Code to generate the raw results used to construct the horizons profiles
# Raw results are matrices in which rows represent the species occurring in an assemblage (i.e. grid cell)
# and columns are the years in the time series. The cell is assigned with 1 if the climate is suitable
# for the species in a given year. The cell is assigned with 0 if the climate is unsuitable.

library(stringr)
library(foreign)
library(openxlsx)
library(janitor)
library(raster)
library(gtools)
library(openxlsx)

if(getwd()=="C:/Users/pc/Dropbox/Analyses/Overshoot") {
  my.path <- "C:/Users/pc/Dropbox/"
}


if(getwd()=="/nfs/geoengineering-data") {
  my.path <- "/nfs/geoengineering-data/"
}




MarineOrTerrestrial <- "Terrestrial"
period <- "1850_2015"
##############################################
### load all data
##############################################
if(MarineOrTerrestrial == "Marine"){
  
  # grid
  load(paste0(my.path,"rangeHorizonsData/Andreas_data/Grid/OceanGrid_without_central_america.rda"))
  my.grid <- OceanGrid
  
  # load(paste0(my.path,"rangeHorizonsData/Andreas_data/Grid/BehrmannMeterGrid_WGS84_land.rda")) 
  # land.grid <- griddedDomain
  
  rm("OceanGrid")
  
  # range data
  load(paste0(my.path,"rangeHorizonsData/Andreas_data/species_data/MarineRanges.rda"))
  
  # niche limits
  load(paste0(my.path,"rangeHorizonsData/Andreas_data/Overshoot/nicheLimits/1_",period,"_2100_0_AllMarineSpecies_Tmax.rda"))
  tmax <- nicheLimitMat
  
  
  # climate data names | not loading climate variables yet to avoid memory issues
  TempTimeSeries <- list.files(paste0(my.path,"rangeHorizonsData/Andreas_data/Overshoot/tempMatrices"), pattern = "tempPostMatOcean")
  TempTimeSeries <- grep(period, TempTimeSeries, value = T)
  
  # table with the list of species in each group
  group.table <- read.csv(paste0(my.path,"rangeHorizonsData/Andreas_data/species_data/Marine_species.csv"), sep=';')
  
}


##############################################
### load all data
##############################################
if(MarineOrTerrestrial == "Terrestrial"){
  
  # grid
  load(paste0(my.path,"rangeHorizonsData/Andreas_data/Grid/BehrmannMeterGrid_WGS84_land.rda"))
  my.grid <- griddedDomain
  
  # load(paste0(my.path,"rangeHorizonsData/Andreas_data/Grid/BehrmannMeterGrid_WGS84_land.rda")) 
  # land.grid <- griddedDomain
  
  rm("griddedDomain")
  
  # range data
  load(paste0(my.path,"rangeHorizonsData/Andreas_data/species_data/TerrestrialRanges.rda"))
  
  # niche limits
  load(paste0(my.path,"rangeHorizonsData/Andreas_data/Overshoot/nicheLimits/1_",period,"_2100_0_AllTerrestrialSpecies_Tmax.rda"))
  
  tmax <- nicheLimitMat
  
  
  # climate data names | not loading climate variables yet to avoid memory issues
  TempTimeSeries <- list.files(paste0(my.path,"rangeHorizonsData/Andreas_data/Overshoot/tempMatrices"), pattern = "tempPostMatLand")
  TempTimeSeries <- grep(period, TempTimeSeries, value = T)
  
  # table with the list of species in each group
  group.table <- read.csv(paste0(my.path,"rangeHorizonsData/Andreas_data/species_data/Terrestrial_species.csv"), sep=';')
  
}


##############################################
### run analyses for each group
##############################################
# 


for(ii in 1:length(unique(group.table$group))){ 
  
  # get group names
  groups <- unique(group.table$group)   ###### REOPEN
  group <- groups[ii]                   ###### REOPEN
  
  # subset the group table based on the name of one group
  species.table <- group.table[group.table$group == group,] #  subset(group.table, group == group)
  
  # create list of species to perform the analyses
  spp.list <- unique(species.table$spp)
  
  ##############################################
  ### select an prepare a climate model
  ##############################################
  
  for(j in 1:length(TempTimeSeries)){ ###### loop over all climate models
    j = 1
    # for(j in c(2,3,5,6,8,9,12,13)){
    
    
    
    # load the focal climate model 
    climate.model <- TempTimeSeries[j]
    load(paste0(my.path,"rangeHorizonsData/Andreas_data/Overshoot/tempMatrices/",climate.model))
    
    # create a tMatrix from 2006 to 2100
    rownames(tempPostMat) <- my.grid$WorldID
    
    # change climate model name to meet tmin/tmax name
    climate.model <- gsub("tempPostMat","tempPreMat", climate.model)
    
    # remove from the climate model those cells with no data
    tMatrix <- remove_empty(tempPostMat, which = "rows")
    
    # list to store the results  
    rawResults <- list()
    
    # create the directory for the results
    folder <- str_sub(TempTimeSeries, end=-5)
    folder <- str_split(folder[j], pattern = "_")
    
    if(MarineOrTerrestrial=="Marine"){
      path <- paste0(my.path,"rangeHorizonsData/Andreas_data/Overshoot/rawResults/Marine/",group)
    } else {
      path <- paste0(my.path,"rangeHorizonsData/Andreas_data/Overshoot/rawResults/Terrestrial/",group) 
    }
    if(!file.exists(path)) dir.create(path, recursive = TRUE)
    
    
    ##############################################
    ### select a species
    ##############################################
    
    gc()
    
    for(k in 1:length(unique(species.table$spp))){
      # for(k in 1:5){
      
      # k <- sample(1:5299,1) # and 44
      
      # select focal species
      species <- spp.list[k]
      
      # print name
      print(paste0(species, " ", k, " out of ", length(unique(species.table$spp))))
      
      # get tmin and tmax
      speciesTmax <- tmax[species,climate.model] 
      

      #### Run analyses
      
      # 1. select cells from the historical range
      # cell numbers
      
      if(MarineOrTerrestrial=="Marine"){
        initial.range <- rangeCellsMarine[[species]]; length(initial.range)
      } else {
        initial.range <- rangeCellsTerrestrial[[species]]; length(initial.range)
        
      }
      initial.range <- intersect(initial.range, rownames(tMatrix))
      
      if(length(initial.range)==0) next
      
      tMatrixSpp <- tMatrix[rownames(tMatrix) %in% initial.range,]    
      
      
      if(class(tMatrixSpp)[1]=="numeric") {
        tMatrixSpp <- t(as.matrix(tMatrixSpp))
        rownames(tMatrixSpp) <- initial.range
      }
      
      if(nrow(tMatrixSpp)==0) next
      
      result <- tMatrixSpp
      
      for(i in 1:nrow(tMatrixSpp)){
        
        result[i, which(result[i,] <= speciesTmax)] <- 1
        result[i, which(result[i,] > speciesTmax)] <- 0
        
      }
      
      # result[,1:5]
      
      
      final <- cbind(rep(species, nrow(result)),rownames(result), result)
      final <- as.data.frame(final)
      
      if(period == "1850_2015") colnames(final) <- c("species","WorldID",2016:2299)
      if(period == "1850_2005") colnames(final) <- c("species","WorldID",2006:2299)
      
      rawResults[[k]] <- final
      
    }
    
    save(rawResults, file = paste0(path,"/",group,"_",folder[[1]][4],"_",period,".rda"),
         compress ="bzip2")
    
  }
}
