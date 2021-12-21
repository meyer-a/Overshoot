## Create temperature matrix to calculate niche limits and climate horizon profiles 


library(stringr)

if(getwd()=="C:/Users/pc/Dropbox/Analyses/Overshoot") {
  my.path <- "C:/Users/pc/Dropbox/"
}

if(getwd()=="/nfs/geoengineering-data") {
  my.path <- "/nfs/geoengineering-data/"
}



files <- list.files(paste0(my.path,"rangeHorizonsData/Andreas_data/Overshoot/tempMatrices"),".rda", full.names = T)
file.names <- list.files(paste0(my.path,"rangeHorizonsData/Andreas_data/Overshoot/tempMatrices"),".rda")


for(i in seq_along(files)){
  
  if(grepl("Ocean", files[i])==T){
    load(files[i])
    tempPreMat <- tempMatOcean[,1:165]
    tempPostMat <- tempMatOcean[,166:450]
    
    n <- str_split(file.names[i], pattern="_")
    
    save(tempPreMat, file=paste0(my.path, "rangeHorizonsData/Andreas_data/Overshoot/tempMatrices/tempPreMatOcean_1850_2015_",n[[1]][2]))
    save(tempPostMat, file=paste0(my.path, "rangeHorizonsData/Andreas_data/Overshoot/tempMatrices/tempPostMatOcean_1850_2015_",n[[1]][2]))
    
  }
}

for(i in seq_along(files)){
  
  if(grepl("Land", files[i])==T){
    load(files[i])
    tempPreMat <- tempMatLand[,1:165]
    tempPostMat <- tempMatLand[,166:450]
    
    n <- str_split(file.names[i], pattern="_")
    
    save(tempPreMat, file=paste0(my.path, "rangeHorizonsData/Andreas_data/Overshoot/tempMatrices/tempPreMatLand_1850_2015_",n[[1]][2]))
    save(tempPostMat, file=paste0(my.path, "rangeHorizonsData/Andreas_data/Overshoot/tempMatrices/tempPostMatLand_1850_2015_",n[[1]][2]))
  }
  
}

