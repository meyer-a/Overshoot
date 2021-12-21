library(raster)
library(exactextractr)
library(dplyr)

# Load files
scenarios <- c("historical","ssp534","ssp585")
netcdfs <- list.files("D:/CMIP6/overshoot/overshoot_regridded", ".nc", recursive = T, full.names = T)
netcdfs <- grep("regriddata", netcdfs, value = T)
models <- dir("D:/CMIP6/overshoot/overshootpostproc/historical")
variables <- c("tos","tas")

# Grids
load("C:/Users/pc/Dropbox/rangeHorizonsData/HorizonsMaster/Horizons_nature_2020/Inputs/Raw_Data/Grid/BehrmannMeterGrid_WGS84_land.rda")
land.grid <- griddedDomain


load("C:/Users/pc/Dropbox/rangeHorizonsData/Andreas_data/Grid/OceanGrid_without_central_america.rda")
ocean.grid <- OceanGrid

rm("griddedDomain"); rm("OceanGrid")

# CanESM5 overshoot: 2040-2300
# CNRM-ESM2-1 overshoot: 2015-2300
# GISS-E2-1-G overshoot: 2040-2300
# IPSL-CM6A-LR overshoot: 2040-300
# MRI-ESM2-0: 2040-2300



tempMatOcean <- matrix(NA, nrow=nrow(ocean.grid), ncol=length(1850:2300))
tempMatLand  <- matrix(NA, nrow=nrow(land.grid), ncol=length(1850:2300))

for(h in seq_along(variables)){
  variables.tmp <- grep(variables[h], netcdfs, value = T)

for(i in seq_along(models)){
  # for(i in 3:5){
  if (variables[h] == "tos")tempMatOcean <- matrix(NA, nrow=nrow(ocean.grid), ncol=length(1850:2300))
  if(variables[h] == "tas") tempMatLand  <- matrix(NA, nrow=nrow(land.grid), ncol=length(1850:2300))
  
  models.tmp <- grep(models[i], variables.tmp, value = T)
  
for(j in seq_along(models.tmp)){
  
  r <- brick(models.tmp[j])
  
  if(grepl("historical",models.tmp[j])==T) periods <- seq(from = 1, to = dim(r)[3]+1, by = 12)
  
  if(grepl("ssp585",models.tmp[j])==T) periods <- seq(from = 1, to = 300 +1, by = 12)
    
  if(grepl("ssp534",models.tmp[j])==T){
    
    periods <- seq(from = ifelse(models[i]=="CNRM-ESM2-1",301,1), to = dim(r)[3]+1, by = 12)
  }
  
for(k in seq_along(periods)){
  print(k)
  
  # stop loop in the last year of the time series
  
  if(k==length(periods)) break
  
  r.tmp <- r[[periods[k]:(periods[k+1]-1)]]
  r.tmp <- raster::rotate(r.tmp)
  
  if (variables[h] == "tas") {
    result <- exact_extract(r.tmp, land.grid, fun = "mean")
    result.mean <- rowMeans(result, na.rm = T)
    
    if (grepl("historical", models.tmp[j]) == T) tempMatLand[, k] <- result.mean
    if (grepl("ssp585", models.tmp[j]) == T)     tempMatLand[, k + 165] <- result.mean
    if (grepl("ssp534", models.tmp[j]) == T) tempMatLand[, k + 190] <- result.mean
  }
  
  
  if (variables[h] == "tos") {
    result <- exact_extract(r.tmp, ocean.grid, fun = "mean") 
    result.mean <- rowMeans(result, na.rm = T)
      
    if (grepl("historical", models.tmp[j]) == T) tempMatOcean[, k] <- result.mean
    if (grepl("ssp585", models.tmp[j]) == T)     tempMatOcean[, k + 165] <- result.mean
    if (grepl("ssp534", models.tmp[j]) == T) tempMatOcean[, k + 190] <- result.mean
      
    }
}
}
  
  if (variables[h] == "tas") {
    save(tempMatLand, file = paste0("C:/Users/pc/Dropbox/rangeHorizonsData/Andreas_data/Overshoot/tempMatrices/tempMatLand_",models[i],".rda"))
  }
  
  if (variables[h] == "tos") {
    save(tempMatOcean, file = paste0("C:/Users/pc/Dropbox/rangeHorizonsData/Andreas_data/Overshoot/tempMatrices/tempMatOcean_",models[i],".rda"))
  }
  
}
}

