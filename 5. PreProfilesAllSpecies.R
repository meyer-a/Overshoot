# Merge results from different groups into a single file

library(scales)
library(rnaturalearth)
library(sp)
library(readr)
library(rlist)
library(Gmisc)
library(stringr)
library(parallel)

if(getwd()=="C:/Users/pc/Dropbox/Analyses/ColExt") {
  path <- "C:/Users/pc/Dropbox/rangeHorizonsData/Andreas_data/Horizons_model_output/"
  parallel <- "yes"
}

if(getwd()=="/research-home/ameyer") {
  path <- "../../nfs/geoengineering-data/rangeHorizonsData/Andreas_data/Overshoot/"
  parallel <- "no"
  
}


# 1. list climate models used in the analyses
files <- list.files(paste0(path, "preProfiles"), recursive = T, full.names = T)
files <- files[-grep("AllGroups", files)]

ecosystem <- c("Marine","Terrestrial")
models <- c("CanESM5", "CNRM-ESM2-1","GISS-E2-1-G","IPSL-CM6A-LR","MRI-ESM2-0")

for(i in seq_along(models)){
  
  model.tmp <- models[i]
  files.tmp <- grep(model.tmp, files, value = T)
  
  for(k in seq_along(ecosystem)){
    
    group.tmp <- grep(ecosystem[k], files.tmp, value = T)
    
    
  # 3. load data and transform into a big list
  results.tmp <- list()

  # get results list of all species and store as a single list
  for(j in 1:length(group.tmp)){
    load(group.tmp[j])
    results.tmp <- fastDoCall(c, list(results.tmp, preProfiles))
    }
  

  
ids <- unique(names(results.tmp))

# sum the number of species of all groups
preProfiles <- lapply(ids, function(x, dat){
  tmp <- dat[names(dat) == x]
  prof <- Reduce("+", tmp)
  prof[,"year"] <- prof["year"]/length(tmp)
  return(prof)
  
}, dat=results.tmp)


names(preProfiles) <- ids


climateModel <- str_split(files.tmp[1], pattern = "_")[[1]][3]
path2 <- paste0(path,"preProfiles/",ecosystem[k],"/AllGroups/")

if(!file.exists(path2)) dir.create(path2, recursive = TRUE)

save(preProfiles, file = paste0(path2,ecosystem[k],"Species_",climateModel,".rda"))

}

}


############################################################
