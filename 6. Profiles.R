library(dplyr)
library(rnaturalearth)
library(abind)
library(Gmisc)

if(getwd()=="/research-home/ameyer") {
  path <- "../../nfs/geoengineering-data/rangeHorizonsData/Andreas_data/"
}



files <- list.files(paste0(path,"Overshoot/preProfiles"), pattern = "rda", rec =T, full.names = T)
groups <- c("MarineSpecies","BenthicInverts","Cephalopods","CoralSeagrass","Fish","Krill",
            "MarineMammals","MarineReptiles","TerrestrialSpecies","Amphibian","Bird",
            "Mammal","Reptile")
groups <- c("MarineSpecies","TerrestrialSpecies")

models <- c("CanESM5", "CNRM-ESM2-1","GISS-E2-1-G","IPSL-CM6A-LR","MRI-ESM2-0")


for(i in seq_along(groups)){
  
  files.tmp <- grep(groups[i], files, value = T)
  
 
    load(files.tmp[1])
    can <- preProfiles
    load(files.tmp[2])
    cnrm <- preProfiles
    load(files.tmp[3])
    giss <- preProfiles
    load(files.tmp[4])
    ipsl <- preProfiles
    load(files.tmp[5])
    mri <- preProfiles
    
    ids <- unique(c(names(can),names(cnrm),names(giss),names(ipsl),names(mri)))
    
    profiles <- list()
    
    for(k in seq_along(ids)){
      
      # k=39705
      # k=sample(1:40000,1)
      can.tmp <- can[names(can)==ids[k]]
      cnrm.tmp <- cnrm[names(cnrm)==ids[k]]
      giss.tmp <- giss[names(giss)==ids[k]]
      ipsl.tmp <- ipsl[names(ipsl)==ids[k]]
      mri.tmp <- mri[names(mri)==ids[k]]
      
      a <- do.call(abind, c(can.tmp,cnrm.tmp,giss.tmp,ipsl.tmp,mri.tmp, list(along=3)))
      
      profiles[[k]] <- apply(a, c(1,2), median)
      

    }
    
    names(profiles) <- ids
    
    ecosystem <- ifelse(groups[i] == "TerrestrialSpecies" | 
                        groups[i] == "Amphibian" |
                        groups[i] == "Bird"|
                        groups[i] == "Mammal"|
                        groups[i] == "Reptile", "Terrestrial","Marine")
    
    save(profiles, file=paste0(path,"Overshoot/profiles/",ecosystem,"/",groups[i],".rda"))
  
}
