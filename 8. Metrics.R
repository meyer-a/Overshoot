library(dplyr)
library(zoo)


if(getwd()=="/research-home/ameyer") {
  path <- "../../nfs/geoengineering-data/rangeHorizonsData/Andreas_data/"
} else {
  
  path <- "C:/Users/pc/Dropbox/rangeHorizonsData/Andreas_data/"
}



########################################################## 
### get magnitude, abruptness and timing
##########################################################


# 1. list climate models used in the analyses

files <- list.files(paste0(path, "Overshoot/preProfiles"),rec = T, full.names = T)
files <- files[grep("AllGroups",files)]
files <- files[grep("Marine",files)]

models <- c("CanESM5", "CNRM-ESM2-1","GISS-E2-1-G","IPSL-CM6A-LR","MRI-ESM2-0")


for(j in 1:5){
load(files[j])

ids <- as.integer(names(preProfiles))



#################################
##########   METRICS   ##########
#################################

# 1.	max.exp.2100 : Max exposure before 2100
# 2.	max.exp.over : Max exposure overall
# 3.	final.exp    : Final exposure at 2300
# 4.  dexp         : Total proportion of species de-exposed 
# 5.	timing.exp.2100 : Median timing of exposure before 2100
# 6.	timing.exp.over : Median timing of exposure overall
# 7.	timing.dexp.2100 : Median timing of de-exposure after 2100
# 8.	timing.dexp.over : Median timing de-exposure overall
# 9.	diff.timing.2100 : Difference in timing 2100 (7 – 5)
# 10.	diff.timing.over : Difference in timing overall (8 – 6)
# 11	abrupt.exp.2100 : Abruptness exposure 2100
# 12	abrupt.exp.over : Abruptness exposure overall
# 13	abrupt.dexp.2100 : Abruptness de-exposure after 2100
# 14	abrupt.dexp.over : Abruptness de-exposure overall
# 15	time.2c : Difference in time between exposure at time of start of 2 °C global overshoot and time of getting back to that exposure level

# all metrics in percentage. absolute values tagged as .abs



metrics <- data.frame(WorldID = NA, max.exp.2100 = NA, max.exp.over = NA, final.exp = NA, dexp = NA, 
                      timing.exp.2100 = NA, timing.exp.over = NA, timing.dexp.2100 = NA, timing.dexp.over = NA,
                      diff.timing.2100 = NA, diff.timing.over = NA, 
                      abrupt.exp.2100 = NA, abrupt.exp.over = NA, abrupt.dexp.2100 = NA, abrupt.dexp.over = NA,
                      exp.begin.2c = NA, exp.end.2c = NA, time.2c.year = NA, time.2c.duration = NA,
                      pool = NA,
                      max.exp.2100.abs = NA, max.exp.over.abs = NA, final.exp.abs = NA, dexp.abs = NA,
                      abrupt.exp.2100.abs = NA, abrupt.exp.over.abs = NA, abrupt.dexp.2100.abs = NA, abrupt.dexp.over.abs = NA)

for(i in 1:length(ids)){
  
  print(i)
  k <- ids[i]
  # k <- sample(ids,1)
  # k <- 17599
  # i=1
  # 
  my.profile.over <- preProfiles[names(preProfiles)==k][[1]]
  if(my.profile.over[1,4] <= 4){
    
    metrics[i,1] <- ids[i]
    
    max.exp.2100.abs <- max(my.profile.2100[,5])
    max.exp.over.abs <- max(my.profile.over[,5])
    final.exp.abs    <- last(my.profile.over[,5])
    dexp.abs <- (max(my.profile.over[,5]) - last(my.profile.over[,5]))
    
    
    metrics[i,2] <- max.exp.2100.abs/my.profile.2100[1,4]*100
    metrics[i,3] <- max.exp.over.abs/my.profile.over[1,4]*100
    metrics[i,4] <- final.exp.abs/my.profile.over[1,4]*100
    metrics[i,5] <- dexp.abs*100/max(my.profile.over[,5])
    
    metrics[i,20] <- max.exp.2100.abs
    metrics[i,21] <- max.exp.over.abs
    metrics[i,22] <- final.exp.abs
    metrics[i,23] <- dexp.abs
    
    metrics[i,6:19] <- NA
    metrics[i,24:ncol(metrics)] <- NA
    next
    
  }
  
  
  my.profile.2100 <- my.profile.over[1:85,]
  my.profile.after.2100 <- my.profile.over[86:285,]
  
  
  # plot(my.profile.over[,5]/my.profile.over[1,4]*100, type="l", main=k, bty="l", ylab="Species Exposed", xaxt="n", xlim=c(-14,300), lwd=2)
  # axis(1, at= seq(-14,300,50), labels=2006+seq(-6,300,50))
  
  # magnitude
  metrics[i,1] <- ids[i]
  
  max.exp.2100.abs <- max(my.profile.2100[,5])
  max.exp.over.abs <- max(my.profile.over[,5])
  final.exp.abs    <- last(my.profile.over[,5])
  dexp.abs <- (max(my.profile.over[,5]) - last(my.profile.over[,5]))
  
  metrics[i,2] <- max.exp.2100.abs/my.profile.2100[1,4]*100
  metrics[i,3] <- max.exp.over.abs/my.profile.over[1,4]*100
  metrics[i,4] <- final.exp.abs/my.profile.over[1,4]*100
  metrics[i,5] <- dexp.abs*100/max(my.profile.over[,5])
  
  metrics[i,21] <- max.exp.2100.abs
  metrics[i,22] <- max.exp.over.abs
  metrics[i,23] <- final.exp.abs
  metrics[i,24] <- dexp.abs
  
  # timing 
  # exposure before 2100
  tmp <- rollapply(my.profile.2100[,5], 2, diff)
  tmp <- as.data.frame(cbind(prof=my.profile.2100[,5], tmp=c(0, tmp),year=c(2016:2100)))
  tmp <- tmp %>% filter(tmp >= 0)
  timing.exp.2100 <- round(median(rep(tmp$year,tmp$tmp)),0)
  
  # exposure overall
  tmp <- rollapply(my.profile.over[,5], 2, diff)
  tmp <- as.data.frame(cbind(prof=my.profile.over[,5], tmp=c(0, tmp),year=c(2016:2300)))
  tmp <- tmp %>% filter(tmp >= 0)
  timing.exp.over <- round(median(rep(tmp$year,tmp$tmp)),0)
  
  # de-exposure after 2100
  tmp <- rollapply(my.profile.after.2100[,5], 2, diff)
  tmp <- as.data.frame(cbind(prof=my.profile.after.2100[,5], tmp=c(0, tmp),year=c(2101:2300)))
  tmp <- tmp %>% filter(tmp <= 0) 
  tmp$tmp <- tmp$tmp * -1
  timing.dexp.2100 <- round(median(rep(tmp$year,tmp$tmp)),0)
  
  # de-exposure overall
  tmp <- rollapply(my.profile.over[,5], 2, diff)
  tmp <- as.data.frame(cbind(prof=my.profile.over[,5], tmp=c(0, tmp),year=c(2016:2300)))
  tmp <- tmp %>% filter(tmp <= 0) 
  tmp$tmp <- tmp$tmp * -1
  timing.dexp.over <- round(median(rep(tmp$year,tmp$tmp)),0)
  
  metrics[i,6] <- timing.exp.2100
  metrics[i,7] <- timing.exp.over
  metrics[i,8] <- timing.dexp.2100
  metrics[i,9] <- timing.dexp.over
  
  # abline(v = timing.exp.2100-2015, col="red", lwd=2)
  # abline(v = timing.exp.over-2015, col="blue", lwd=2, lty=2)
  # abline(v = timing.dexp.2100-2015, col="forestgreen", lwd=2)
  # abline(v = timing.dexp.over-2015, col="purple", lwd=2, lty=2)
  
  # Difference in timing 2100 
  metrics[i,10] <- timing.dexp.2100 - timing.exp.2100
  metrics[i,11] <- timing.dexp.over - timing.exp.over
  
  # abruptness
  # Abruptness exposure before 2100
  tmp <- rollapply(my.profile.2100[,5], 2, diff)
  tmp <- as.data.frame(cbind(my.profile.2100[,5], tmp=c(0, tmp),c(2016:2100)))
  tmp <- tmp %>% filter(tmp >= 0)
  abr <- rollapply(tmp[,2], 10, sum)
  tmp <- cbind(tmp, abr=c(rep(NA,9), abr))
  abrupt.exp.2100.abs <- ceiling(max(tmp[,4], na.rm = T))
  abrupt.exp.2100 <- (abrupt.exp.2100.abs/(max(my.profile.over[,5]))*100)
  
  # Abruptness exposure overall
  tmp <- rollapply(my.profile.over[,5], 2, diff)
  tmp <- as.data.frame(cbind(my.profile.over[,5], tmp=c(0, tmp),c(2016:2300)))
  tmp <- tmp %>% filter(tmp >= 0)
  abr <- rollapply(tmp[,2], 10, sum)
  tmp <- cbind(tmp, abr=c(rep(NA,9), abr))
  abrupt.exp.over.abs <- ceiling(max(tmp[,4], na.rm = T))
  abrupt.exp.over <- (abrupt.exp.over.abs/(max(my.profile.over[,5]))*100)
  
  # Abruptness de-exposure after 2100
  tmp <- rollapply(my.profile.after.2100[,5], 2, diff)
  tmp <- as.data.frame(cbind(my.profile.after.2100[,5], tmp=c(0, tmp),c(2101:2300)))
  tmp <- tmp %>% filter(tmp <= 0)
  tmp$tmp <- tmp$tmp * -1
  abr <- rollapply(tmp[,2], 10, sum)
  tmp <- cbind(tmp, abr=c(rep(NA,9), abr))
  abrupt.dexp.2100.abs <- ceiling(max(tmp[,4], na.rm = T))
  # abrupt.dexp.2100 <- abrupt.dexp.2100.abs*100/(dexp.abs)
  abrupt.dexp.2100 <- (abrupt.dexp.2100.abs/(max(my.profile.over[,5]))*100)
  
  # Abruptness de-exposure overall
  tmp <- rollapply(my.profile.over[,5], 2, diff)
  tmp <- as.data.frame(cbind(my.profile.over[,5], tmp=c(0, tmp),c(2016:2300)))
  tmp <- tmp %>% filter(tmp <= 0)
  tmp$tmp <- tmp$tmp * -1
  abr <- rollapply(tmp[,2], 10, sum)
  tmp <- cbind(tmp, abr=c(rep(NA,9), abr))
  abrupt.dexp.over.abs <- ceiling(max(tmp[,4], na.rm = T))
  abrupt.dexp.over <- (abrupt.dexp.over.abs/(max(my.profile.over[,5]))*100)
  
  metrics[i,12] <- abrupt.exp.2100
  metrics[i,13] <- abrupt.exp.over
  metrics[i,14] <- abrupt.dexp.2100
  metrics[i,15] <- abrupt.dexp.over
  
  metrics[i,25] <- abrupt.exp.2100.abs
  metrics[i,26] <- abrupt.exp.over.abs
  metrics[i,27] <- abrupt.dexp.2100.abs
  metrics[i,28] <- abrupt.dexp.over.abs
  
  
  # Difference in time between exposure at time of start of 2 °C global overshoot and time of getting back to that exposure level
  # load climate data
  
  
  if(j==1) {begin.overshoot <- 2023; end.overshoot <- NA
  
  metrics[i,16:20] <- NA 
  
  } else {
    
    if(j==2) {begin.overshoot <- 2047; end.overshoot <- 2152}
    if(j==3) {begin.overshoot <- 2041; end.overshoot <- 2092}
    if(j==4) {begin.overshoot <- 2036; end.overshoot <- 2233}
    if(j==5) {begin.overshoot <- 2038; end.overshoot <- 2094}
    
    exp.begin.2c.abs <- my.profile.over[my.profile.over[,1] == begin.overshoot,5]
    exp.end.2c.abs   <- my.profile.over[my.profile.over[,1] == end.overshoot,5]
    
    my.profile.tmp <- my.profile.over[my.profile.over[,1] > begin.overshoot,,drop=F]
    tmp <- rle(my.profile.tmp[,5])
    tmp <- data.frame(values=tmp$values, lengths=tmp$lengths)
    tmp$time <- cumsum(tmp$lengths)
    tmp$year <- tmp$time + begin.overshoot
    if(tmp$values[1] == exp.begin.2c.abs){
      begin.change <- tmp$year[1]
      tmp2 <- tmp[-1,]
      tmp <- tmp2[which(tmp2[,1] == exp.begin.2c.abs),,drop=F]
    } else {
      begin.change <- begin.overshoot
      tmp2 <- tmp
      tmp <- tmp2[which(tmp2[,1] == exp.begin.2c.abs),,drop=F]
    }
    
    if(is.na(tmp[1,2])){
      
      time.2c.year <- 2301
      time.2c.duration <- time.2c.year - begin.change - (end.overshoot - begin.overshoot)
      
    } else {
      
      time.2c.year <- tmp[1,4] - tmp[1,2] + 1
      time.2c.duration <- time.2c.year - begin.change - (end.overshoot - begin.overshoot)
    }
    
    metrics[i,16] <- exp.begin.2c.abs
    metrics[i,17] <- exp.end.2c.abs[1]
    metrics[i,18] <- time.2c.year
    metrics[i,19] <- time.2c.duration
    metrics[i,20] <- my.profile.over[1,4]
    
    metrics
  }
}


# Save

save(metrics, file = paste0(path,"Overshoot/mapData/MarineSpecies_v4_",models[j],".rda"))

}



#################################
########## TERRESTRIAL  ###########
#################################

files <- list.files(paste0(path, "Overshoot/preProfiles"),rec = T, full.names = T)
files <- files[grep("AllGroups",files)]
files <- files[grep("Terrestrial",files)]

for(j in 1:5){
  load(files[j])
  
  ids <- as.integer(names(preProfiles))
  
  
  
  #################################
  ##########   METRICS   ##########
  #################################
  
  # 1.	max.exp.2100 : Max exposure before 2100
  # 2.	max.exp.over : Max exposure overall
  # 3.	final.exp    : Final exposure at 2300
  # 4.  dexp         : Total proportion of species de-exposed 
  # 5.	timing.exp.2100 : Median timing of exposure before 2100
  # 6.	timing.exp.over : Median timing of exposure overall
  # 7.	timing.dexp.2100 : Median timing of de-exposure after 2100
  # 8.	timing.dexp.over : Median timing de-exposure overall
  # 9.	diff.timing.2100 : Difference in timing 2100 (7 – 5)
  # 10.	diff.timing.over : Difference in timing overall (8 – 6)
  # 11	abrupt.exp.2100 : Abruptness exposure 2100
  # 12	abrupt.exp.over : Abruptness exposure overall
  # 13	abrupt.dexp.2100 : Abruptness de-exposure after 2100
  # 14	abrupt.dexp.over : Abruptness de-exposure overall
  # 15	time.2c : Difference in time between exposure at time of start of 2 °C global overshoot and time of getting back to that exposure level
  
  # all metrics in percentage. absolute values tagged as .abs
  
  
  metrics <- data.frame(WorldID = NA, max.exp.2100 = NA, max.exp.over = NA, final.exp = NA, dexp = NA, 
                        timing.exp.2100 = NA, timing.exp.over = NA, timing.dexp.2100 = NA, timing.dexp.over = NA,
                        diff.timing.2100 = NA, diff.timing.over = NA, 
                        abrupt.exp.2100 = NA, abrupt.exp.over = NA, abrupt.dexp.2100 = NA, abrupt.dexp.over = NA,
                        exp.begin.2c = NA, exp.end.2c = NA, time.2c.year = NA, time.2c.duration = NA,
                        pool = NA,
                        max.exp.2100.abs = NA, max.exp.over.abs = NA, final.exp.abs = NA, dexp.abs = NA,
                        abrupt.exp.2100.abs = NA, abrupt.exp.over.abs = NA, abrupt.dexp.2100.abs = NA, abrupt.dexp.over.abs = NA)
  
  for(i in 1:length(ids)){
    
    print(i)
    k <- ids[i]
    # k <- sample(ids,1)
    # k <- 17599
    # i=1
    # 
    my.profile.over <- preProfiles[names(preProfiles)==k][[1]]
    if(my.profile.over[1,4] <= 4){
      
      metrics[i,1] <- ids[i]
      
      max.exp.2100.abs <- max(my.profile.2100[,5])
      max.exp.over.abs <- max(my.profile.over[,5])
      final.exp.abs    <- last(my.profile.over[,5])
      dexp.abs <- (max(my.profile.over[,5]) - last(my.profile.over[,5]))
      
      
      metrics[i,2] <- max.exp.2100.abs/my.profile.2100[1,4]*100
      metrics[i,3] <- max.exp.over.abs/my.profile.over[1,4]*100
      metrics[i,4] <- final.exp.abs/my.profile.over[1,4]*100
      metrics[i,5] <- dexp.abs*100/max(my.profile.over[,5])
      
      metrics[i,20] <- max.exp.2100.abs
      metrics[i,21] <- max.exp.over.abs
      metrics[i,22] <- final.exp.abs
      metrics[i,23] <- dexp.abs
      
      metrics[i,6:19] <- NA
      metrics[i,24:ncol(metrics)] <- NA
      next
      
    }
    
    
    my.profile.2100 <- my.profile.over[1:85,]
    my.profile.after.2100 <- my.profile.over[86:285,]
    
    
    # plot(my.profile.over[,5]/my.profile.over[1,4]*100, type="l", main=k, bty="l", ylab="Species Exposed", xaxt="n", xlim=c(-14,300), lwd=2)
    # axis(1, at= seq(-14,300,50), labels=2006+seq(-6,300,50))
    
    # magnitude
    metrics[i,1] <- ids[i]
    
    max.exp.2100.abs <- max(my.profile.2100[,5])
    max.exp.over.abs <- max(my.profile.over[,5])
    final.exp.abs    <- last(my.profile.over[,5])
    dexp.abs <- (max(my.profile.over[,5]) - last(my.profile.over[,5]))
    
    metrics[i,2] <- max.exp.2100.abs/my.profile.2100[1,4]*100
    metrics[i,3] <- max.exp.over.abs/my.profile.over[1,4]*100
    metrics[i,4] <- final.exp.abs/my.profile.over[1,4]*100
    metrics[i,5] <- dexp.abs*100/max(my.profile.over[,5])
    
    metrics[i,21] <- max.exp.2100.abs
    metrics[i,22] <- max.exp.over.abs
    metrics[i,23] <- final.exp.abs
    metrics[i,24] <- dexp.abs
    
    # timing 
    # exposure before 2100
    tmp <- rollapply(my.profile.2100[,5], 2, diff)
    tmp <- as.data.frame(cbind(prof=my.profile.2100[,5], tmp=c(0, tmp),year=c(2016:2100)))
    tmp <- tmp %>% filter(tmp >= 0)
    timing.exp.2100 <- round(median(rep(tmp$year,tmp$tmp)),0)
    
    # exposure overall
    tmp <- rollapply(my.profile.over[,5], 2, diff)
    tmp <- as.data.frame(cbind(prof=my.profile.over[,5], tmp=c(0, tmp),year=c(2016:2300)))
    tmp <- tmp %>% filter(tmp >= 0)
    timing.exp.over <- round(median(rep(tmp$year,tmp$tmp)),0)
    
    # de-exposure after 2100
    tmp <- rollapply(my.profile.after.2100[,5], 2, diff)
    tmp <- as.data.frame(cbind(prof=my.profile.after.2100[,5], tmp=c(0, tmp),year=c(2101:2300)))
    tmp <- tmp %>% filter(tmp <= 0) 
    tmp$tmp <- tmp$tmp * -1
    timing.dexp.2100 <- round(median(rep(tmp$year,tmp$tmp)),0)
    
    # de-exposure overall
    tmp <- rollapply(my.profile.over[,5], 2, diff)
    tmp <- as.data.frame(cbind(prof=my.profile.over[,5], tmp=c(0, tmp),year=c(2016:2300)))
    tmp <- tmp %>% filter(tmp <= 0) 
    tmp$tmp <- tmp$tmp * -1
    timing.dexp.over <- round(median(rep(tmp$year,tmp$tmp)),0)
    
    metrics[i,6] <- timing.exp.2100
    metrics[i,7] <- timing.exp.over
    metrics[i,8] <- timing.dexp.2100
    metrics[i,9] <- timing.dexp.over
    
    # abline(v = timing.exp.2100-2015, col="red", lwd=2)
    # abline(v = timing.exp.over-2015, col="blue", lwd=2, lty=2)
    # abline(v = timing.dexp.2100-2015, col="forestgreen", lwd=2)
    # abline(v = timing.dexp.over-2015, col="purple", lwd=2, lty=2)

    # Difference in timing 2100 
    metrics[i,10] <- timing.dexp.2100 - timing.exp.2100
    metrics[i,11] <- timing.dexp.over - timing.exp.over
    
    # abruptness
    # Abruptness exposure before 2100
    tmp <- rollapply(my.profile.2100[,5], 2, diff)
    tmp <- as.data.frame(cbind(my.profile.2100[,5], tmp=c(0, tmp),c(2016:2100)))
    tmp <- tmp %>% filter(tmp >= 0)
    abr <- rollapply(tmp[,2], 10, sum)
    tmp <- cbind(tmp, abr=c(rep(NA,9), abr))
    abrupt.exp.2100.abs <- ceiling(max(tmp[,4], na.rm = T))
    abrupt.exp.2100 <- (abrupt.exp.2100.abs/(max(my.profile.over[,5]))*100)
    
    # Abruptness exposure overall
    tmp <- rollapply(my.profile.over[,5], 2, diff)
    tmp <- as.data.frame(cbind(my.profile.over[,5], tmp=c(0, tmp),c(2016:2300)))
    tmp <- tmp %>% filter(tmp >= 0)
    abr <- rollapply(tmp[,2], 10, sum)
    tmp <- cbind(tmp, abr=c(rep(NA,9), abr))
    abrupt.exp.over.abs <- ceiling(max(tmp[,4], na.rm = T))
    abrupt.exp.over <- (abrupt.exp.over.abs/(max(my.profile.over[,5]))*100)
    
    # Abruptness de-exposure after 2100
    tmp <- rollapply(my.profile.after.2100[,5], 2, diff)
    tmp <- as.data.frame(cbind(my.profile.after.2100[,5], tmp=c(0, tmp),c(2101:2300)))
    tmp <- tmp %>% filter(tmp <= 0)
    tmp$tmp <- tmp$tmp * -1
    abr <- rollapply(tmp[,2], 10, sum)
    tmp <- cbind(tmp, abr=c(rep(NA,9), abr))
    abrupt.dexp.2100.abs <- ceiling(max(tmp[,4], na.rm = T))
    # abrupt.dexp.2100 <- abrupt.dexp.2100.abs*100/(dexp.abs)
    abrupt.dexp.2100 <- (abrupt.dexp.2100.abs/(max(my.profile.over[,5]))*100)
    
    # Abruptness de-exposure overall
    tmp <- rollapply(my.profile.over[,5], 2, diff)
    tmp <- as.data.frame(cbind(my.profile.over[,5], tmp=c(0, tmp),c(2016:2300)))
    tmp <- tmp %>% filter(tmp <= 0)
    tmp$tmp <- tmp$tmp * -1
    abr <- rollapply(tmp[,2], 10, sum)
    tmp <- cbind(tmp, abr=c(rep(NA,9), abr))
    abrupt.dexp.over.abs <- ceiling(max(tmp[,4], na.rm = T))
    abrupt.dexp.over <- (abrupt.dexp.over.abs/(max(my.profile.over[,5]))*100)
    
    metrics[i,12] <- abrupt.exp.2100
    metrics[i,13] <- abrupt.exp.over
    metrics[i,14] <- abrupt.dexp.2100
    metrics[i,15] <- abrupt.dexp.over
    
    metrics[i,25] <- abrupt.exp.2100.abs
    metrics[i,26] <- abrupt.exp.over.abs
    metrics[i,27] <- abrupt.dexp.2100.abs
    metrics[i,28] <- abrupt.dexp.over.abs
    
    
    # Difference in time between exposure at time of start of 2 °C global overshoot and time of getting back to that exposure level
    # load climate data
    
    
    if(j==1) {begin.overshoot <- 2023; end.overshoot <- NA
    
      metrics[i,16:20] <- NA 
      
    } else {
      
    if(j==2) {begin.overshoot <- 2047; end.overshoot <- 2152}
    if(j==3) {begin.overshoot <- 2041; end.overshoot <- 2092}
    if(j==4) {begin.overshoot <- 2036; end.overshoot <- 2233}
    if(j==5) {begin.overshoot <- 2038; end.overshoot <- 2094}
    
    exp.begin.2c.abs <- my.profile.over[my.profile.over[,1] == begin.overshoot,5]
    exp.end.2c.abs   <- my.profile.over[my.profile.over[,1] == end.overshoot,5]
    
    my.profile.tmp <- my.profile.over[my.profile.over[,1] > begin.overshoot,,drop=F]
    tmp <- rle(my.profile.tmp[,5])
    tmp <- data.frame(values=tmp$values, lengths=tmp$lengths)
    tmp$time <- cumsum(tmp$lengths)
    tmp$year <- tmp$time + begin.overshoot
    if(tmp$values[1] == exp.begin.2c.abs){
      begin.change <- tmp$year[1]
      tmp2 <- tmp[-1,]
      tmp <- tmp2[which(tmp2[,1] == exp.begin.2c.abs),,drop=F]
    } else {
      begin.change <- begin.overshoot
      tmp2 <- tmp
      tmp <- tmp2[which(tmp2[,1] == exp.begin.2c.abs),,drop=F]
    }
    
    if(is.na(tmp[1,2])){
      
      time.2c.year <- 2301
      time.2c.duration <- time.2c.year - begin.change - (end.overshoot - begin.overshoot)
      
    } else {
      
    time.2c.year <- tmp[1,4] - tmp[1,2] + 1
    time.2c.duration <- time.2c.year - begin.change - (end.overshoot - begin.overshoot)
    }
    
    metrics[i,16] <- exp.begin.2c.abs
    metrics[i,17] <- exp.end.2c.abs[1]
    metrics[i,18] <- time.2c.year
    metrics[i,19] <- time.2c.duration
    metrics[i,20] <- my.profile.over[1,4]
    
    metrics
    }
  }
  
  # Save
  
  save(metrics, file = paste0(path,"Overshoot/mapData/TerrestrialSpecies_v4_",models[j],".rda"))
  
}


