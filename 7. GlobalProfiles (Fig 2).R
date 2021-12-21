library(dplyr)
library(scales)
library(abind)
library(zoo)
library(viridis)
library(Gmisc)
library(parallel)
library(rnaturalearth)
library(sf)
library(rgdal)

rm(list=ls())
gc()

if(getwd()=="/nfs/geoengineering-data") {
   path <- "rangeHorizonsData/Andreas_data/"
} else {
   
   path <- "C:/Users/pc/Dropbox/rangeHorizonsData/Andreas_data/"
}

################################################################
# prepare data to plot global profiles
################################################################


files <- list.files(paste0(path,"Overshoot/preProfiles"), recursive = T, full.names = T)
marine <- grep(pattern = "Marine/AllGroups", files, value = T)
terrestrial <- grep(pattern = "Terrestrial/AllGroups", files, value = T)


marine.res <- list()

for(i in seq_along(marine)){
   
   load(marine[i])
   marine.res[[i]] <- Reduce("+", preProfiles)
   
}


terrestrial.res <- list()

for(i in seq_along(terrestrial)){
   
   load(terrestrial[i])
   terrestrial.res[[i]] <- Reduce("+", preProfiles)
   
}


t1 <- (terrestrial.res[[1]]$n.species.exp - terrestrial.res[[1]]$n.species.sui)/terrestrial.res[[1]]$pool[1]*100
t2 <- (terrestrial.res[[2]]$n.species.exp - terrestrial.res[[2]]$n.species.sui)/terrestrial.res[[2]]$pool[1]*100
t3 <- (terrestrial.res[[3]]$n.species.exp - terrestrial.res[[3]]$n.species.sui)/terrestrial.res[[3]]$pool[1]*100
t4 <- (terrestrial.res[[4]]$n.species.exp - terrestrial.res[[4]]$n.species.sui)/terrestrial.res[[4]]$pool[1]*100
t5 <- (terrestrial.res[[5]]$n.species.exp - terrestrial.res[[5]]$n.species.sui)/terrestrial.res[[5]]$pool[1]*100

m1 <- (marine.res[[1]]$n.species.exp - marine.res[[1]]$n.species.sui)/marine.res[[1]]$pool[1]*100
m2 <- (marine.res[[2]]$n.species.exp - marine.res[[2]]$n.species.sui)/marine.res[[2]]$pool[1]*100
m3 <- (marine.res[[3]]$n.species.exp - marine.res[[3]]$n.species.sui)/marine.res[[3]]$pool[1]*100
m4 <- (marine.res[[4]]$n.species.exp - marine.res[[4]]$n.species.sui)/marine.res[[4]]$pool[1]*100
m5 <- (marine.res[[5]]$n.species.exp - marine.res[[5]]$n.species.sui)/marine.res[[5]]$pool[1]*100

tdf <- data.frame(t1,t2,t3,t4,t5)
mdf <- data.frame(m1,m2,m3,m4,m5)

tmedian <- apply(tdf, 1, median)
mmedian <- apply(mdf, 1, median)

names(tmedian) <- names(mmedian) <- 2015:2299

begin.overshoot <- 2041; end.overshoot <- 2102

exp.begin.t <- tmedian[names(tmedian) == begin.overshoot]
exp.begin.m <- mmedian[names(mmedian) == begin.overshoot]

which(tmedian[28:285] <= exp.begin.t)
which(mmedian[28:285] <= exp.begin.m)

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



t1 <- terrestrial.res[[1]]$div.profile 
t2 <- terrestrial.res[[2]]$div.profile 
t3 <- terrestrial.res[[3]]$div.profile 
t4 <- terrestrial.res[[4]]$div.profile
t5 <- terrestrial.res[[5]]$div.profile

m1 <- marine.res[[1]]$div.profile
m2 <- marine.res[[2]]$div.profile
m3 <- marine.res[[3]]$div.profile
m4 <- marine.res[[4]]$div.profile
m5 <- marine.res[[5]]$div.profile

tdf <- data.frame(t1,t2,t3,t4,t5)
mdf <- data.frame(m1,m2,m3,m4,m5)

tmedian.new <- apply(tdf, 1, median)
mmedian.new <- apply(mdf, 1, median)


climate.data <- list.files("D:/CMIP6/overshoot/overshootpostproc", pattern = ".csv", recursive = T, full.names = T)
climate.data <- grep("fldmeanglobaldata", climate.data, value = T)
climate.data <- grep("tas", climate.data, value = T)

models <- c("CanESM5", "CNRM-ESM2-1","GISS-E2-1-G","IPSL-CM6A-LR","MRI-ESM2-0")

result.climate <- list()

for(i in seq_along(models)){
   
   tmp <- grep(models[i], climate.data, value = T)
   historical <- read.csv(tmp[1], header=F)
   ssp534 <- read.csv(tmp[2], header=F)
   ssp585 <- read.csv(tmp[3], header=F)
   
   result.climate[[i]] <- c(as.vector(historical[,1]),as.vector(ssp585[1:25,1]),as.vector(ssp534[,1]))
   if(i == 2)  result.climate[[i]] <- c(as.vector(historical[,1]),as.vector(ssp534[,1]),NA)
}

df <- data.frame(m1 = result.climate[[1]],
                 m2 = result.climate[[2]],
                 m3 = result.climate[[3]],
                 m4 = result.climate[[4]],
                 m5 = result.climate[[5]])

df <- na.omit(df)

median.df <- apply(df, 1, median)
names(median.df) <- 1850:2299
plot(median.df, type="l", col="red", ylab="Temperature", ylim=c(12,21))
lines(df[,2], col="pink")
lines(df[,3], col="blue")
lines(df[,4], col="gold3")
lines(df[,5], col="forestgreen")
lines(df[,1], col="black")

pre.industrial <- mean(median.df[1:50])


mean.clim <- zoo::rollmean(median.df, k=10, align  = "center")


################################################################
# prepare data to plot individual profiles
################################################################



# get median climate for each grid cell

# create temp pre mat to calculate niche limits 

if(file.exists(paste0(path,"Overshoot/tempMatricesMedian/tempMatLandMedian.rda"))){
   
   load(paste0(path,"Overshoot/tempMatricesMedian/tempMatLandMedian.rda"))
   load(paste0(path,"Overshoot/tempMatricesMedian/tempMatOceanMedian.rda"))
   
} else {
files <- list.files(paste0(path,"Overshoot/tempMatrices"),"tempMatLand", full.names = T)

my.list <- list()
for(i in seq_along(files)){
   
   load(files[i])
   tempMatLand <- tempMatLand[,-451]
   print(dim(tempMatLand))
   my.list[[i]] <- tempMatLand
}

a <- do.call(abind, c(my.list, list(along=3)))

clust <- makeCluster(detectCores()-1)
clusterExport(clust, "a")
tempMatLandMedian <- parApply(clust,a, c(1,2), median)
stopCluster(clust)
tempMatLandMedian <- tempMatLandMedian - 273.15

save(tempMatLandMedian, file = paste0(path,"Overshoot/tempMatricesMedian/tempMatLandMedian.rda"))


files <- list.files(paste0(path,"Overshoot/tempMatrices"),"tempMatOcean", full.names = T)

my.list <- list()
for(i in seq_along(files)){
   
   load(files[i])
   tempMatOcean <- tempMatOcean[,-451]
   print(dim(tempMatOcean))
   my.list[[i]] <- tempMatOcean
}


a <- do.call(abind, c(my.list, list(along=3)))

clust <- makeCluster(detectCores()-1)
clusterExport(clust, "a")
tempMatOceanMedian <- parApply(clust,a, c(1,2), median, na.rm = T)
stopCluster(clust)
save(tempMatOceanMedian, file = paste0(path,"Overshoot/tempMatricesMedian/tempMatOceanMedian.rda"))

}


load("C:/Users/pc/Dropbox/rangeHorizonsData/HorizonsMaster/Horizons_nature_2020/Inputs/Raw_Data/Grid/BehrmannMeterGrid_WGS84_land.rda")
load("C:/Users/pc/Dropbox/rangeHorizonsData/Andreas_data/Grid/OceanGrid_without_central_america.rda") # 

tempMatLandMedian <- cbind(griddedDomain$WorldID, tempMatLandMedian)
tempMatOceanMedian <- cbind(OceanGrid$WorldID, tempMatOceanMedian)


files.land <- list.files(paste0(path,"Overshoot/tempMatrices"),"tempMatLand", full.names = T)
files.ocean <- list.files(paste0(path,"Overshoot/tempMatrices"),"tempMatOcean", full.names = T)
my.model <- 1:5
j=3


# tiff(paste0("../../rangeHorizonsData/Andreas_data/Overshoot/figures/Cumulative_exposure_complete_",fig.version,".tif"),
#      h=6, w =12, units="in", res=400, comp="lzw")

jpeg(paste0("../../rangeHorizonsData/Andreas_data/Overshoot/figures/Fig. 2.jpg"), h=6, w =12, units="in", res=600)


m <- matrix(c(1,1,1,1,1,1,2,2,2,3,3,3,
              1,1,1,1,1,1,4,4,4,5,5,5,
              1,1,1,1,1,1,6,6,6,7,7,7), ncol=12, nrow=3, byrow=T)


layout(m)


par(mar=c(12,7,12,7))
par(xpd=T)

cols <- alpha(c("#2b2d42","#8d99ae","#ef233c","#5f0f40"),0.8)


plot("", type="l", ylim=c(0,13), xlim=c(-15,300), xlab="Year", ylab="Populations exposed globally (%)", xaxt="n",yaxt="n", bty="l", bty="u", cex.lab = 1.4, cex.main=1.1) 

mtext(expression(bold("Global Profiles")), side = 3, line = 2.5)

axis(1, at= seq(-14,300,50), labels=2006+seq(-6,300,50), cex.axis=1.2)
# axis(2, at= seq(0,600000,200000), labels = c(0,200,400,600), cex.axis=1.2)
axis(2, at= seq(0,12,4),  cex.axis=1.2)

# polygon(x=c(26,26,87,87),y=c(0,600000,600000,0), col=alpha(cols[3],0.15), border=NA)
polygon(x=c(26,26,87,87),y=c(0,12.5,12.5,0), col=alpha(cols[3],0.15), border=NA)

text(x = 56, y=12.9, label = "Overshoot period (2°C)", col=alpha(cols[3],0.7), cex=1.1)
par(new = TRUE)

   climate <- mean.clim[-c(1:162)]-pre.industrial
   
   plot(x=c(1:279), y=climate, type = "l", axes = FALSE, bty = "n",  xlim=c(-15,300), ylim=c(1.09,3),
        xlab="", ylab="", col=cols[3], lwd=2, cex=1.2)
   




axis(side=4, at=c(1,1.5,2,2.5,3), cex.axis=1.2)

mtext("Global warming level (ºC)", side=4, line=3, cex = 0.9)

legend(y=3, x=135, legend=c("Terrestrial Species","Marine Species","Global Mean Temperature"), col=c(cols[1],cols[2],cols[3]),
       lwd=c(2,2,2), bty="n", cex=1.2)

par(new = TRUE)
plot("", type="l", ylim=c(0,13), xlim=c(-15,300), xlab="", ylab="", xaxt="n",yaxt="n", main = "", bty="l", bty="u") 

lines(tmedian, lwd=2, col=cols[1])
lines(mmedian, lwd=2, col=cols[2])

text(x=-25, y=750000, labels = expression(italic("(a)")), cex=1.1)




files.m <- list.files(paste0(path, "Overshoot/preProfiles/Marine/AllGroups"), rec = T, full.names = T)
files.t <- list.files(paste0(path, "Overshoot/preProfiles/Terrestrial/AllGroups"), rec = T, full.names = T)



load(files.m[my.model[j]])
marine <- preProfiles

load(files.t[my.model[j]])
land <- preProfiles

load(files.land[my.model[j]])
load(files.ocean[my.model[j]])

tempMatLand <- tempMatLand[,-ncol(tempMatLand)]
tempMatLand <- tempMatLand - 273.15

tempMatOcean <- tempMatOcean[,-ncol(tempMatOcean)]

tempMatLand <- cbind(griddedDomain$WorldID, tempMatLand)
tempMatOcean <- cbind(OceanGrid$WorldID, tempMatOcean)

# par(mar=c(5,5,5,7))

lwd <- 1.2
###########################
# Amazon
###########################
k1 <- 25317


my.profile <- land[names(land)==k1][[1]]
profile <- my.profile[,5]/my.profile[1,4]*100
   

max.y <- max(profile)*1.1

par(mar=c(4.2,4.2,4.2,4.2));par(xpd=T)

plot(profile, type="l", bty="l", ylab="Species Exposed (%)", xaxt="n", 
     xlim=c(-15,300), ylim=c(0,100),xlab="", bty="u", lwd=lwd, col=alpha("black",0.8), yaxt="n")
mtext(expression(bold("Amazon")), 3, line = 2.2, cex=0.9)
axis(2, at=c(0,25,50,75,100), labels=c(0,NA,50,NA,100))

if(j==3) {
   overshoot.rectangle <- c(26,26,77,77)
} else {
   overshoot.rectangle <- c(26,26,87,87)
}


polygon(x=overshoot.rectangle,y=c(0,100,100,0), col=alpha(cols[3],0.15), border=NA)
text(x = 56, y=116, label = "Overshoot \nperiod (2°C)", col=alpha(cols[3],0.7), cex=0.8)

axis(1, at= seq(-14,300,50), labels=2006+seq(-6,300,50))

text(x=-37, y=145, labels = expression(italic("(b)")), cex=1.1)

   temp <- tempMatLand[which(tempMatLand[,1]==k1),-1]
   names(temp) <- 1850:2299
   mean.temp<- zoo::rollmean(temp, k=10)
   
   par(new = TRUE)
   
   y.lim <- c(min(mean.temp[-c(1:161)])*1,max(mean.temp[-c(1:161)])*1.01)
   plot(x=c(1:280), y=mean.temp[-c(1:161)], type = "l", axes = FALSE, bty = "n",  xlim=c(-15,300), ylim=y.lim,
        xlab="", ylab="", col=cols[3], lwd=lwd)
   
   
   legend(y=max(mean.temp)*1.022, x=143, legend=c("Horizon profile","Mean Annual \nTemperature"), col=c(alpha("black",0.8),cols[3]),
          lwd=lwd, bty="n", cex=0.8)
   

axis(side=4)



###########################
# Congo basin
###########################
k2 <- 27921
k <- k2

   my.profile <- land[names(land)==k][[1]]
   
   # profile <- (my.profile[,2] - my.profile[,3])/my.profile[1,4]*100
   profile <- my.profile[,5]/my.profile[1,4]*100

   
   files <- list.files(paste0(path, "Overshoot/preProfiles"), pattern="Species", rec = T, full.names = T)
   files <- files[grep("AllGroups", files)]
   

max.y <- max(profile)*1.1

par(mar=c(4.2,4.2,4.2,4.2));par(xpd=T)

plot(profile, type="l", bty="l", ylab="", xaxt="n", 
     xlim=c(-15,300), ylim=c(0,100),xlab="", bty="u", lwd=lwd, col=alpha("black",0.8), yaxt="n")
mtext(expression(bold("Congo Basin")), 3, line = 2.2, cex=0.9)

axis(2, at=c(0,25,50,75,100), labels=c(0,NA,50,NA,100))

if(j==3) {
   overshoot.rectangle <- c(26,26,77,77)
} else {
   overshoot.rectangle <- c(26,26,87,87)
}


polygon(x=overshoot.rectangle,y=c(0,100,100,0), col=alpha(cols[3],0.15), border=NA)
text(x = 56, y=116, label = "Overshoot \nperiod (2°C)", col=alpha(cols[3],0.7), cex=0.8)

axis(1, at= seq(-14,300,50), labels=2006+seq(-6,300,50))

temp <- tempMatLand[which(tempMatLand[,1]==k2),-1]

text(x=-37, y=145, labels = expression(italic("(c)")), cex=1.1)


   temp <- tempMatLand[which(tempMatLand[,1]==k2),-1]
   names(temp) <- 1850:2299
   mean.temp<- zoo::rollmean(temp, k=10)
   
   par(new = TRUE)
   
   
   y.lim <- c(min(mean.temp[-c(1:161)])*1,max(mean.temp[-c(1:161)])*1.01)
   plot(x=c(1:280), y=mean.temp[-c(1:161)], type = "l", axes = FALSE, bty = "n",  xlim=c(-15,300), ylim=y.lim,
        xlab="", ylab="", col=cols[3], lwd=lwd)
   


axis(side=4)

mtext("Mean Annual Temp. (°C)", side=4, line=3, cex = .6)


###########################
# Pantanal
###########################

k3 <- 19204
k <- k3

my.profile <- land[names(land)==k][[1]]
   
profile <- my.profile[,5]/my.profile[1,4]*100

max.y <- max(profile)*1.1

par(mar=c(4.2,4.2,4.2,4.2));par(xpd=T)

plot(profile, type="l", bty="l", ylab="Species Exposed (%)", xaxt="n", 
     xlim=c(-15,300), ylim=c(0,100),xlab="", bty="u", lwd=lwd, col=alpha("black",0.8), yaxt="n")

mtext(expression(bold("Pantanal Wetlands")), 3, line = 2.2, cex=0.9)
axis(2, at=c(0,25,50,75,100), labels=c(0,NA,50,NA,100))


if(j==3) {
   overshoot.rectangle <- c(26,26,77,77)
} else {
   overshoot.rectangle <- c(26,26,87,87)
}


polygon(x=overshoot.rectangle,y=c(0,100,100,0), col=alpha(cols[3],0.15), border=NA)
text(x = 56, y=116, label = "Overshoot \nperiod (2°C)", col=alpha(cols[3],0.7), cex=0.8)

axis(1, at= seq(-14,300,50), labels=2006+seq(-6,300,50))

temp <- tempMatLandMedian[which(tempMatLandMedian[,1]==k3),-1]
names(temp) <- 1850:2299

text(x=-37, y=145, labels = expression(italic("(d)")), cex=1.1)


   temp <- tempMatLand[which(tempMatLand[,1]==k3),-1]
   names(temp) <- 1850:2299
   mean.temp<- zoo::rollmean(temp, k=10)
   
   par(new = TRUE)
   
   
   y.lim <- c(min(mean.temp[-c(1:161)])*1,max(mean.temp[-c(1:161)])*1.01)
   plot(x=c(1:280), y=mean.temp[-c(1:161)], type = "l", axes = FALSE, bty = "n",  xlim=c(-15,300), ylim=y.lim,
        xlab="", ylab="", col=cols[3], lwd=lwd)
   




axis(side=4)



###########################
# Coral Triangle
#########################

k4 <- 27306
k <- k4

my.profile <- marine[names(marine)==k][[1]]
   
profile <- my.profile[,5]/my.profile[1,4]*100

max.y <- max(profile)*1.1

par(mar=c(4.2,4.2,4.2,4.2));par(xpd=T)

plot(profile, type="l", bty="l", ylab="", xaxt="n", 
     xlim=c(-15,300), ylim=c(0,100),xlab="Year", bty="u", lwd=lwd, col=alpha("black",0.8), yaxt="n")

mtext(expression(bold("Coral Triangle")), 3, line = 2.2, cex=0.9)
axis(2, at=c(0,25,50,75,100), labels=c(0,NA,50,NA,100))

if(j==3) {
   overshoot.rectangle <- c(26,26,77,77)
} else {
   overshoot.rectangle <- c(26,26,87,87)
}


polygon(x=overshoot.rectangle,y=c(0,100,100,0), col=alpha(cols[3],0.15), border=NA)
text(x = 56, y=116, label = "Overshoot \nperiod (2°C)", col=alpha(cols[3],0.7), cex=0.8)

axis(1, at= seq(-14,300,50), labels=2006+seq(-6,300,50))

temp <- tempMatOceanMedian[which(tempMatOceanMedian[,1]==k4),-1]

text(x=-37, y=145, labels = expression(italic("(e)")), cex=1.1)



   temp <- tempMatOcean[which(tempMatOcean[,1]==k4),-1]
   names(temp) <- 1850:2299
   mean.temp<- zoo::rollmean(temp, k=10)
   
   par(new = TRUE)
   
   
   y.lim <- c(min(mean.temp[-c(1:161)])*1,max(mean.temp[-c(1:161)])*1.01)
   plot(x=c(1:280), y=mean.temp[-c(1:161)], type = "l", axes = FALSE, bty = "n",  xlim=c(-15,300), ylim=y.lim,
        xlab="", ylab="", col=cols[3], lwd=lwd)
   


axis(side=4)

mtext("Mean Annual Temp. (°C)", side=4, line=3, cex = .6)


###########################
# Caribbean
#########################


k5 <- 36820
k <- k5

my.profile <- marine[names(marine)==k][[1]]
   
profile <- my.profile[,5]/my.profile[1,4]*100

max.y <- max(profile)*1.1

par(mar=c(4.2,4.2,4.2,4.2));par(xpd=T)

plot(profile, type="l", bty="l", ylab="Species Exposed (%)", xaxt="n", 
     xlim=c(-15,300), ylim=c(0,100),xlab="Year", bty="u", lwd=lwd, col=alpha("black",0.8), yaxt="n")

mtext(expression(bold("Caribbean Sea")), 3, line = 2.2, cex=0.9)
axis(2, at=c(0,25,50,75,100), labels=c(0,NA,50,NA,100))

if(j==3) {
   overshoot.rectangle <- c(26,26,77,77)
} else {
   overshoot.rectangle <- c(26,26,87,87)
}


polygon(x=overshoot.rectangle,y=c(0,100,100,0), col=alpha(cols[3],0.15), border=NA)
text(x = 56, y=116, label = "Overshoot \nperiod (2°C)", col=alpha(cols[3],0.7), cex=0.8)

axis(1, at= seq(-14,300,50), labels=2006+seq(-6,300,50))

temp <- tempMatOceanMedian[which(tempMatOceanMedian[,1]==k5),-1]
names(temp) <- 1850:2299

text(x=-37, y=145, labels = expression(italic("(f)")), cex=1.1)


   temp <- tempMatOcean[which(tempMatOcean[,1]==k5),-1]
   names(temp) <- 1850:2299
   mean.temp<- zoo::rollmean(temp, k=10)
   
   par(new = TRUE)
   
   
   y.lim <- c(min(mean.temp[-c(1:161)])*1,max(mean.temp[-c(1:161)])*1.01)
   plot(x=c(1:280), y=mean.temp[-c(1:161)], type = "l", axes = FALSE, bty = "n",  xlim=c(-15,300), ylim=y.lim,
        xlab="", ylab="", col=cols[3], lwd=lwd)
   

axis(side=4)

mtext("Mean Annual Temp. (°C)", side=4, line=3, cex = .6)



par(mar=c(1,1,1,1))
par(xpd=T)
plot(ne_countries(), axes=F)


coords <- rbind(griddedDomain, OceanGrid[,1:3])
coords <- coords[which(coords$WorldID %in% c(k1,k2,k3,k4,k5)),]


coords.df <- coords@data[!duplicated(coords@data$WorldID),]



for(i in 1:5){
   points(x=coords.df$X_COORD[i], y=coords.df$Y_COORD[i], col="black", pch=21, cex=3, bg="red")
}

text(x=coords.df[which(coords$WorldID == k1),2], y=coords.df[which(coords$WorldID == k1),3]+1, col="white", labels = "b")
text(x=coords.df[which(coords$WorldID == k2),2], y=coords.df[which(coords$WorldID == k2),3]+1, col="white", labels = "c")
text(x=coords.df[which(coords$WorldID == k3),2], y=coords.df[which(coords$WorldID == k3),3]+1, col="white", labels = "d")
text(x=coords.df[which(coords$WorldID == k4),2], y=coords.df[which(coords$WorldID == k4),3]+1, col="white", labels = "e")
text(x=coords.df[which(coords$WorldID == k5),2], y=coords.df[which(coords$WorldID == k5),3]+1, col="white", labels = "f")

text(x=-143, y=110, labels = expression(italic("(g)")), cex=1.1)


dev.off()

















