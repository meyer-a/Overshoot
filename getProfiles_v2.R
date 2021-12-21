load("C:/Users/pc/Dropbox/rangeHorizonsData/Andreas_data/Grid/OceanGrid_without_central_america.rda") #

library(Gmisc)
library(dplyr)
library(openxlsx)


load("C:/Users/pc/Dropbox/rangeHorizonsData/Andreas_data/Overshoot/rawResults/Marine/BenthicInverts/BenthicInverts_GISS-E2-1-G.rda")

x <- as.data.frame(fastDoCall(rbind, rawResults))

rm("rawResults"); gc()


rownames(x) <- NULL

source("functions/getDivProfiles.R")
source("functions/getExposureYears.R")


#
data <- read.xlsx("../../cel29320.xlsx")

# cell.id <- 25889
# cell.id <- 30722

# pdf("Overshoot_prelim.pdf", h=7,w=11)
# par(mfrow=c(3,4))
# par(mar=c(5,5,1,2))
# for(k in 1:12){

cell.id <- sample(x$WorldID,1)

# cell.id <- 18259
data <- x[x$WorldID == cell.id,]
data <- na.omit(data)
dim(data)
# data[,280:296]

getProfiles <- function(data){


data <- data[,-c(1,2)]


results <- apply(data,1,getExposureYears)

if(class(results) == "list"){

r2 <- fastDoCall(rbind, results)
r2 <- na.omit(r2)

# if there are zeros in the values, replace by -1
if(any(r2$values==0) == TRUE) r2[r2$values==0,]$values <- -1

exposure <- r2[r2$values==-1,]

# if there are exposure events in the cell
if(nrow(exposure)>=1){
exposure <- aggregate(values ~ year, exposure, sum)
exposure$values <- exposure$values*-1

expProfile <- data.frame(year=2006:2300, n.species.exp=0)
expProfile <- left_join(expProfile, exposure, by="year")
expProfile[is.na(expProfile$values),]$values <- 0
expProfile$n.species.exp <- cumsum(expProfile$values)
expProfile <- expProfile[,-3]
} else { # if there are not

expProfile <- data.frame(year=2006:2300, n.species.exp=0)
  
}

suitability <- r2[r2$values==1,]

if(nrow(suitability)>=1){
suitability <- aggregate(values ~ year, suitability, sum)

suiProfile <- data.frame(n.species.sui=0, pool=nrow(data), year=2006:2300)
suiProfile <- left_join(suiProfile, suitability, by="year")
suiProfile[is.na(suiProfile$values),]$values <- 0
suiProfile$n.species.sui <- cumsum(suiProfile$values)
suiProfile <- suiProfile[,-c(3,4)]

} else { # if there are not
  
suiProfile <- data.frame(year=2006:2300, n.species.sui=0, pool=nrow(data))
suiProfile <- suiProfile[,-1]  

}

final <- cbind(expProfile, suiProfile)
return(final)

}



if(class(results) == "logical"){

expProfile <- data.frame(year=2006:2300, n.species.exp=0)
suiProfile <- data.frame(n.species.sui=0, pool=nrow(data))
final <- cbind(expProfile, suiProfile)
return(final)

}
}

# final
# 
# 
# y <- final$n.species.exp - final$n.species.sui
# y <- (y/nrow(data))*100
# plot(y, type="l", ylim=c(0,100), xlim=c(-6,300), xaxt="n", col="red",lwd=2, xlab="Year", ylab="Species exposed (%)")
# axis(1, at= seq(-6,300,50), labels=2006+seq(-6,300,50))
# 
# }
# 
# dev.off()

