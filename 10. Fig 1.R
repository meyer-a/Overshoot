rm(list=ls()) # cleaning up
# Load required libraries
library(ncdf4)
library(maps)
library(fields)
library(MASS)
library(plotrix)
library(RColorBrewer)
library(scales)
library(zoo)


# setwd("/media/romaric/Seagate_Expansion_Drive_01/Andreas_Data/overshoot/overshootpostproc/data")

heatsst  <- nc_open("../../rangeHorizonsData/Andreas_data/Overshoot/extremedata/var_Omon_ENS-MEDIAN-ALL_historical-ssp585-ssp534_r1i1p1f1_185001-230012.nc")
heatday  <- nc_open("../../rangeHorizonsData/Andreas_data/Overshoot/extremedata/heatdayGM_year_ENS-MEDIAN_historical-ssp585-ssp534-over_r1i1p1f1_1850-2300.nc")
heatarea <- nc_open("../../rangeHorizonsData/Andreas_data/Overshoot/extremedata/landareawHSDay_year_ENS-MEDIAN_historical-ssp585-ssp534-over_r1i1p1f1_1850-2300.nc")

sstheat  <- ncvar_get(heatsst, "var")
landheat <- ncvar_get(heatday, "heatday")
landarea <- ncvar_get(heatarea, "heatarea")

cols <- alpha("#ef233c",0.8)

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

pre.industrial <- mean(median.df[1:50])
mean.clim <- zoo::rollmean(median.df, k=10, align  = "center")



# tas <- ncvar_get(meantas, "tas")

sstheat  <- sstheat[101:451]
landheat <- landheat[101:451]
landarea <- landarea[101:451]*100.0
mean.clim <- mean.clim[97:441]



jpeg("../../rangeHorizonsData/Andreas_data/Overshoot/figures/Fig. 1.jpg", h = 7, w = 5, units="in", res=700)
par(mfrow = c(3, 1), mar=c(4,6,5,6), xpd=T)


plot(mean.clim, type="l", lwd=1.1, col=cols, bty="l", xlab="", ylab="",
     xaxt="n", yaxt = "n", xlim=c(1,350),ylim=c(13.5,16.7), main = "Global Mean Surface Air Temperature", cex.lab=1.2)

polygon(x = c(92, 92, 153, 153),  # X-Coordinates of polygon 
        y = c(13.5, 16.5, 16.5, 13.5),    # Y-Coordinates of polygon
        col = alpha(cols, 0.15), border = NA)
lines(mean.clim, lwd=1.3, col=cols)


axis(side = 1, at = seq(1,351,50), labels = seq(1950, 2300, 50), cex.axis=1.1)

axis(side = 2, at = seq(14, 16.5,0.5), labels=c(14,NA,15,NA,16,NA), cex.axis=1.1)
mtext("Temperature (°C)", side=2, line=3.4, cex=0.8)
mtext("(a)", side = 3, col = "black", line = 2.1, cex = .8, adj = 0, font=3)


text(x = 122.5, y=16.8, label = "Overshoot period (2°C)", col=alpha(cols,0.7), cex=1)



plot(landheat, type="l",col="#2b2d42", lwd=1.1, bty="u", xlab="", ylab="",
     xaxt="n", main = "Heat Stress Frequency", cex.lab=1.1, cex.axis=1.1, ylim=c(0,40))


polygon(x = c(92, 92, 153, 153),  # X-Coordinates of polygon 
        y = c(0, 42, 42, 0),    # Y-Coordinates of polygon
        col = alpha(cols, 0.15), border = NA)

lines(landheat, col="#2b2d42", lwd=1.1)
axis(side = 1, at = seq(1,351,50), labels = seq(1950, 2300, 50), cex.axis=1.1)
mtext("(b)", side = 3, col = "black", line = 2, cex = .8, adj = 0, font=3)
mtext("Land heat stress \nfrequency (days/year)", side=2, line=3.4, cex=0.8)

par(new=T)
plot(sstheat, type="l", col="deepskyblue2", lwd=1.1, xaxt="n", yaxt="n", xlab="", ylab="", bty="n",ylim = c(0,2.2) )
axis(side=4, at=c(0,0.5,1,1.5,2), cex.axis=1, cex.axis=1.1)
mtext("Corals heat stress \nfrequency (months/year)", side=4, line=4, cex = 0.8)

legend(x=265, y=2.6, legend=c("Land","Corals"), lty=1, lwd=2, col=c("#2b2d42","grey70"), bty="n")



plot(landarea, type="l", col="#2b2d42",lwd=1.1, xaxt="n", xlab="Year",bty="l",main="Land Fraction with Heat Stress",
     ylab="", cex.lab=1.1, cex.axis=1.1, ylim=c(35,102))
axis(side = 1, at = seq(1,351,50), labels = seq(1950, 2300, 50), cex.axis=1.1)

polygon(x = c(92, 92, 153, 153),  # X-Coordinates of polygon 
        y = c(35, 102, 102, 35),    # Y-Coordinates of polygon
        col = alpha(cols, 0.15), border = NA)

lines(landarea,col="#2b2d42",lwd=1.1)
mtext("(c)", side = 3, col = "black", line = 2, cex = .8, adj = 0, font=3)

mtext("Fraction exposed to \nheat stress (%)", side=2, line=3.4, cex=0.8)

dev.off()

