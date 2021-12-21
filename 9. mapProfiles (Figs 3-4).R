library(tidyverse)
library(abind)
library(sf)
library(fasterize)
library(raster)
library(sp)
library(scales)
library(rnaturalearth)
library(RColorBrewer)
library(ggpointdensity)
library(ggpubr)
library(ggnewscale)
library(viridis)

source("functions/scale_fill_fermenter_custom.R")
source("http://www.math.mcmaster.ca/bolker/R/misc/legendx.R")


models <- c("CanESM5", "CNRM-ESM2-1","GISS-E2-1-G","IPSL-CM6A-LR","MRI-ESM2-0")


terrestrial.data <- list.files("C:/Users/pc/Dropbox/rangeHorizonsData/Andreas_data/Overshoot/mapData", pattern = "TerrestrialSpecies_v4", full.names = T)

my.list <- list()
df<- data.frame(WorldID = griddedDomain$WorldID)
for(i in 1:5){
  tmp <- terrestrial.data[grep(models[i], terrestrial.data)]
  load(tmp)
  metrics <- metrics %>% mutate(time.2c.year.nodexp = replace(time.2c.year, time.2c.year==2301, 10000))
  metrics <- metrics %>% mutate(time.2c.duration = replace(time.2c.duration, time.2c.year==2301, NA))
  my.list[[i]] <- left_join(df, metrics, by="WorldID")
  print(dim(my.list[[i]]))
  
}

terrestrial.tmp <- abind(my.list, along=3)
terrestrial.tmp <- apply(terrestrial.tmp, c(1,2), median, na.rm=T)
terrestrial.tmp <- as.data.frame(terrestrial.tmp)
# terrestrial <-  metrics # %>% filter(dexp != "NaN") 

marine.data <- list.files("C:/Users/pc/Dropbox/rangeHorizonsData/Andreas_data/Overshoot/mapData", pattern = "MarineSpecies_v4", full.names = T)

my.list <- list()
df<- data.frame(WorldID = OceanGrid$WorldID)
for(i in 1:5){
tmp <- marine.data[grep(models[i], marine.data)]
load(tmp)
metrics <- metrics %>% mutate(time.2c.year.nodexp = replace(time.2c.year, time.2c.year==2301, 10000))
metrics <- metrics %>% mutate(time.2c.duration = replace(time.2c.duration, time.2c.year==2301, NA))
my.list[[i]] <- left_join(df, metrics, by="WorldID")
print(dim(my.list[[i]]))
}

marine.tmp <- abind(my.list, along=3)
marine.tmp <- apply(marine.tmp, c(1,2), median, na.rm=T)
marine.tmp <- as.data.frame(marine.tmp)

terrestrial <- terrestrial.tmp %>% filter(max.exp.over.abs >= 5)
marine <- marine.tmp %>% filter(max.exp.over.abs >= 5)


load("C:/Users/pc/Dropbox/rangeHorizonsData/Andreas_data/Grid/OceanGrid_without_central_america.rda") # 
load("C:/Users/pc/Dropbox/rangeHorizonsData/Andreas_data/Grid/BehrmannMeterGrid_WGS84_land.rda") # 
OceanGridMag <- OceanGrid
griddedDomainMag <- griddedDomain


median(terrestrial$timing.exp.2100, na.rm=T)
median(terrestrial$timing.dexp.2100, na.rm=T)

median(terrestrial$timing.exp.over, na.rm=T)
median(terrestrial$timing.dexp.over, na.rm=T)

median(marine$timing.exp.2100, na.rm=T)
median(marine$timing.dexp.2100, na.rm=T)

median(marine$timing.exp.over, na.rm=T)
median(marine$timing.dexp.over, na.rm=T)

median(terrestrial$abrupt.exp.over, na.rm=T)
median(terrestrial$abrupt.dexp.over, na.rm=T)

median(marine$abrupt.exp.over, na.rm=T)
median(marine$abrupt.dexp.over, na.rm=T)


ocean.grid.data <- left_join(OceanGrid@data, marine, by = "WorldID")
land.grid.data <- left_join(griddedDomain@data, terrestrial, by = "WorldID")

ocean.grid.data.mag <- left_join(OceanGridMag@data, marine.tmp, by = "WorldID")
land.grid.data.mag  <- left_join(griddedDomainMag@data, terrestrial.tmp, by = "WorldID")


OceanGrid@data <- cbind(OceanGrid@data,ocean.grid.data)
griddedDomain@data <- cbind(griddedDomain@data,land.grid.data)

OceanGridMag@data <- cbind(OceanGridMag@data,ocean.grid.data.mag)
griddedDomainMag@data <- cbind(griddedDomainMag@data,land.grid.data.mag)


ocean.grid <- st_as_sf(OceanGrid)
land.grid <- st_as_sf(griddedDomain)

ocean.grid.mag <- st_as_sf(OceanGridMag)
land.grid.mag <- st_as_sf(griddedDomainMag)

ocean.grid.2c.2300 <- st_as_sf(OceanGrid[which(OceanGrid$time.2c.year==2301),])
land.grid.2c.2300  <- st_as_sf(griddedDomain[which(griddedDomain$time.2c.year==2301),])


r <- raster(ncol=3600,nrow=1800, res=1)
r.land <- fasterize(sf=land.grid,r)
r.ocean <- fasterize(sf=ocean.grid,r)

r.land.mag  <- fasterize(sf=land.grid.mag ,r)
r.ocean.mag  <- fasterize(sf=ocean.grid.mag ,r)



# magnitude land
max.exp.2100.land <- fasterize(land.grid.mag, r.land.mag, field = "max.exp.2100")
max.exp.over.land  <- fasterize(land.grid.mag, r.land.mag, field = "max.exp.over")
final.exp.land <- fasterize(land.grid.mag, r.land.mag, field = "final.exp")
dexp.land <- fasterize(land.grid.mag, r.land.mag, field = "dexp")

# magnitude ocean
max.exp.2100.ocean <- fasterize(ocean.grid.mag, r.ocean.mag, field = "max.exp.2100")
max.exp.over.ocean  <- fasterize(ocean.grid.mag, r.ocean.mag, field = "max.exp.over")
final.exp.ocean <- fasterize(ocean.grid.mag, r.ocean.mag, field = "final.exp")
dexp.ocean <- fasterize(ocean.grid.mag, r.ocean.mag, field = "dexp")




# timing land
timing.exp.2100.land <- fasterize(land.grid, r.land, field = "timing.exp.2100")
timing.exp.over.land <- fasterize(land.grid, r.land, field = "timing.exp.over")
timing.dexp.2100.land <- fasterize(land.grid, r.land, field = "timing.dexp.2100")
timing.dexp.over.land <- fasterize(land.grid, r.land, field = "timing.dexp.over")

diff.timing.2100.land <- fasterize(land.grid, r.land, field = "diff.timing.2100")
diff.timing.over.land <- fasterize(land.grid, r.land, field = "diff.timing.over")

# timing ocean
timing.exp.2100.ocean <- fasterize(ocean.grid, r.ocean, field = "timing.exp.2100")
timing.exp.over.ocean <- fasterize(ocean.grid, r.ocean, field = "timing.exp.over")
timing.dexp.2100.ocean <- fasterize(ocean.grid, r.ocean, field = "timing.dexp.2100")
timing.dexp.over.ocean <- fasterize(ocean.grid, r.ocean, field = "timing.dexp.over")

diff.timing.2100.ocean <- fasterize(ocean.grid, r.ocean, field = "diff.timing.2100")
diff.timing.over.ocean <- fasterize(ocean.grid, r.ocean, field = "diff.timing.over")

# abruptness land
abrupt.exp.2100.land <- fasterize(land.grid, r.land, field = "abrupt.exp.2100")
abrupt.exp.over.land <- fasterize(land.grid, r.land, field = "abrupt.exp.over")
abrupt.dexp.2100.land <- fasterize(land.grid, r.land, field = "abrupt.dexp.2100")
abrupt.dexp.over.land <- fasterize(land.grid, r.land, field = "abrupt.dexp.over")

# timing ocean
abrupt.exp.2100.ocean <- fasterize(ocean.grid, r.ocean, field = "abrupt.exp.2100")
abrupt.exp.over.ocean <- fasterize(ocean.grid, r.ocean, field = "abrupt.exp.over")
abrupt.dexp.2100.ocean <- fasterize(ocean.grid, r.ocean, field = "abrupt.dexp.2100")
abrupt.dexp.over.ocean <- fasterize(ocean.grid, r.ocean, field = "abrupt.dexp.over")


# 2c time
land.2c <- fasterize(land.grid, r.land, field = "time.2c.duration")
ocean.2c <- fasterize(ocean.grid, r.ocean, field = "time.2c.duration")

land.2c.nodexp <- fasterize(land.grid, r.land, field = "time.2c.year.nodexp")
ocean.2c.nodexp <- fasterize(ocean.grid, r.ocean, field = "time.2c.year.nodexp")



max.exp.2100 <- raster::merge(max.exp.2100.land,max.exp.2100.ocean)
max.exp.over <- raster::merge(max.exp.over.land,max.exp.over.ocean)
final.exp <- raster::merge(final.exp.land,final.exp.ocean)
dexp <- raster::merge(dexp.land, dexp.ocean)

timing.exp.2100  <- raster::merge(timing.exp.2100.land, timing.exp.2100.ocean)
timing.dexp.2100 <- raster::merge(timing.dexp.2100.land, timing.dexp.2100.ocean)
timing.exp.over  <- raster::merge(timing.exp.over.land, timing.exp.over.ocean)
timing.dexp.over <- raster::merge(timing.dexp.over.land, timing.dexp.over.ocean)

diff.timing.2100 <- raster::merge(diff.timing.2100.land, diff.timing.2100.ocean)
diff.timing.over <- raster::merge(diff.timing.over.land, diff.timing.over.ocean)

abrupt.exp.2100  <- raster::merge(abrupt.exp.2100.land, abrupt.exp.2100.ocean)
abrupt.dexp.2100 <- raster::merge(abrupt.dexp.2100.land, abrupt.dexp.2100.ocean)
abrupt.exp.over  <- raster::merge(abrupt.exp.over.land, abrupt.exp.over.ocean)
abrupt.dexp.over <- raster::merge(abrupt.dexp.over.land, abrupt.dexp.over.ocean)

time.2c <- raster::merge(land.2c, ocean.2c)
# time.2c.2300 <- raster::merge(land.2c.2300, ocean.2c.2300)
time.2c.nodexp <- raster::merge(land.2c.nodexp, ocean.2c.nodexp)
time.2c.nodexp.4models <- reclassify(time.2c.nodexp, rcl = cbind(0,7000,NA), right=FALSE)
time.2c.nodexp.2models <- reclassify(time.2c.nodexp, rcl = cbind(0,5000,NA), right=FALSE)
time.2c.nodexp.2models <- reclassify(time.2c.nodexp.2models, rcl = cbind(7000,11000,NA), right=FALSE)

time.2c.nodexp.4models[time.2c.nodexp.4models > 0] <- 4000
time.2c.nodexp.2models[time.2c.nodexp.2models > 0] <- 2000


# calc area occupied by each time.2c category
x <- getValues(time.2c)
x <- x[!is.na(x)]
table(x > 0.0001)

x <- getValues(time.2c.nodexp.4models)
x <- x[!is.na(x)]
length(x)

x <- getValues(time.2c.nodexp.2models)
x <- x[!is.na(x)]
length(x)

# 4160 cells with shorter duration (38%)
# 3839 cells with longer overshoot (35%)
# 2096 cells with uncertainty (19%)
# 801 no return (8%)
# total: 10896


robin.proj <- CRS("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
countries <- st_as_sf((spTransform(ne_countries(), robin.proj)))
graticules <- st_as_sf((spTransform(ne_download(type="graticules_30", category = "physical"), robin.proj)))
bbox <- st_as_sf((spTransform(ne_download(type="wgs84_bounding_box", category = "physical"), robin.proj)))

# Magnitude
max.exp.2100.df <- as.data.frame(projectRaster(max.exp.2100, crs=robin.proj, over=T), xy=TRUE)
max.exp.over.df <- as.data.frame(projectRaster(max.exp.over, crs=robin.proj, over=T), xy=TRUE)
final.exp.df <- as.data.frame(projectRaster(final.exp, crs=robin.proj, over=T), xy=TRUE)
dexp.df <- as.data.frame(projectRaster(dexp, crs=robin.proj, over=T), xy=TRUE)

# Timing
timing.exp.2100.df <- as.data.frame(projectRaster(timing.exp.2100, crs=robin.proj, over=T), xy=TRUE)
timing.dexp.2100.df <- as.data.frame(projectRaster(timing.dexp.2100, crs=robin.proj, over=T), xy=TRUE)
timing.exp.over.df <- as.data.frame(projectRaster(timing.exp.over, crs=robin.proj, over=T), xy=TRUE)
timing.dexp.over.df <- as.data.frame(projectRaster(timing.dexp.over, crs=robin.proj, over=T), xy=TRUE)

diff.timing.2100.df <- as.data.frame(projectRaster(diff.timing.2100, crs=robin.proj, over=T), xy=TRUE)
diff.timing.over.df <- as.data.frame(projectRaster(diff.timing.over, crs=robin.proj, over=T), xy=TRUE)

abrupt.exp.2100.df <- as.data.frame(projectRaster(abrupt.exp.2100, crs=robin.proj, over=T), xy=TRUE)
abrupt.dexp.2100.df <- as.data.frame(projectRaster(abrupt.dexp.2100, crs=robin.proj, over=T), xy=TRUE)
abrupt.exp.over.df <- as.data.frame(projectRaster(abrupt.exp.over, crs=robin.proj, over=T), xy=TRUE)
abrupt.dexp.over.df <- as.data.frame(projectRaster(abrupt.dexp.over, crs=robin.proj, over=T), xy=TRUE)

time.2c.df <- as.data.frame(projectRaster(time.2c, crs=robin.proj, over=T), xy=TRUE)
time.2c.2300.df <- as.data.frame(projectRaster(time.2c.2300, crs=robin.proj, over=T), xy=TRUE)
time.2c.nodexp.df <- as.data.frame(projectRaster(time.2c.nodexp, crs=robin.proj, over=T), xy=TRUE)

time.2c.nodexp.4models.df <- as.data.frame(projectRaster(time.2c.nodexp.4models, crs=robin.proj, over=T), xy=TRUE)
time.2c.nodexp.2models.df <- as.data.frame(projectRaster(time.2c.nodexp.2models, crs=robin.proj, over=T), xy=TRUE)



time.2c.df.2 <- as.data.frame(time.2c, xy=TRUE)


# A few corrections
# In some places the max number of species exposed was negative our above 100
# max.exp.p.df[which(max.exp.p.df$layer<0),3] <- 0
# max.exp.p.df[which(max.exp.p.df$layer>100),3] <- 100
# final.exp.p.df[which(final.exp.p.df$layer<0),3] <- 0
# de.exp.p.df[which(de.exp.p.df$layer<0),3] <- 0

theme_set(theme_bw() + theme(legend.position="bottom",
                             legend.key.height= unit(0.5, 'cm'),
                             legend.key.width= unit(2, 'cm'),
                             panel.grid.major = element_blank(),
                             axis.text.x=element_blank(),
                             axis.text.y=element_blank(),
                             axis.title.y=element_blank(),
                             axis.title.x=element_blank(),
                             panel.border=element_blank(),
                             axis.ticks=element_blank(),
                             plot.title = element_text(),
                             plot.subtitle = element_text(size=9, hjust=0))) 

# Maximum Exposure before 2100
max.exp.2100.gg <- ggplot() +
  geom_sf(data=bbox, colour="white", fill="gray85") +
  geom_raster(data=max.exp.2100.df ,mapping=aes(x=x, y=y, fill=layer)) +
  scale_fill_viridis_b(breaks=seq(0,100,10), na.value=NA, option = "A", begin=0.2,end = 0.9, direction=-1,
                        limits = c(0,100)) +
  geom_sf(data=graticules, colour=alpha("white",0.7)) +
  geom_sf(data = countries, fill = NA,
                    colour = alpha("black",0.8), size = 0.5)+
  labs(title = "Maximum exposure before 2100 (%)", subtitle = "Maximum percentage of species exposed during the temperature overshoot before 2100") +
  guides(fill = guide_colourbar(title = "Species exposed (%)", title.position = "top",
                                barwidth = 15, title.hjust = 0.5, title.vjust = 2)); max.exp.2100.gg

# Maximum Exposure overall
max.exp.over.gg <- ggplot() +
  geom_sf(data=bbox, colour="white", fill="gray85") +
  geom_raster(data=max.exp.over.df %>% filter(layer > 0) ,mapping=aes(x=x, y=y, fill=layer)) +
  scale_fill_viridis_b(breaks=seq(0,100,10), na.value=NA, option = "A", begin=0.2,end = 0.9, direction=-1,
                       labels = c(">0", seq(10,100,10)), limits = c(0,100)) +
  geom_sf(data=graticules, colour=alpha("white",0.7)) +
  geom_sf(data = countries, fill = NA,
          colour = alpha("black",0.8), size = 0.5)+
  labs(title = "Maximum exposure (%)", subtitle = "Maximum percentage of species exposed between 2015-2300") +
  guides(fill = guide_colourbar(title = "Species exposed (%)", title.position = "top",
                                barwidth = 15, title.hjust = 0.5, title.vjust = 2)); max.exp.over.gg

# Final exposure
final.exp.gg <- ggplot() +
  geom_sf(data=bbox, colour="white", fill="gray88") +
  geom_raster(data=final.exp.df %>% filter(layer > 0), mapping=aes(x=x, y=y, fill=layer)) +
  scale_fill_viridis_b(breaks=seq(0,100,10), na.value=NA, option = "A", begin=0.2,end = 0.9, direction=-1, limits=c(0,100),
                       labels = c(">0", seq(10,100,10))) +
  geom_sf(data=graticules, colour=alpha("white",0.7)) +
  geom_sf(data = countries, fill = NA,
          colour = alpha("black",0.8), size = 0.5)+
  labs(title = "Final exposure (%)", subtitle = "Percentage of species exposed at 2300") +
  guides(fill = guide_colourbar(title = "Species exposed (%)", title.position = "top",
                                barwidth = 15, title.hjust = 0.5, title.vjust = 2)); final.exp.gg

# De-exposure
dexp.gg <- ggplot() +
  geom_sf(data=bbox, colour="white", fill="gray88") +
  geom_raster(data=dexp.df %>% filter(layer > 0), mapping=aes(x=x, y=y, fill=layer)) +
  scale_fill_viridis_b(breaks=seq(0,100,10), na.value=NA, option = "A", begin=0.2,end = 0.9, direction=-1,
                       labels = c(">0", seq(10,100,10)), limits = c(0,100)) +
  geom_sf(data=graticules, colour=alpha("white",0.6)) +
  geom_sf(data = countries, fill = NA,
          colour = alpha("black",0.7), size = 0.5)+
  labs(title = "Total de-exposure (%)", subtitle = "Percentage of species de-exposed by 2300") +
  guides(fill = guide_colourbar(title = "Species de-exposed (%)", title.position = "top",
                                barwidth = 15, title.hjust = 0.5, title.vjust = 2)); dexp.gg


# Abruptness of exposure before 2100
abrupt.exp.2100.gg <- ggplot() +
  geom_sf(data=bbox, colour="white", fill="gray88") +
  geom_raster(data=abrupt.exp.2100.df %>%  filter(layer > 0),mapping=aes(x=x, y=y, fill=layer)) +
  scale_fill_viridis_b(breaks=seq(0,100,10), na.value=NA, option = "D", begin=0.2,end = 1, direction=1, limits=c(0,100)) +
  geom_sf(data=graticules, colour=alpha("white",0.6)) +
  geom_sf(data = countries, fill = NA,
          colour = alpha("black",0.7), size = 0.5)+
  labs(title = "Abruptness of exposure", subtitle = "Percentage of species exposed at the worst decade") +
  guides(fill = guide_colourbar(title = "Species exposed (%)", title.position = "top",
                                barwidth = 15, title.hjust = 0.5, title.vjust = 2));abrupt.exp.2100.gg

# Abruptness of exposure overall
abrupt.exp.over.gg <- ggplot() +
  geom_sf(data=bbox, colour="white", fill="gray88") +
  geom_raster(data=abrupt.exp.over.df %>%  filter(layer > 0),mapping=aes(x=x, y=y, fill=layer)) +
  scale_fill_viridis_b(breaks=seq(0,100,10), na.value=NA, option = "D", begin=0.2,end = 1, direction=1, limits=c(0,100)) +
  geom_sf(data=graticules, colour=alpha("white",0.6)) +
  geom_sf(data = countries, fill = NA,
          colour = alpha("black",0.7), size = 0.5)+
  labs(title = "Abruptness of exposure", subtitle = "Percentage of species exposed at the worst decade") +
  guides(fill = guide_colourbar(title = "Species exposed (%)", title.position = "top",
                                barwidth = 15, title.hjust = 0.5, title.vjust = 2));abrupt.exp.over.gg


# Abruptness of de-exposure after 2100
abrupt.dexp.2100.gg <- ggplot() +
  geom_sf(data=bbox, colour="white", fill="gray88") +
  geom_raster(data=abrupt.dexp.2100.df %>%  filter(layer > 0),mapping=aes(x=x, y=y, fill=layer)) +
  scale_fill_viridis_b(breaks=seq(0,100,10), na.value=NA, option = "D", begin=0.2,end = 1, direction=1, limits=c(0,100)) +
  geom_sf(data=graticules, colour=alpha("white",0.6)) +
  geom_sf(data = countries, fill = NA,
          colour = alpha("black",0.7), size = 0.5)+
  labs(title = "Abruptness of de-exposure events", subtitle = "Percentage of species de-exposed at the best decade") +
  guides(fill = guide_colourbar(title = "Species de-exposed (%)", title.position = "top",
                                barwidth = 15, title.hjust = 0.5, title.vjust = 2));abrupt.dexp.2100.gg

# Abruptness of de-exposure overall
abrupt.dexp.over.gg <- ggplot() +
  geom_sf(data=bbox, colour="white", fill="gray88") +
  geom_raster(data=abrupt.dexp.over.df %>%  filter(layer > 0),mapping=aes(x=x, y=y, fill=layer)) +
  scale_fill_viridis_b(breaks=seq(0,100,10), na.value=NA, option = "D", begin=0.2,end = 1, direction=1, limits=c(0,100)) +
  geom_sf(data=graticules, colour=alpha("white",0.6)) +
  geom_sf(data = countries, fill = NA,
          colour = alpha("black",0.7), size = 0.5)+
  labs(title = "Abruptness of de-exposure", subtitle = "Percentage of species de-exposed at the best decade") +
  guides(fill = guide_colourbar(title = "Species de-exposed (%)", title.position = "top",
                                barwidth = 15, title.hjust = 0.5, title.vjust = 2));abrupt.dexp.over.gg



# Timing of exposure before 2100
timing.exp.2100.gg <- ggplot() +
  geom_sf(data=bbox, colour="white", fill="gray88") +
  geom_raster(data=timing.exp.2100.df ,mapping=aes(x=x, y=y, fill=layer)) +
  scale_fill_viridis_b(breaks=seq(2020,2200,20), na.value=NA, option = "C", begin=0,end = 1, direction = -1,
                       limits = c(2020, 2200)) +
  geom_sf(data=graticules, colour=alpha("white",0.6)) +
  geom_sf(data = countries, fill = NA,
          colour = alpha("black",0.7), size = 0.5)+
  labs(title = "Timing of exposure events", subtitle = "Median year in which exposure occurrs") +
  guides(fill = guide_colourbar(title = "Year", title.position = "top",
                                barwidth = 15, title.hjust = 0.5, title.vjust = 2));timing.exp.2100.gg


# Timing of de-exposure after 2100
timing.dexp.2100.gg <- ggplot() +
  geom_sf(data=bbox, colour="white", fill="gray88") +
  geom_raster(data=timing.dexp.2100.df ,mapping=aes(x=x, y=y, fill=layer)) +
  scale_fill_viridis_b(breaks=seq(2020,2200,20), na.value=NA, option = "C", begin=0,end = 1, direction = -1,
                       limits = c(2020, 2200)) +
  geom_sf(data=graticules, colour=alpha("white",0.6)) +
  geom_sf(data = countries, fill = NA,
          colour = alpha("black",0.7), size = 0.5)+
  labs(title = "Timing of de-exposure events", subtitle = "Median year in which de-exposure occurrs") +
  guides(fill = guide_colourbar(title = "Year", title.position = "top",
                                barwidth = 15, title.hjust = 0.5, title.vjust = 2)); timing.dexp.2100.gg

# Difference in timing 2100
diff.timing.2100.gg <- ggplot() +
  geom_sf(data=bbox, colour="white", fill="gray88") +
  geom_raster(data=diff.timing.2100.df ,mapping=aes(x=x, y=y, fill=layer)) +
  scale_fill_viridis_b(breaks=seq(0,150,25), na.value=NA, option = "D", begin=0,end = 1, direction = 1, limits=c(0,150)) +
  geom_sf(data=graticules, colour=alpha("white",0.6)) +
  geom_sf(data = countries, fill = NA,
          colour = alpha("black",0.7), size = 0.5)+
  labs(title = "Duration of exposure", subtitle = "N. of years between exposure and de-exposure events") +
  guides(fill = guide_colourbar(title = "Years", title.position = "top",
                                barwidth = 15, title.hjust = 0.5, title.vjust = 2)); diff.timing.2100.gg



# Timing of exposure overall
timing.exp.over.gg <- ggplot() +
  geom_sf(data=bbox, colour="white", fill="gray88") +
  geom_raster(data=timing.exp.over.df ,mapping=aes(x=x, y=y, fill=layer)) +
  scale_fill_viridis_b(breaks=seq(2020,2200,20), na.value=NA, option = "H", begin=0.05,end = 0.95, direction = -1,
                       limits = c(2020, 2200)) +
  geom_sf(data=graticules, colour=alpha("white",0.6)) +
  geom_sf(data = countries, fill = NA,
          colour = alpha("black",0.7), size = 0.5)+
  labs(title = "Timing of exposure events", subtitle = "Median year in which exposure occurrs") +
  guides(fill = guide_colourbar(title = "Year", title.position = "top",
                                barwidth = 15, title.hjust = 0.5, title.vjust = 2));timing.exp.over.gg


# Timing of de-exposure overall
timing.dexp.over.gg <- ggplot() +
  geom_sf(data=bbox, colour="white", fill="gray88") +
  geom_raster(data=timing.dexp.over.df ,mapping=aes(x=x, y=y, fill=layer)) +
  scale_fill_viridis_b(breaks=seq(2020,2200,20), na.value=NA, option = "C", begin=0,end = 1, direction = -1,
                       limits = c(2020, 2200)) +
  geom_sf(data=graticules, colour=alpha("white",0.6)) +
  geom_sf(data = countries, fill = NA,
          colour = alpha("black",0.7), size = 0.5)+
  labs(title = "Timing of de-exposure events", subtitle = "Median year in which de-exposure occurrs") +
  guides(fill = guide_colourbar(title = "Year", title.position = "top",
                                barwidth = 15, title.hjust = 0.5, title.vjust = 2)); timing.dexp.over.gg


# Difference in timing 2100
diff.timing.over.gg <- ggplot() +
  geom_sf(data=bbox, colour="white", fill="gray88") +
  geom_raster(data=diff.timing.over.df ,mapping=aes(x=x, y=y, fill=layer)) +
  scale_fill_viridis_b(breaks=seq(0,150,25), na.value=NA, option = "D", begin=0,end = 1, direction = 1, limits=c(0,150)) +
  geom_sf(data=graticules, colour=alpha("white",0.6)) +
  geom_sf(data = countries, fill = NA,
          colour = alpha("black",0.7), size = 0.5)+
  labs(title = "Duration of exposure", subtitle = "N. of years between exposure and de-exposure events") +
  guides(fill = guide_colourbar(title = "Years", title.position = "top",
                                barwidth = 15, title.hjust = 0.5, title.vjust = 2)); diff.timing.over.gg



time.2c.reproj <- projectRaster(time.2c, crs=robin.proj, over=T)
time.4models <- projectRaster(time.2c.nodexp.4models, crs=robin.proj, over=T, method = "ngb")
time.2models <- projectRaster(time.2c.nodexp.2models, crs=robin.proj, over=T, method = "ngb")

time.r <- raster::merge(time.4models, time.2models)


jpeg("../../rangeHorizonsData/Andreas_data/Overshoot/figures/Fig. 4.jpg",
     height = 5, width = 10, units = "in", res=1000)
par(xpd=T, mar=c(4,2,1,0))

negative <- viridis(4, option = "A", begin=0.2, end=0.95)[1]
positive <- viridis(4, option = "A", begin=0.2, end=0.95)[2]
uncert <- viridis(4, option = "A", begin=0.2, end=0.95)[3]
no.return <- viridis(4, option = "A", begin=0.2, end=0.95)[4]


plot(time.2c.reproj, axes=F, box=F, legend=F, breaks=c(-200,0,200), col=c(negative,positive), colNA = "grey88")
plot(time.r, col=c(uncert, no.return), add=T, legend=F)

plot(graticules, col=alpha("white", 0.5), add=T)


plot(countries, add=T, col=NA, border = "black", lwd=1.5)
plot(bgpoly, add=T, col="white", border=NA)


legend(x=-17000000,y=-9800000, legend=c("Shorter duration", "Longer duration", "Uncertain return", "No return"),
       fill = c(negative, positive, uncert, no.return), bty="n", box.cex=c(2.5,1.03), horiz = TRUE, xpd = T,
       text.width = c(0,6500000,6350000,6300000))

dev.off()



fig.empirical<- ggarrange(max.exp.over.gg, timing.exp.over.gg,NULL,NULL, final.exp.gg, abrupt.exp.over.gg,NULL,NULL,dexp.gg, abrupt.dexp.over.gg,
                          ncol=2, nrow=5, labels = c("(a)","(d)",NA,NA,"(b)","(e)",NA,NA,"(c)","(f)"), 
                          font.label = list(size = 14, face = "italic"), 
                          hjust=0.3, vjust = -1,heights = c(1, 0.1, 1,0.1,1)) +
  theme(plot.margin = margin(1,1,1,1, "cm")) 

ggexport(fig.empirical, width = 5500*2, height = 6000*2, res = 500*2,
         filename = "../../rangeHorizonsData/Andreas_data/Overshoot/figures/Fig. 3.jpg")


