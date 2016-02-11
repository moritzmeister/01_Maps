
#Set working directory
setwd("C:/Projekte/Pfister/01_Maps")

install.packages("sp")
install.packages("rgdal")
install.packages("dplyr")
install.packages("plyr")
install.packages("tmap")
install.packages("rgeos")
install.packages("raster")

#spatial data packages

library(sp)
library(ggplot2)
library(rgdal)
library(plyr)
library(dplyr)
library(tmap)
library(raster)

#read data and spatial files
plzdata <- read.csv("out_plz_einw_koord.csv", header = TRUE, sep = ";", quote = "\"", dec = ".")
shops <- read.csv("out_filialen_lat_long.csv", header = TRUE, sep = ";", quote = "\"", dec = ".")
map <- readRDS("CHE_adm0.rds")
ch1 <- readRDS("CHE_adm1.rds")
ch2 <- readRDS("CHE_adm2.rds")
ch3 <- readRDS("CHE_adm3.rds")

#convert plz data to char type
plzdata <- data.frame(lapply(plzdata, as.character), stringsAsFactors=FALSE)

#aggregate plz data to Kanton Level
test <- as.data.frame(tapply(as.numeric(plzdata[,8]), as.factor(paste("CH.", plzdata$KANTON, sep = "")), sum))
test$names <- rownames(test) 
colnames(test) <- c("Summe","HASC_1")

#count kantons
count(as.data.frame(tapply(as.numeric(plzdata[,8]), as.factor(plzdata$ORT), sum)))

#join kpi to spatial data
?left_join
ch1@data <-left_join(ch1@data, test)

#plot
?qtm
qtm(ch1, "Summe", fill.n = 5, fill.style = "cont", fill.palette = "Blues") + qtm(sp1, bubble.col = "Red", bubble.size = 0.6, title = as.character(sp1$shops[, 1])) + qtm(sp1, bubble.col = "Black")


mat <- as.matrix(shops[,2:3])
colnames(mat) <- c("Longitude", "Latitude")
sp1 <- SpatialPointsDataFrame(coords = mat, as.data.frame(shops[,1]),proj4string=CRS(as.character(NA)))






proj4string(sp1) <- NA_character_
# remove CRS information from lnd
proj4string(sp1) <-CRS("+init=epsg:27700")

# assign a new CRS

EPSG <- make_EPSG()
EPSG[grepl("WGS 84$", EPSG$note),]

sp84 <- spTransform(sp1, CRS("+init=epsg:4326"))

?SpatialPointsDataFrame

qtm(sp1)
??proj4string

mat <- as.matrix(plzdata[,12:13])
sp1 <- SpatialPoints(coords = mat)
spdf1 <- SpatialPointsDataFrame(sp1, data = cbind(plzdata[,12:13], plzdata[,8]))
?qtm
plot(spdf1)

?qtm
ggplot(ch2) +  geom_polygon(data=ch2, aes(x=long, y=lat, group=group))
ch3@data[,"ID_3"]
# 
# # get an example shape file
# download.file('http://www.census.gov/geo/cob/bdy/tr/tr00shp/tr48_d00_shp.zip', destfile = 'census.zip')
# 
# # unzip, and load tools
# install.packages("maptools")
# install.packages("gpclib")
# install.packages("qmap")
# install.packages("ggmap")
# install.packages("get_map")
# unzip('CHE_adm_shp.zip')
# library(maptools); library(gpclib); #library(sp);
# gpclibPermit()
# library(qmap)
# 
# # read data into R
# shapefile <- readShapeSpatial('CHE_adm0.shp')
# # convert to a data.frame for use with ggplot2/ggmap and plot
# data <- fortify(shapefile)
# qmap(maptype ='satellite') +
#   geom_polygon(aes(x = long, y = lat, group = group), data = data,
#                colour ='white', fill ='black', alpha = .4, size = .3)
