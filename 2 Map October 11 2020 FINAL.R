#Analysis section
setwd("C:/RobsR/survival/Florida 2019_2020")
#AllData3=read.csv("AlldataFL3_Year_revised.csv")

AllData3=read.csv("Florida_Final.csv")

#Make a map
longlat<- '+proj=longlat +ellps=WGS84 +no_defs'
eqc<- "+proj=eqc +lat_ts=0 +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
utm <- "+proj=utm +zone=17 +units=km +datum=WGS84"
library(rworldmap)
library(maptools)
library(raster)
library(rworldxtra)
wholeworld<-getMap(resolution="high")
FLOutplantMap<-crop(wholeworld, c(-83.25548, -79.8523,24.23897, 27.4085)) #ignore warning #message that they have different projections
#plot(FLOutplantMap)
FLOutplantMap<-spTransform(FLOutplantMap, longlat)


# outplant sites figure
par(mar=c(1,1,1,1))
plot(FLOutplantMap, col = "gray", ylim = c(24.3,26.3), xlim = c(-83.2,-79.9))
#plot(regionslonglat, add=T)
AllData3$Agency=as.factor(AllData3$Agency)
color_easy = c("black", "red", "green", "blue", "cyan", "hotpink")[AllData3$Agency]
points(AllData3$Longitude, AllData3$Latitude, col = color_easy, pch=20)

#points(AllData3$Longitude, AllData3$Latitude, col = AllData3$Agency, pch=20)
#points(AllData3$Longitude, AllData3$Latitude, col = "black")

box(which="plot")
axis(1, at=seq(-83,-80,1), NA, cex.axis=.7, font=1, tck=.02)
axis(2, at=c(24.5,25, 25.5), NA, cex.axis=.7, font=1, tck=.02)
axis(3, at=seq(-83,-80,1), NA, cex.axis=.7, font=1, tck=.02)
axis(4, at=c(24.5,25, 25.5), NA, cex.axis=.7, font=1, tck=.02)
text(c(-81, -80), 24.3, c("81°W", "80°W"))
text(-83.32,24.5,"24.5°N")
text(-83.32,25.5,"25.5°N")
legend( "topleft",title="Program", legend=c("TNC", "Mote", "FWC", "CRF", "UM", "NSU"),
        col=c("cyan", "green", "red", "black", "hotpink", "blue"),  pch=20)

#Revised
maps::map.scale(x= -83, y= 25.3, relwidth=0.2, ratio=FALSE)
arrows(-82.5, 26, -82.5, 26.2)
text(-82.5, 26.26, "N")


#
#Include subregions

library(rgdal)
#read in subregions shapefile and unified benthic habitat shapefile
#project both to longlat
subregions <- readOGR("./Data","Unified_Florida_Reef_Map_v2.0_Regions")
regionslonglat<- spTransform(subregions, longlat)
#reefTract <- readOGR("./Data","UnifiedFloridaReefTract_poly")
#reefTractlonglat<- spTransform(reefTract, longlat)
plot(regionslonglat, add=TRUE)
text(-82.85, 24.47, "Dry Tortugas")
text(-82.25, 24.47, "Marquesas")
text(-81.38, 24.4, "Lower Florida Keys")
text(-80.58, 24.6, "Middle Florida Keys")
text(-79.97, 24.95, "Upper Florida Keys")
text(-80.6, 25.6, "Biscayne Bay")
text(-80.55, 26, "Broward-Miami")
text(-80.8, 25.05, "Florida Bay")
text(-82.5, 25.5, "Gulf of Mexico", cex=1.2)
text(-80.9, 26.18, "Florida", cex=1.8)

