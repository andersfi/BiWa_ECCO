# figure 1 
working_dir <- "C:\\iSkya\\GitHub\\BIWA_tools\\BiWa_ECCO\\toctime"
setwd(working_dir)
source("Slope_estimation.r")

library(ggplot2)
library(sp)
library(raster)
library(rgeos)


# make sen.data a spatial file (WGS84)
#coordinates <- read.table("S5_position_of_lakes_inLatLong_WGS84.txt",header=T)
plot.data <- sen.data
xy <- plot.data[c("x_coordWGS", "y_coordWGS")]
sen.data.SPDF_latlong <- SpatialPointsDataFrame(coords=xy, data=sen.data, proj4string=CRS("+proj=longlat +datum=WGS84"))
# transform to UTM33 for visualization purposes
sen.data.SPDF <- spTransform(sen.data.SPDF_latlong, CRS("+init=epsg:32633"))


# get underlying map of Norway for illustative purposes
no <- getData('GADM', country="NOR", level=0) #raster data, format SpatialPolygonsDataFrame


no <- gSimplify(no, tol=0.01, topologyPreserve=TRUE)
noUTM33 <- spTransform(no, CRS("+init=epsg:32633"))
spl.no <- list("sp.lines", as(noUTM33, "SpatialLines"))

variables.to.plot <- c("bTOC","bNDVI","SO4","bQsummer","bSdep","bTMsummer")
sen.data.SPDF@data <- sen.data.SPDF@data[variables.to.plot]
names(sen.data.SPDF@data) <- c("TOC","NDVI","SO4","runoff","S_deposition","Temperature")

cut.vec <- quantile(as.matrix(sen.data.SPDF@data),prob=c(seq(from=0,to=1,length.out=20)))
cut.vec <- round(as.numeric(cut.vec),4)

#windows()
spplot(sen.data.SPDF,c("NDVI","S_deposition","runoff","TOC","SO4","Temperature"),
       sp.layout=spl.no,cex=1.8,cuts=cut.vec, scales=list(draw=F),
       col.regions=colorRampPalette(c('blue','gray80','brown'))(4),
       key.space = "right", as.table = TRUE) 
