# result section of "softwater_norway"

#setwd("C:\\Users\\andersfi\\Dropbox\\a_fishing\\biwa_ecco\\ca-time\\manuscript\\Rcode_and_data")
source("importing_and_preprossesing_data.r")

############################
# figure 1 
#############################
library(ggplot2)
library(sp)
library(raster)
library(rgeos)

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

variables.to.plot <- c("meanNDVI","bNDVI","Ca","bCa.ndvi")
sen.data.SPDF@data <- sen.data.SPDF@data[variables.to.plot]
names(sen.data.SPDF@data) <- c("meanNDVI","trendNDVI","trendCa","Ca.NDVI")

cut.vec <- quantile(as.matrix(sen.data.SPDF@data),prob=c(seq(from=0,to=1,length.out=10)))
cut.vec <- round(as.numeric(cut.vec),5)

#windows()
fig1 <- spplot(sen.data.SPDF,c("meanNDVI","trendNDVI","trendCa","Ca.NDVI"),
       sp.layout=spl.no,cex=1.8,cuts=cut.vec, scales=list(draw=F),
       col.regions=colorRampPalette(c('brown','gray80','blue'))(5),
       key.space = "right", as.table = TRUE) 


# number of lakes with significant negative Ca trend
n.lakes.Ca.decr <- round(length((sen.data$Ca[sen.data$Ca<0])),digits=0)

# decadal change in abs Ca
percDecadalCa <- round(mean(sen.data$percDecedalCa[sen.data$Ca<0]),digits=3)
decadalCa <- mean(sen.data$decedalCa,digits=3)

############################################
# list of study lakes with changes in Ca
###########################################

table <- as.data.frame(aggregate(sen.data$vatn_lnr,by=list(sen.data$vatn_lnr),FUN=mean)$x)
names(table) <- c("LakeID")
table$Latitude <- aggregate(sen.data$y_coordWGS,by=list(sen.data$vatn_lnr),FUN=mean)$x
table$Longitude <- aggregate(sen.data$x_coordWGS,by=list(sen.data$vatn_lnr),FUN=mean)$x
table$lakeArea <- round(aggregate(sen.data$lake_area,by=list(sen.data$vatn_lnr),FUN=mean)$x,digits=2)
table$catchmentArea <- round((aggregate(sen.data$catch_area,by=list(sen.data$vatn_lnr),FUN=mean)$x) / 1000000,digits = 2)
table$Ca <- round(aggregate(sen.data$meanCa,by=list(sen.data$vatn_lnr),FUN=mean,na.rm=T)$x,digits=2)
table$deltaCaAbs <- round(aggregate(sen.data$decedalCa,by=list(sen.data$vatn_lnr),FUN=mean,na.rm=T)$x,digits=2)
table$deltaCaperc <- round(aggregate(sen.data$percDecedalCa,by=list(sen.data$vatn_lnr),FUN=mean,na.rm=T)$x,digits=2)
listOfLakes <- table

