# script creates list of study lakes with some information 

setwd(working_dir)
datainn <- read.table("data.to.analyses.txt",header=T)
coordinates <- read.table("S5_position_of_lakes_inLatLong_WGS84.txt",header=T)
catcstat <- read.table("catchment_static_niva.lakes.txt",header=T)
datainn2 <- merge(datainn,coordinates)
datainn2b <- merge(datainn2,catcstat)
datainn3 <- datainn2b[datainn2b$year>1986,]

table <- as.data.frame(aggregate(datainn3$vatn_lnr,by=list(datainn3$vatn_lnr),FUN=mean)$x)
names(table) <- c("LakeID")
table$Latitude <- aggregate(datainn3$y_coordWGS,by=list(datainn3$vatn_lnr),FUN=mean)$x
table$Longitude <- aggregate(datainn3$x_coordWGS,by=list(datainn3$vatn_lnr),FUN=mean)$x
table$lakeArea <- round(aggregate(datainn3$lake_area,by=list(datainn3$vatn_lnr),FUN=mean)$x,digits=2)
table$catchmentArea <- round((aggregate(datainn3$catch_area,by=list(datainn3$vatn_lnr),FUN=mean)$x) / 1000000,digits = 2)
table$TOC <- round(aggregate(datainn3$TOC,by=list(datainn3$vatn_lnr),FUN=mean,na.rm=T)$x,digits=2)
listOfLakes <- table