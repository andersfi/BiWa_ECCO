# figure 1 
## working_dir <- "C:\\iSkya\\GitHub\\BIWA_tools\\BiWa_ECCO\\toctime"
## setwd(working_dir)

require(ggplot2)
require(sp)
require(raster)
require(rgeos)
require(maptools)

# make sen.data a spatial file (WGS84)
#coordinates <- read.table("S5_position_of_lakes_inLatLong_WGS84.txt",header=T)
plot.data <- sen.data
xy <- plot.data[c("x_coordWGS", "y_coordWGS")]
sen.data.SPDF_latlong <-
  SpatialPointsDataFrame(coords=xy, data=sen.data,
                         proj4string=CRS("+proj=longlat +datum=WGS84"))
# transform to UTM33 for visualization purposes
sen.data.SPDF <- spTransform(sen.data.SPDF_latlong, CRS("+init=epsg:32633"))


# get underlying map of Norway for illustative purposes
no <- getData('GADM', country="NOR", level=0)
##raster data, format SpatialPolygonsDataFrame

# Norway map
no <- gSimplify(no, tol=0.01, topologyPreserve=TRUE)
noUTM33 <- spTransform(no, CRS("+init=epsg:32633"))
spl.no <- list("sp.lines", as(noUTM33, "SpatialLines"))




###### plot function 

# function prints png file to disk
figure <- function(columns,units,figname){
 
# colours for positive and negative slopes
cols <- c('blue4', 'blue2', 'lightblue', 'pink', 'red1', 'red4')  
png(figname, height=3000, width=8000, pointsize=120)

for (j in 1:length(columns)) {
  i <- columns[j]

  # spacing between the panels
  if (j == 1) {
    par(fig=c(0.32 * (j - 1), 0.32 * (j) + 0.1, 0.1, 1.6) * 0.58,
        mar=c(0, 0, 2, 0), bg=NA)
  } else {
    par(fig=c(0.32 * (j - 1), 0.32 * (j) + 0.1, 0.1, 1.6) * 0.58,
        mar=c(0, 0, 2, 0), bg=NA, new=TRUE)
  }
  
  # define categories (3 positive, 3 negative)
pr <- pretty(c(sen.data.SPDF@data[[i]],
        -1 * sen.data.SPDF@data[[i]]), n = 6)

  # Norway map
  plot(noUTM33, lwd=5)

  # Catchment and Lake slopes
  plot(sen.data.SPDF[, i], 
       pch=21, col='black', lwd=14, cex=1.0, 
       bg=cols[cut(sen.data.SPDF@data[[i]], pr)[1:70]],
       add=TRUE)

  # panel heads for manually checking
  title(names(sen.data)[i])
  
  legend(400000, 7000000, ## this position seems to work well
         legend = sprintf('%s to %s', pr[6:1], pr[7:2]),
         pch=21, pt.lwd=14, pt.cex=1.0,
         col='black', pt.bg=cols[6:1], bty='n', cex = 0.7,
         title=sprintf('%s', units[j]))
}
dev.off()
}
