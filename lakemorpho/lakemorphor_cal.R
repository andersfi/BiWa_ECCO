require(gstat)
require(rgdal)
require(rgeos)
require(raster)
# install.packages("lakemorpho")
require(lakemorpho)

### corrected lakemorpho calculation

ebint<-numeric()

LMmaxdepth<-numeric()
LMvol<-numeric()
# fetch<-numeric()
LMmaxlength<-numeric()
LMmaxwidth<-numeric()
LMmeandepth<-numeric()
LMmeanwidth<-numeric()
LMshorelinedeve<-numeric()
LMshorelineleng<-numeric()
LMmaxdistance <- numeric()

eccobiwa<-readOGR("C:/Hong Yang/Work01/GIS_files_from_NINA", "ecco-biwa_lakes_v0.1_UTM33")
DEM<- raster ("C:/Hong Yang/Work01/GIS_files_from_NINA/NOSEFI_DEM_25m.tif")
# Slope<-raster("C:/Hong Yang/Work01/GIS_files_from_NINA/NOSEFI_slope_25m.tif")

lakepoly<-readOGR('.', 'bath_lakes_Aug_2015_polygons')
# change lakepoly projection to same as DEM
lakepoly2 <- spTransform(lakepoly, CRS(proj4string(DEM)))
eccobiwa<-spTransform(lakepoly, CRS(proj4string(lakepoly2)))
# catch<-readOGR(dsn="C:/Hong Yang/Work01/GIS_files_from_NINA", layer="ecco_biwa_lake_catchment")
## catchment polygon with bathmetry data
# catch<-readOGR(dsn="C:/Hong Yang/Work01/GIS_files_from_NINA", layer="catch_bath")

for (i in 1:length(lakepoly2@data$ebint))
  #   for (i in 600:1200)
  # for (i in 694:694)  
  
{
  #   sublake<-lakepoly2[lakepoly2@data$ebint==lakepoly2@data$ebint[i],]
  ## subcat <-catch[catch@data$ebint==lakepoly2@data$ebint[i],]
  ## temp.catzone <- gDifference(subcat, sublake)
  
  sublake<-subset(eccobiwa, ebint==lakepoly2[["ebint"]][i])
  
  ebint[i]<-lakepoly2@data$ebint[i]
  
  cat(sprintf('%4i %8i', i, ebint[i]))
  
  area <- gArea(sublake)
  cat(sprintf(' lake area %4.1f km2', area * 1e-6))
  if (area < 8101) {
    cat(' small lake skipping\n')
  } else {
    time0 <- proc.time()
    
    buf <- gBuffer(sublake, width=max(250, sqrt(area/pi)))
    
    cat.dem<-crop(DEM, buf)
    ## cat.dem.max[i] <- extract(cat.dem, temp.catzone,fun=max, na.rm=TRUE)
    ## cat.dem.mean[i]  <- extract(cat.dem, temp.catzone,fun=mean, na.rm=TRUE)
    ## cat.dem.median[i] <- extract(cat.dem, temp.catzone,fun=median, na.rm=TRUE)
    ## cat.dem.range [i]<- extract(cat.dem, temp.catzone,fun=myrange,na.rm=TRUE)
    
    ## cat.slope<-crop(Slope, subcat)
    ## cat.slope.max[i]<- extract(cat.slope, temp.catzone, fun=max, na.rm=TRUE)
    ## cat.slope.mean[i]<-extract(cat.slope, temp.catzone, fun=mean, na.rm=TRUE)
    ## cat.slope.median[i]<-extract(cat.slope, temp.catzone, fun=median, na.rm=TRUE)
    ## cat.slope.sd[i]<-extract(cat.slope, temp.catzone, fun=sd, na.rm=TRUE)
    
    inputLM<-lakeSurroundTopo(sublake, cat.dem)
    LMmaxdepth[i]<-lakeMaxDepth(inputLM)
    LMvol[i]<-lakeVolume(inputLM)
    ## fetch0[i]<-lakeFetch(inputLM,0)
    ## fetch45[i]<-lakeFetch(inputLM,45)
    ## fetch90[i]<-lakeFetch(inputLM,90)
    ## fetch135[i]<-lakeFetch(inputLM,135)
    LMmaxlength[i]<-lakeMaxLength(inputLM, 50)
    LMmaxwidth[i]<-lakeMaxWidth(inputLM, 50)
    LMmeandepth[i]<-lakeMeanDepth(inputLM)
    LMmeanwidth[i]<-lakeMeanWidth(inputLM)
    LMshorelinedeve[i]<-lakeShorelineDevelopment(inputLM)
    LMshorelineleng[i]<-lakeShorelineLength(inputLM)
    LMmaxdistance[i] <- inputLM[['lakeDistance']]@data@max
    time1 <- proc.time()
    cat(sprintf(' took %4.1f sec\n',
                time1[1] + time1[2] - time0[1] - time0[2]))
  }
}

catch.res<-data.frame(ebint, 
                      ## cat.dem.max, cat.dem.mean, cat.dem.median, cat.dem.range, 
                      ## cat.slope.max, cat.slope.mean, cat.slope.median,
                      ## cat.slope.sd,
                      LMmaxdepth, LMvol, ##LMfetch,
                      LMmaxlength, LMmaxwidth, LMmeandepth, LMmeanwidth,
                      LMshorelinedeve, LMshorelineleng,
                      LMmaxdistance)

write.csv(catch.res, "lakemorphstats.csv")