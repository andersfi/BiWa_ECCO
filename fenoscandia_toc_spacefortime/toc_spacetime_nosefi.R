########################################################################
#
# TOC predictions from catchment variables (following Larsen et al. 2011)
#
# created by AGF 2015-10-23
#
########################################################################

library(dplyr)

#############################################################################
#
# Connecting using RPostgreSQL - slower, but can read views (dplyr appear to not be able)
# Replace USER with "yourusername" and PSW with "yourpassword".
#
##############################################################################

library(RPostgreSQL)
# # define username as USER and password as PSW (use the read only account for this script)
# 
drv <- dbDriver("PostgreSQL")
ebdb <- dbConnect(drv,dbname="ecco_biwa_db",host="vm-srv-finstad.vm.ntnu.no",
                   user=USER,password=PSW)
dbListTables(ebdb)
waterchem <- dbReadTable(ebdb, "view_output_north_euro_lake_surv_with_catchmentdata")
colnames(waterchem)
dim(waterchem)
data.raw <- as.data.frame(waterchem)


# select and modify innput variables (note to myself; unlist dplyr output, some functions spit out dataframes)
library(dplyr)

data.analys <-select(data.raw,ebint) 

# NDVI (mean all year, mean summer and max)
data.analys$ndvi <- rowMeans(as.data.frame(select(data.raw, starts_with("ndvi"))),na.rm=T)
data.analys$ndvi.summer <- rowMeans(as.data.frame(data.raw %>% select(starts_with("ndvi_")) %>% 
                                                    select(ends_with("06"),ends_with("07"),
                                                           ends_with("08"),ends_with("09"))),na.rm=T)
data.analys$ndvi.max <- apply(as.data.frame(data.raw %>% select(starts_with("ndvi"))),1,max,na.rm=T)

# Corine: Codes to CORINE landcover http://sia.eionet.europa.eu/CLC2000/classes
# Proportin of land-cover classes
data.analys$catch_area_m <- unlist(select(data.raw,catchment_area_m))

data.analys$moors <- unlist(select(data.raw,corine2000_322))
data.analys$bogs <- rowSums(select(data.raw,starts_with("corine2000_41")))  
data.analys$waterbody <- rowSums((select(data.raw,starts_with("corine2000_5")))) 
data.analys$forest <- rowSums((select(data.raw,starts_with("corine2000_31"))))
data.analys$corine_area <- rowSums((select(data.raw,starts_with("corine2000")))) 
data.analys$bogs_prop <- data.analys$bogs/data.analys$corine_area
data.analys$forest_prop <- data.analys$forest/data.analys$corine_area
data.analys$water_prop <- data.analys$waterbody/data.analys$corine_area
data.analys$moors_prop <- data.analys$moors/data.analys$corine_area

data.analys$forest <- rowSums((select(data.raw,starts_with("corine2000_31"))))

data.analys$airtemp_mjja <- unlist(select(data.raw,airtemp_dmi_2001_2010_mjja))
data.analys$airtemp_jj <- unlist(select(data.raw,airtemp_dmi_2001_2010_jj))
data.analys$runoff_mjja <- unlist(select(data.raw,runoff_dmi_2001_2010_mjja))
data.analys$runoff_jj <- unlist(select(data.raw,runoff_dmi_2001_2010_jj))

# others 
data.analys$slope <- unlist(select(data.raw,slope_median))
data.analys$nation <- unlist(select(data.raw,nation))
data.analys$latitude <- unlist(select(data.raw,latitude))
data.analys$longitude <- unlist(select(data.raw,longitude))
data.analys$sdep <- unlist(select(data.raw,s_dep_g_s_m2_a))
data.analys$flowacu <- unlist(select(data.raw,flow_accum_median))

# TOC
data.analys$toc <- unlist(select(data.raw,toc_mg_l))

data.to.analyses <- select(data.analys,toc,sdep,moors_prop,flowacu,slope,ndvi,ndvi.summer,ndvi.max,bogs_prop,forest_prop,water_prop,airtemp_mjja,runoff_jj,runoff_mjja)
data.to.analyses <- data.frame(data.to.analyses)
data.std <- as.data.frame(scale(data.to.analyses))
data.std$ebint <- data.analys$ebint
data.std$nation <- data.analys$nation
data.std$latitude <- data.analys$latitude
data.std$longitude <- data.analys$longitude



############ visualize data ##########################

# scatterplots
par(mfrow=c(4,3))
plot(data.to.analyses$sdep,data.to.analyses$toc,xlab="Sdep",ylab="lnTOC")
plot(data.to.analyses$ndvi,data.to.analyses$toc,xlab="ndvi",ylab="lnTOC")
plot(data.to.analyses$slope,data.to.analyses$toc,xlab="slope",ylab="lnTOC")
plot(data.to.analyses$ndvi.summer,data.to.analyses$toc,xlab="ndvi.summer",ylab="lnTOC")
plot(data.to.analyses$ndvi.max,data.to.analyses$toc,xlab="ndvi.max",ylab="lnTOC")
plot(data.to.analyses$bogs_prop,data.to.analyses$toc ,xlab="Bogs",ylab="lnTOC")
plot(data.to.analyses$forest_prop,data.to.analyses$toc ,xlab="forest",ylab="lnTOC")
plot(data.to.analyses$runoff_mjja,data.to.analyses$toc ,xlab="runoff_mjja",ylab="lnTOC")
plot(data.to.analyses$runoff_jj,data.to.analyses$toc ,xlab="runoff_jj",ylab="lnTOC")
plot(data.to.analyses$airtemp_mjja,data.to.analyses$toc ,xlab="airtemp_mjja",ylab="lnTOC")
plot(data.to.analyses$water_prop,data.to.analyses$toc ,xlab="waters",ylab="lnTOC")



# First modelling attempths following larsen et al. 2011
m1 <- lm(log(data.to.analyses$toc)~ndvi.summer+runnoff+bogs_prop+forest_prop+
           water_prop+slope,
         na.action=na.exclude,data=data.std)
summary(m1)

library(mgcv)
gam1 <- lm(log(data.to.analyses$toc)~ndvi.summer+runnoff+bogs_prop+forest_prop+
           water_prop+slope+s(latitude,longitude),
         na.action=na.exclude,data=data.std)
summary(gam1)

# ############################################################################################
# #
# # Postgresql connection using dplyr 
# #
# ###############################################################################################
# ecco_biwa_db <- src_postgres(dbname="ecco_biwa_db",host="vm-srv-finstad.vm.ntnu.no",
#                              user=USER,password=PSW,options = "-c search_path=temporary")
# # read table with catchment data and waterchem
# data.inn <- tbl(ecco_biwa_db,"output_north_euro_lake_surv_with_catchmentdata")
# data.raw <- tbl_dt(data.inn)
