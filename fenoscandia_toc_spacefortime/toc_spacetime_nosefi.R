########################################################################
#
# TOC predictions from catchment variables (following Larsen et al. 2011)
#
# created by AGF 2015-10-23
#
########################################################################

library(dplyr)
library(data.table)
#Connection using dplyr defining search path; good idea to use read only account for this work
ecco_biwa_db <- src_postgres(dbname="ecco_biwa_db",host="vm-srv-finstad.vm.ntnu.no",
                             user=USER,password=PSW,options = "-c search_path=temporary")
# read table with catchment data and waterchem
data.inn <- tbl(ecco_biwa_db,"output_north_euro_lake_surv_with_catchmentdata")

# NDVI (all months) means
ndvi <- rowMeans(as.data.frame(select(data.inn, starts_with("ndvi"))),na.rm=T)
# NDVI summer means
ndvi.summer <- rowMeans(as.data.frame(data.inn %>% select(starts_with("ndvi_")) %>% select(ends_with("06"),ends_with("07"),
                                                     ends_with("08"),ends_with("09"))),na.rm=T)
# NDVI (all months) maximum
ndvi.max <- apply(as.data.frame(data.inn %>% select(starts_with("ndvi"))),1,max,na.rm=T)



head(garg)
summarise(garg, mean=mean(value), sd=sd(value))
xxx <- as.data.frame(sum(garg,ndvi_1995_11,ndvi_1995_1))
head(xxx)


###########################################################################3
##############################################################################

# library(RPostgreSQL)
# # define username as USER and password as PSW (use the read only account for this script)
# 
# drv <- dbDriver("PostgreSQL")
# ebdb <- dbConnect(drv,dbname="ecco_biwa_db",host="vm-srv-finstad.vm.ntnu.no",
#                   user=USER,password=PSW)
# dbListTables(ebdb)
# 
# setwd("C:\\iSkya\\GitHub\\BIWA_tools\\BiWa_ECCO\\fenoscandia_toc_spacefortime")
# source("get_data.r")
# rs <- dbSendQuery(ebdb, statement = query)
# waterchem <- dbReadTable(ebdb, lakes_for_kim)
# #the resultSet into a data.frame
# df <- fetch(rs, n = -1)   # extract all rows
# dim(df)
# dbListTables(ebdb)
# # ...list column names from particular table
# dbListFields(ebdb, "public.view_north_euro_lake_surv_with_catchmentdata")
# garg <- dbReadTable(conn=ebdb,name="public.bathymetry_predicted",)
