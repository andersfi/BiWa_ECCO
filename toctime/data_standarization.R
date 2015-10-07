# data standarization 

setwd(working_dir)

# importing data
data.inn <- read.table("data.to.analyses.txt",header=T)
coordinates <- read.table("S5_position_of_lakes_inLatLong_WGS84.txt",header=T)

# Subset dataframe on numeric values, standarizing standarize these by sentering on column means dividing on SD, and, exluding missing values. INdex variables are added after scaling.  
data.to.scale <- data.inn[c("lnTOC","tm.summer","sdep_pred","q.summer","ndvi.summer","SO4",
                            "ndvi.summer_lag1","ndvi.summer_lag2","ndvi.summer_lag3","ndvi.summer_lag4",
                            "ndvi.summer_lag5","q.summer_lag1","q.summer_lag2","q.summer_lag3","q.summer_lag4","q.summer_lag5","year")]
data.std <- as.data.frame(scale(data.to.scale, center = TRUE, scale = TRUE))
#data.std$year <- data.inn$year
data.std$vatn_lnr <- data.inn$vatn_lnr

# Remove missing values 
data.std.temp <- na.omit(data.std)
data.std2 <- merge(data.std.temp,coordinates)
