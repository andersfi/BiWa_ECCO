########################################################
# importing and preprosessing data for "softwater_Norway"
# 
# Created by AGF 2015-10-08
#
# script contains to parts A) importing and preprosessing
# and B) extracting slopes
# 
# output is two files: data.std2 = standarized data for mixed models 
# and sen.data containing slopes and associated metrixes
# 
########################################################

##############
#importing data
################

library(repmis)
data.inn <- source_DropboxData("data.to.analyses.csv",key="zz0wbqa8mrsertg", sep=";", header=T)

coordinates <- source_DropboxData("position_of_NO_lakes_inLatLong_WGS84.csv",key="iu65i9z2ud6bffu", sep=";", header=T)
catcstat <- source_DropboxData("catchment_static_niva.lakes.csv",key="5gwu2ajpzypk2h5", sep=";", header=T)

# Subset dataframe on numeric values, standarizing standarize these by sentering on column means dividing on SD, and, exluding missing values. INdex variables are added after scaling.  
data.to.scale <- data.inn[c("lnCa","Ca","tm.summer","sdep_pred","q.summer","ndvi.summer","SO4",
                            "ndvi.summer_lag1","ndvi.summer_lag2","ndvi.summer_lag3","ndvi.summer_lag4",
                            "ndvi.summer_lag5","q.summer_lag1","q.summer_lag2","q.summer_lag3","q.summer_lag4","q.summer_lag5","year","sdep_pred_lag1","sdep_pred_lag2","sdep_pred_lag3","sdep_pred_lag4","sdep_pred_lag5","year")]
data.std <- as.data.frame(scale(data.to.scale, center = TRUE, scale = TRUE))
#data.std$year <- data.inn$year
data.std$vatn_lnr <- data.inn$vatn_lnr
data.std$abs_Ca <- data.inn$Ca
data.std$abs_ndvi.summer <- data.inn$ndvi.summer
data.std$abs_tm.summer <- data.inn$tm.summer
data.std$abs_q.summer <- data.inn$q.summer
data.std$abs_SO4 <- data.inn$SO4
data.std$abs_sdep <- data.inn$sdep_pred
data.std$abs_year <- data.inn$year

# Remove missing values 
data.std.temp <- na.omit(data.std)
data.std2 <- merge(data.std.temp,coordinates)

##############################################
# part B create slopes and associated metrixes
#################################################

# create matrix to store output
data.to_sen <- data.std # change name of dataframe due to lasy programming - recycle of code below
data.to_sen$year <- data.std$abs_year 
data.to_sen <- data.to_sen[data.to_sen$year>1985,]

vatn_lnr_list <- unique(data.to_sen$vatn_lnr)

sen.data <- as.data.frame(matrix(ncol=1,nrow=length(vatn_lnr_list)))
names(sen.data) <- c("vatn_lnr")

library(rkt)
# loop over each lake and create matrix of sen slopes, relative change, and absolute change for different parameters
for(i in 1:length(unique(data.to_sen$vatn_lnr)))
{
  tempdata.inn <- data.to_sen[data.to_sen$vatn_lnr==vatn_lnr_list[i],]
  
  # vatn_lnr
  sen.data$vatn_lnr[i] <- vatn_lnr_list[i]
  
  # Ca
  tempdata <- tempdata.inn[!is.na(tempdata.inn$Ca),]
  rkt.out.temp <- rkt(date=tempdata$year, y=tempdata$Ca)
  sen.data$Ca[i] <- rkt.out.temp$B
  sen.data$Ca_p[i] <- rkt.out.temp$sl
  
  temppred <- predict(lm(abs_Ca~year,data=tempdata),newdata=list(year=c(1986,2013)))
  sen.data$percCa[i] <- ((temppred[2] - temppred[1])/ mean(tempdata$abs_Ca,na.rm=T))*100
  sen.data$absCa[i] <- temppred[2] - temppred[1]
  sen.data$decedalCa[i] <- ((temppred[2] - temppred[1]) / (2013-1986))*10
  sen.data$percDecedalCa[i] <- ((((temppred[2] - temppred[1]) / (2013-1986))*10) / mean(tempdata$abs_Ca,na.rm=T))*100
  sen.data$meanCa[i] <- mean(tempdata$abs_Ca,na.rm=T)
  
  sen.data$bCa[i] <- coef(lm(log(abs_Ca)~year,data=tempdata, na.action = na.omit))[2]

  rkt.out.temp <- rkt(date=tempdata$year, y=tempdata$lnCa)
  sen.data$lnCa[i] <- rkt.out.temp$B
  sen.data$lnCa_p[i] <- rkt.out.temp$sl
 
  # ndvi summer 
  tempdata <- tempdata.inn[!is.na(tempdata.inn$ndvi.summer),]
  rkt.out.temp <- rkt(date=tempdata$year,y=tempdata$ndvi.summer)
  sen.data$ndvi.summer[i] <- rkt.out.temp$B
  sen.data$ndvi.summer_p[i] <- rkt.out.temp$sl
  
  temppred <- predict(lm(abs_ndvi.summer~year,data=tempdata),newdata=list(year=c(1986,2013)))
  sen.data$perc.ndvi.summer[i] <- ((temppred[2] - temppred[1])/ mean(tempdata$abs_ndvi.summer,na.rm=T))*100
  sen.data$abs.ndvi.summer[i] <- temppred[2] - temppred[1]
  sen.data$bNDVI[i] <- coef(lm(log(abs_ndvi.summer)~year,data=tempdata, na.action = na.omit))[2]
  sen.data$meanNDVI[i] <- mean(tempdata$abs_ndvi.summer)
  
  # sdep
  tempdata <- tempdata.inn[!is.na(tempdata.inn$sdep_pred),]
  rkt.out.temp <- rkt(date=tempdata$year,y=tempdata$sdep_pred)
  sen.data$sdep[i] <- rkt.out.temp$B
  sen.data$sdep_p[i] <- rkt.out.temp$sl
  
  temppred <- predict(lm(abs_sdep~year,data=tempdata),newdata=list(year=c(1986,2013)))
  sen.data$perc.sdep[i] <- ((temppred[2] - temppred[1])/ mean(tempdata$abs_sdep,na.rm=T))*100
  sen.data$abs.sdep[i] <- temppred[2] - temppred[1]
  sen.data$bSdep[i] <- coef(lm(log(abs_sdep)~year,data=tempdata, na.action = na.omit))[2]
  
  # SO4
  tempdata <- tempdata.inn[!is.na(tempdata.inn$SO4),]
  rkt.out.temp <- rkt(date=tempdata$year, y=tempdata$SO4)
  sen.data$SO4[i] <- rkt.out.temp$B
  sen.data$SO4_p[i] <- rkt.out.temp$sl
  
  temppred <- predict(lm(abs_SO4~year,data=tempdata),newdata=list(year=c(1986,2013)))
  sen.data$perc.SO4[i] <- ((temppred[2] - temppred[1])/ mean(tempdata$abs_SO4,na.rm=T))*100
  sen.data$abs.SO4[i] <- temppred[2] - temppred[1]
  sen.data$bSO4[i] <- coef(lm(log(abs_SO4)~year,data=tempdata, na.action = na.omit))[2]
  
  # q.summer
  tempdata <- tempdata.inn[!is.na(tempdata.inn$q.summer),]
  rkt.out.temp <- rkt(date=tempdata$year, y=tempdata$q.summer)
  sen.data$q.summer[i] <- rkt.out.temp$B
  sen.data$q.summer_p[i] <- rkt.out.temp$sl
  
  temppred <- predict(lm(abs_q.summer~year,data=tempdata),newdata=list(year=c(1986,2013)))
  sen.data$perc.q.summer[i] <- ((temppred[2] - temppred[1])/ mean(tempdata$abs_q.summer,na.rm=T))*100
  sen.data$abs.q.summer[i] <- temppred[2] - temppred[1]
  sen.data$bQsummer[i] <- coef(lm(log(abs_q.summer)~year,data=tempdata, na.action = na.omit))[2]
  
  # tm.summer
  tempdata <- tempdata.inn[!is.na(tempdata.inn$tm.summer),]
  rkt.out.temp <- rkt(date=tempdata$year, y=tempdata$tm.summer)
  sen.data$tm.summer[i] <- rkt.out.temp$B 
  sen.data$tm.summer_p[i] <- rkt.out.temp$sl
  
  temppred <- predict(lm(abs_tm.summer~year,data=tempdata),newdata=list(year=c(1986,2013)))
  sen.data$perc.tm.summer[i] <- ((temppred[2] - temppred[1])/ mean(tempdata$abs_tm.summer,na.rm=T))*100
  sen.data$abs.tm.summer[i] <- temppred[2] - temppred[1]
  sen.data$bTMsummer[i] <- coef(lm(log(abs_tm.summer)~year,data=tempdata, na.action = na.omit))[2]
  
  # Ca - NDVI slopes
  tempdata <- tempdata.inn[!is.na(tempdata.inn$ndvi.summer) & !is.na(tempdata.inn$Ca),]
  sen.data$bCa.ndvi[i] <- coef(lm(log(abs_Ca)~log(abs_ndvi.summer),data=tempdata, na.action = na.omit))[2]
  
}

sen.data.temp <- merge(sen.data,coordinates)
sen.data <- merge(sen.data.temp,catcstat)

