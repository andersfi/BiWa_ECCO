# result section of "softwater_norway"

setwd("C:\\iSkya\\GitHub\\BIWA_tools\\BiWa_ECCO\\softwater_norway")
source("importing_and_preprossesing_data.r")

############################
# figures  
#############################
library(ggplot2)
library(sp)
library(raster)
library(rgeos)
# get underlying map of Norway for illustative purposes from http://www.gadm.org/
no <- readRDS("NOR_adm0.rds") 
no <- gSimplify(no, tol=0.01, topologyPreserve=TRUE)
noUTM33 <- spTransform(no, CRS("+init=epsg:32633"))
spl.no <- list("sp.lines", as(noUTM33, "SpatialLines"))

# fig 1 mean Ca, abs Change in Ca #################################################
plot.data <- sen.data
xy <- plot.data[c("x_coordWGS", "y_coordWGS")]
sen.data.SPDF_latlong <- SpatialPointsDataFrame(coords=xy, data=sen.data, proj4string=CRS("+proj=longlat +datum=WGS84"))
sen.data.SPDF <- spTransform(sen.data.SPDF_latlong, CRS("+init=epsg:32633")) # transform to UTM33 for visualization purposes


variables.to.plot <- c("meanCa","decedalCa")
sen.data.SPDF@data <- sen.data.SPDF@data[variables.to.plot]
names(sen.data.SPDF@data) <- c("meanCa","Change_Ca_10yr")

cut.vec <- quantile(as.matrix(sen.data.SPDF@data),prob=c(seq(from=0,to=1,length.out=20)))
cut.vec <- round(as.numeric(cut.vec),5)

fig1 <- spplot(sen.data.SPDF,c("meanCa","Change_Ca_10yr"),
               sp.layout=spl.no,cex=1.8,cuts=cut.vec, scales=list(draw=F),
               col.regions=colorRampPalette(c('brown','gray80','blue'))(5),
               key.space = "right", as.table = TRUE) 


## fig 3 ############################################################
sen.data$Ca_2013_below_critical_0.5 <- ifelse(sen.data$Ca2013<0.5,-1,1) # critica limit for D. Magna from http://plankt.oxfordjournals.org/content/22/3/553.full
sen.data$Ca_2013_below_critical_1 <- ifelse(sen.data$Ca2013<1,-1,1) # critica limit different
sen.data$Ca_change_to_below_critical_0.5 <- ifelse(sen.data$Ca2013<0.5 & sen.data$Ca1986>0.5,-1,1) # critica limit for D. Magna from http://plankt.oxfordjournals.org/content/22/3/553.full
sen.data$Ca_change_to_below_critical_1 <- ifelse(sen.data$Ca2013<1 & sen.data$Ca1986>1,-1,1) # critica limit different

plot.data <- sen.data
xy <- plot.data[c("x_coordWGS", "y_coordWGS")]
sen.data.SPDF_latlong <- SpatialPointsDataFrame(coords=xy, data=sen.data, proj4string=CRS("+proj=longlat +datum=WGS84"))
# transform to UTM33 for visualization purposes
sen.data.SPDF <- spTransform(sen.data.SPDF_latlong, CRS("+init=epsg:32633"))


variables.to.plot <- c("Ca_2013_below_critical_0.5","Ca_2013_below_critical_1",
                       "Ca_change_to_below_critical_0.5","Ca_change_to_below_critical_1")
sen.data.SPDF@data <- sen.data.SPDF@data[variables.to.plot]
names(sen.data.SPDF@data) <- c("Ca_2013_below_critical_0.5","Ca_2013_below_critical_1",
                               "Ca_change_to_below_critical_0.5","Ca_change_to_below_critical_1")

cut.vec <- quantile(as.matrix(sen.data.SPDF@data),prob=c(seq(from=0,to=1,length.out=1)))
cut.vec <- c(-1,0,1)

fig3 <- spplot(sen.data.SPDF,c("Ca_2013_below_critical_0.5","Ca_2013_below_critical_1",
                               "Ca_change_to_below_critical_0.5","Ca_change_to_below_critical_1"),
       sp.layout=spl.no,cex=1,cuts=cut.vec, scales=list(draw=F),
       col.regions=colorRampPalette(c('brown','gray80','blue'))(5),
       key.space = "right", as.table = TRUE) 
fig3
## fig 2 ############################################################
plot.data <- sen.data
xy <- plot.data[c("x_coordWGS", "y_coordWGS")]
sen.data.SPDF_latlong <- SpatialPointsDataFrame(coords=xy, data=sen.data, proj4string=CRS("+proj=longlat +datum=WGS84"))
# transform to UTM33 for visualization purposes
sen.data.SPDF <- spTransform(sen.data.SPDF_latlong, CRS("+init=epsg:32633"))


variables.to.plot <- c("bCa","bNDVI","bSdep","bQsummer","bCa.ndvi","bTMsummer")
variables.to.plot <- c("Ca","ndvi.summer","sdep","q.summer","tm.summer")

sen.data.SPDF@data <- sen.data.SPDF@data[variables.to.plot]
names(sen.data.SPDF@data) <- c("slope_Ca","slope_NDVI","slope_S_dep.","slope_Q","slope_Tempr.")

cut.vec <- quantile(as.matrix(sen.data.SPDF@data),prob=c(seq(from=0,to=1,length.out=10)))
cut.vec <- round(as.numeric(cut.vec),5)

fig2 <- spplot(sen.data.SPDF,c("slope_Ca","slope_NDVI","slope_S_dep.","slope_Q","slope_Tempr."),
               sp.layout=spl.no,cex=1.8,cuts=cut.vec, scales=list(draw=F),
               col.regions=colorRampPalette(c('brown','gray80','blue'))(5),
               key.space = "right", as.table = TRUE) 

################### some figure related metrixs ######################################################


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

########################################################################################
# Mixed effect modelling Ca using S dep as predictor as shown in main text of manuscript 
########################################################################################

# comparing random structure
library(nlme)

f1 <- formula(lnCa ~ ndvi.summer_lag4 + q.summer + sdep_pred_lag4 + tm.summer+year) 

m1 <- gls(f1, data = data.std2, na.action = na.fail,method="REML") 
m2 <- lme(f1, data = data.std2, random= ~1 | as.factor(vatn_lnr),na.action = na.fail,method="REML") 
m3 <- lme(f1, data = data.std2, random= ~year | as.factor(vatn_lnr),na.action = na.fail,method="REML") 
m4 <- lme(f1, data = data.std2, random= ~ 1 | as.factor(vatn_lnr), 
          corAR1(form = ~ year | as.factor(vatn_lnr)),method="REML",na.action = na.fail) 
#m5 <- lme(f1, data = data.std2, random= ~ year | as.factor(vatn_lnr), 
#          corAR1(form = ~ year | as.factor(vatn_lnr)),method="REML",na.action = na.fail) # does not convergre, ditched


AIC(m1,m2,m3,m4)

# selecting fixed structure
library(nlme)
library(MuMIn)
library(relaimpo)

f1 <- formula(lnCa ~ ndvi.summer_lag4 + q.summer + sdep_pred_lag5 + tm.summer + year) 

# rerunning best random structure with ML estimator
m6 <- lme(f1, data = data.std2, random= ~ 1 | as.factor(vatn_lnr),
          method="ML",na.action = na.fail) 
#avgmod.95p <- m6
dregde_mod <- dredge(m6,rank="AIC") 
dregdetable <- dregde_mod[1:8]
modelsummary <- summary(m6)


##########################################################################################
# Mixed effect modelling Ca  subsituting SO4 deposition for S deposition  as predictor 
#########################################################################################

# comparing random structure
library(nlme)

f1 <- formula(lnCa ~ ndvi.summer_lag4 + q.summer + SO4 + tm.summer+year) 

m1 <- gls(f1, data = data.std2, na.action = na.fail,method="REML") 
m2 <- lme(f1, data = data.std2, random= ~1 | as.factor(vatn_lnr),na.action = na.fail,method="REML") 
m3 <- lme(f1, data = data.std2, random= ~year | as.factor(vatn_lnr),na.action = na.fail,method="REML") 

m4 <- lme(f1, data = data.std2, random= ~ 1 | as.factor(vatn_lnr), 
          corAR1(form = ~ year | as.factor(vatn_lnr)),method="REML",na.action = na.fail) 
#m5 <- lme(f1, data = data.std2, random= ~ year | as.factor(vatn_lnr), 
#          corAR1(form = ~ year | as.factor(vatn_lnr)),method="REML",na.action = na.fail) # does not convergre, ditched

AIC(m1,m2,m3,m4)

# selecting fixed structure

f1 <- formula(lnCa ~ ndvi.summer_lag4 + q.summer + SO4 + tm.summer + year) 

# rerunning best random structure with ML estimator
m6 <- lme(f1, data = data.std2, random= ~ 1 | as.factor(vatn_lnr),
          method="ML",na.action = na.fail) 
#avgmod.95p <- m6
dregde_modSO4 <- dredge(m6,rank="AIC") 
dregdetableSO4 <- dregde_modSO4[1:8]
modelsummarySO4 <- summary(m6)


#########################################
# regional Kendall trend test for Ca
#########################################

# create matrix to store output
data.to_sen <- data.std2 # change name of dataframe due to lasy programming - recycle of code below
data.to_sen$year <- data.std2$abs_year 
data.to_sen <- data.to_sen[data.to_sen$year>1985,]

# regional Kendall trend test on standarized values of ca
rkt(date=data.to_sen$year,y=data.to_sen$Ca,block=data.to_sen$vatn_lnr)

# regional Kendall trend test on un-standarized values of ca
rkt(date=data.to_sen$year,y=data.to_sen$abs_Ca,block=data.to_sen$vatn_lnr)
