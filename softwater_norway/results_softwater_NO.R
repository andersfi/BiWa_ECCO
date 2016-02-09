# result section of "softwater_norway"

setwd("C:\\iSkya\\GitHub\\BiWa_ECCO\\softwater_norway")
source("importing_and_preprossesing_data.r")

############################
# figures  
#############################
require(maptools)
library(ggplot2)
library(sp)
library(raster)
library(rgeos)

# Create underlying map of Norway 
no <- getData('GADM', country="NOR", level=0) #raster data, format SpatialPolygonsDataFrame
no <- gSimplify(no, tol=0.01, topologyPreserve=TRUE)
# transform to UTM33 for visualization purposes
noUTM33 <- spTransform(no, CRS("+init=epsg:32633"))
spl.no <- list("sp.lines", as(noUTM33, "SpatialLines"))

# Make slope data SpatialPointDataFrame
plot.data <- sen.data
xy <- plot.data[c("x_coordWGS", "y_coordWGS")]
sen.data.SPDF_latlong <-
  SpatialPointsDataFrame(coords=xy, data=sen.data,
                         proj4string=CRS("+proj=longlat +datum=WGS84"))
# transform to UTM33 for visualization purposes
sen.data.SPDF <- spTransform(sen.data.SPDF_latlong, CRS("+init=epsg:32633"))



# fig 1 mean Ca, abs Change in Ca #################################################


# Figure 1
columns <- c(match("meanCa",names(sen.data.SPDF)),
             match("decedalCa",names(sen.data.SPDF)))
units <- c('mg l-1',
           'mg l-1 y-10')
figname <- "Figure_1.png"
headings <- c("mean Ca","delta Ca")
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
  if (j == 1) {
  pr <- pretty(c(sen.data.SPDF@data[[i]]), n=7)
} else {
  pr <- pretty(c(sen.data.SPDF@data[[i]],
                 -1 * sen.data.SPDF@data[[i]]), n = 6)
}
  
  
  # Norway map
  plot(noUTM33, lwd=5)
  
  # Catchment and Lake slopes
  plot(sen.data.SPDF[, i], 
       pch=21, col='black', lwd=14, cex=1.0, 
       bg=cols[cut(sen.data.SPDF@data[[i]], pr)[1:70]],
       add=TRUE)
  
  # panel heads for manually checking
  title(headings[j])
  
  legend(400000, 7000000, ## this position seems to work well
         legend = sprintf('%s to %s', pr[6:1], pr[7:2]),
         pch=21, pt.lwd=14, pt.cex=1.0,
         col='black', pt.bg=cols[6:1], bty='n', cex = 0.7,
         title=sprintf('%s', units[j]))
}
dev.off()

######### fig 2 ########################################################################
################# Figure 2_upper pannel ################################################

columns <- c(match("bCa",names(sen.data.SPDF)),
             match("bSdep",names(sen.data.SPDF)),
             match("bNDVI",names(sen.data.SPDF)))
             
units <- c('',
           '')
figname <- "Figure_2_upperPannel.png"
headings <- c("Ca","S dep.","NDVI")
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
  title(headings[j])
  
  legend(400000, 7000000, ## this position seems to work well
         legend = sprintf('%s to %s', pr[6:1], pr[7:2]),
         pch=21, pt.lwd=14, pt.cex=1.0,
         col='black', pt.bg=cols[6:1], bty='n', cex = 0.7,
         title=sprintf('%s', units[j]))
}
dev.off()

################# Figure 2_lower pannel ################################################

columns <- c(match("bQsummer",names(sen.data.SPDF)),
             match("bTMsummer",names(sen.data.SPDF)),
             match("bCl",names(sen.data.SPDF)))

units <- c('',
           '')
figname <- "Figure_2_lowerPannel.png"
headings <- c("Q","Temperature","Cl")
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
  title(headings[j])
  
  legend(400000, 7000000, ## this position seems to work well
         legend = sprintf('%s to %s', pr[6:1], pr[7:2]),
         pch=21, pt.lwd=14, pt.cex=1.0,
         col='black', pt.bg=cols[6:1], bty='n', cex = 0.7,
         title=sprintf('%s', units[j]))
}
dev.off()

############## fig 3 ####################################################


columns <- c(match("Ca_below0.5_2013",names(sen.data.SPDF)),
             match("Ca_below1.0_2013",names(sen.data.SPDF)))

units <- c('',
           '')
figname <- "Figure_3.png"
headings <- c("Ca < 0.5 mg l-1","Ca < 1.0 mg l-1")
# colours for positive and negative slopes
cols <- c('red2','blue2')  
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
                 -1 * sen.data.SPDF@data[[i]]), n = 2)
  
  
  
  # Norway map
  plot(noUTM33, lwd=5)
  
  # Catchment and Lake slopes
  plot(sen.data.SPDF[, i], 
       pch=21, col='black', lwd=14, cex=1.0, 
       bg=cols[cut(sen.data.SPDF@data[[i]], pr)[1:70]],
       add=TRUE)
  
  # panel heads for manually checking
  title(headings[j])
  
#   legend(400000, 7000000, ## this position seems to work well
#          legend = sprintf('%s to %s', pr[6:1], pr[7:2]),
#          pch=21, pt.lwd=14, pt.cex=1.0,
#          col='black', pt.bg=cols[6:1], bty='n', cex = 0.7,
#          title=sprintf('%s', units[j]))
}
dev.off()





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



#########################################
# Bivariate plot of variables 
#########################################
NDVI <- data.std2$q.summer_lag4
Sdep <- data.std2$sdep_pred
Tempr. <- data.std2$tm.summer
Year <- data.std2$year
Cl <- data.std2$Cl
Ca <- data.std2$Ca

slope_Ca <-  sen.data$bCa
slope_NDVI <- sen.data$bNDVI
slope_Sdep <- sen.data$bSdep
slope_Tempr. <- sen.data$bTMsummer
slope_Cl <- sen.data$bCl


# panel.cor function with compliments to Stephen Turner http://www.r-bloggers.com/scatterplot-matrices-in-r/
panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

pairs(~Ca + Cl + NDVI + Sdep + Tempr.+ Year,main="Raw variables",upper.panel=panel.cor)

pairs(~slope_Ca + slope_Cl + slope_NDVI + slope_Sdep + slope_Tempr.,main="Slope",upper.panel=panel.cor)


