### ndvi/Ca Correlation in time lags ############
source("importing_and_preprossesing_data.r")

acf.out <- as.numeric()
lag.out <- as.numeric()
lnr.out <- as.numeric()

for(i in 1:(length(unique(data.std2$vatn_lnr)))) #rep for unique no. lakes
{ccf.list <- ccf( 
  as.ts(data.std2$ndvi.summer[data.std2$vatn_lnr == unique(data.std2$vatn_lnr)[i]]), 
  as.ts(data.std2$lnCa[data.std2$vatn_lnr == unique(data.std2$vatn_lnr)[i]]), 
  lag.max = 30, 
  na.action = na.pass,
  type = c("correlation"),plot=F
)
acf2 <- as.numeric(ccf.list$acf)
lag2 <- as.numeric(ccf.list$lag)
lnr <-  rep(unique(data.std2$vatn_lnr)[i],length(lag2))

acf.out <- append(acf.out, acf2)
lag.out <- append(lag.out, lag2)
lnr.out <- append(lnr.out, lnr)

}

lag.cor.ndvi.ca <- as.data.frame(cbind(acf.out, lag.out, lnr.out))

ndvi.ca.lag <- as.data.frame(cbind(lag.out, acf.out))
names(ndvi.ca.lag) <- c("lag", "acf")
cor.agg.ndvi <- aggregate(ndvi.ca.lag, by = list(ndvi.ca.lag$lag), FUN = mean)



############################
### sdep/Ca

acf.out.sdep <- as.numeric()
lag.out.sdep <- as.numeric()
lnr.out.sdep <- as.numeric()

for(i in 1:(length(unique(data.std2$vatn_lnr)))) #rep for unique no. lakes
{
  ccf.list.sdep <- ccf( 
    as.ts(data.std2$sdep_pred[data.std2$vatn_lnr == unique(data.std2$vatn_lnr)[i]]), 
    as.ts(data.std2$lnCa[data.std2$vatn_lnr == unique(data.std2$vatn_lnr)[i]]), 
    lag.max = 30, 
    na.action = na.pass,
    type = c("correlation"),plot=F)
  
  acf2 <- as.numeric(ccf.list.sdep$acf)
  lag2 <- as.numeric(ccf.list.sdep$lag)
  lnr <-  rep(unique(data.std2$vatn_lnr)[i],length(lag2))
  
  acf.out.sdep <- append(acf.out.sdep, acf2)
  lag.out.sdep <- append(lag.out.sdep, lag2)
  lnr.out.sdep <- append(lnr.out.sdep, lnr)
}

lag.cor.sdep.ca <- as.data.frame(cbind(acf.out.sdep, lag.out.sdep, lnr.out.sdep))
names(lag.cor.sdep.ca) <- c("acf","lag","lnr")
cor.agg.sdep <- aggregate(lag.cor.sdep.ca, by = list(lag.cor.sdep.ca$lag), FUN = mean)



############################
### q.summer/Ca 

acf.out.q.summer <- as.numeric()
lag.out.q.summer <- as.numeric()
lnr.out.q.summer <- as.numeric()

for(i in 1:(length(unique(data.std2$vatn_lnr)))) #rep for unique no. lakes
{
  ccf.list.q.summer <- ccf( 
    as.ts(data.std2$q.summer[data.std2$vatn_lnr == unique(data.std2$vatn_lnr)[i]]), 
    as.ts(data.std2$lnCa[data.std2$vatn_lnr == unique(data.std2$vatn_lnr)[i]]), 
    lag.max = 30, 
    na.action = na.pass,
    type = c("correlation"),plot=F)
  
  acf2 <- as.numeric(ccf.list.q.summer$acf)
  lag2 <- as.numeric(ccf.list.q.summer$lag)
  lnr <-  rep(unique(data.std2$vatn_lnr)[i],length(lag2))
  
  acf.out.q.summer <- append(acf.out.q.summer, acf2)
  lag.out.q.summer <- append(lag.out.q.summer, lag2)
  lnr.out.q.summer <- append(lnr.out.q.summer, lnr)
  
}

lag.cor.q.summer.toc <- as.data.frame(cbind(acf.out.q.summer, lag.out.q.summer, lnr.out.q.summer))
names(lag.cor.q.summer.toc) <- c("acf","lag","lnr")
cor.agg.q.summer <- aggregate(lag.cor.q.summer.toc, by = list(lag.cor.q.summer.toc$lag), FUN = mean)


###############
## tm.summer-Ca 

acf.out.tm.summer <- as.numeric()
lag.out.tm.summer <- as.numeric()
lnr.out.tm.summer <- as.numeric()

for(i in 1:(length(unique(data.std2$vatn_lnr)))) #rep for unique no. lakes
{
  ccf.list.tm.summer <- ccf( 
    as.ts(data.std2$tm.summer[data.std2$vatn_lnr == unique(data.std2$vatn_lnr)[i]]), 
    as.ts(data.std2$lnCa[data.std2$vatn_lnr == unique(data.std2$vatn_lnr)[i]]), 
    lag.max = 30, 
    na.action = na.pass,
    type = c("correlation"),plot=F)
  
  acf2 <- as.numeric(ccf.list.tm.summer$acf)
  lag2 <- as.numeric(ccf.list.tm.summer$lag)
  lnr <-  rep(unique(data.std2$vatn_lnr)[i],length(lag2))
  
  acf.out.tm.summer <- append(acf.out.tm.summer, acf2)
  lag.out.tm.summer <- append(lag.out.tm.summer, lag2)
  lnr.out.tm.summer <- append(lnr.out.tm.summer, lnr)
  
}


lag.cor.tm.summer.toc <- as.data.frame(cbind(acf.out.tm.summer, lag.out.tm.summer, lnr.out.tm.summer))
names(lag.cor.tm.summer.toc) <- c("acf","lag","lnr")

cor.agg.tm.summer <- aggregate(lag.cor.tm.summer.toc, by = list(lag.cor.tm.summer.toc$lag), FUN = mean)

###############################################
## plotting ###################################
##################################################
## plot NDVI-TOC

par(mfrow=c(2,2))

scatter.smooth(cor.agg.ndvi$lag[cor.agg.ndvi$lag<1], 
               cor.agg.ndvi$acf[cor.agg.ndvi$lag<1], 
               xlab="Lag",
               ylab="ccf"
               ,span = 0.25,
               col = "green",
               pch = 19,main="NDVI - Ca ccf",xlim=c(-15,0))


scatter.smooth(cor.agg.sdep$lag[cor.agg.sdep$lag<1], 
               cor.agg.sdep$acf[cor.agg.sdep$lag<1], 
               xlab="Lag",
               ylab="ccf"
               ,span = 0.25,
               col = "blue",
               pch = 19,main="S deposition - Ca ccf"
)


scatter.smooth(cor.agg.q.summer$lag[cor.agg.q.summer$lag<1], 
               cor.agg.q.summer$acf[cor.agg.q.summer$lag<1], 
               xlab="Lag",
               ylab="acf"
               ,span = 0.25,
               col = "blue",
               pch = 19,main="runoff -Ca ccf")


scatter.smooth(cor.agg.tm.summer$lag[cor.agg.tm.summer$lag<1], 
               cor.agg.tm.summer$acf[cor.agg.tm.summer$lag<1], 
               xlab="Lag",
               ylab="ccf"
               ,span = 0.25,
               col = "blue",
               pch = 19,main="Temperature - Ca ccf"
)
# abline(0,0,col="red")
# abline(v = 0, lty = 3, col = "red")


