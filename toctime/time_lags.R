### ndvi/TOC Correlation in time lags ############

acf.out <- as.numeric()
lag.out <- as.numeric()
lnr.out <- as.numeric()

for(i in 1:(length(unique(data.std2$vatn_lnr)))) #rep for unique no. lakes
{ccf.list <- ccf( 
  as.ts(data.std2$ndvi.summer[data.std2$vatn_lnr == unique(data.std2$vatn_lnr)[i]]), 
  as.ts(data.std2$lnTOC[data.std2$vatn_lnr == unique(data.std2$vatn_lnr)[i]]), 
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

lag.cor.ndvi.toc <- as.data.frame(cbind(acf.out, lag.out, lnr.out))

ndvi.toc.lag <- as.data.frame(cbind(lag.out, acf.out))
names(ndvi.toc.lag) <- c("lag", "acf")
ndvi.cor.agg <- aggregate(ndvi.toc.lag, by = list(ndvi.toc.lag$lag), FUN = mean)



############################
### sdep/TOC 

acf.out.sdep <- as.numeric()
lag.out.sdep <- as.numeric()
lnr.out.sdep <- as.numeric()

for(i in 1:(length(unique(data.std2$vatn_lnr)))) #rep for unique no. lakes
{
  ccf.list.sdep <- ccf( 
    as.ts(data.std2$sdep_pred[data.std2$vatn_lnr == unique(data.std2$vatn_lnr)[i]]), 
    as.ts(data.std2$lnTOC[data.std2$vatn_lnr == unique(data.std2$vatn_lnr)[i]]), 
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

lag.cor.sdep.toc <- as.data.frame(cbind(acf.out.sdep, lag.out.sdep, lnr.out.sdep))
names(lag.cor.sdep.toc) <- c("acf","lag","lnr")
cor.agg.sdep <- aggregate(lag.cor.sdep.toc, by = list(lag.cor.sdep.toc$lag), FUN = mean)



############################
### q.summer/TOC 

acf.out.q.summer <- as.numeric()
lag.out.q.summer <- as.numeric()
lnr.out.q.summer <- as.numeric()

for(i in 1:(length(unique(data.std2$vatn_lnr)))) #rep for unique no. lakes
{
  ccf.list.q.summer <- ccf( 
    as.ts(data.std2$q.summer[data.std2$vatn_lnr == unique(data.std2$vatn_lnr)[i]]), 
    as.ts(data.std2$lnTOC[data.std2$vatn_lnr == unique(data.std2$vatn_lnr)[i]]), 
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
## tm.summer-TOC 

acf.out.tm.summer <- as.numeric()
lag.out.tm.summer <- as.numeric()
lnr.out.tm.summer <- as.numeric()

for(i in 1:(length(unique(data.std2$vatn_lnr)))) #rep for unique no. lakes
{
  ccf.list.tm.summer <- ccf( 
    as.ts(data.std2$tm.summer[data.std2$vatn_lnr == unique(data.std2$vatn_lnr)[i]]), 
    as.ts(data.std2$lnTOC[data.std2$vatn_lnr == unique(data.std2$vatn_lnr)[i]]), 
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