# checking model assumptions 
modpred <- predict(avgmod.95p)
moderr <- data.std2$lnCa - modpred

par(mfrow=c(2,2))
plot(modpred,moderr,xlab="Predicted lnCa",ylab="Residuals",main="Fig. 5: Predicted TOC vs. model residuals")
abline(0,0,col="red")

plot(data.std2$lnCa,modpred,ylim=c(-3,3),xlim=c(-3,3),
     xlab="Observed Ca",ylab="Predicted Ca",main="Fig. 6: Observed lnCa vs. predicted Ca")
abline(0,1,col="red")

# autocorrelations in residuals
acf.out <- as.numeric()
lag.out <- as.numeric()
lnr.out <- as.numeric()

for(i in 1:(length(unique(data.std2$vatn_lnr)))) #rep for unique no. lakes
{
  acf.list <- acf( 
    as.ts(moderr[data.std2$vatn_lnr == unique(data.std2$vatn_lnr)[i]]),  
    lag.max = 15, 
    na.action = na.pass,
    type = c("correlation"),plot=F)
  
  acf2 <- as.numeric(acf.list$acf)
  lag2 <- as.numeric(acf.list$lag)
  lnr <-  rep(unique(data.std2$vatn_lnr)[i],length(lag2))
  
  acf.out <- append(acf.out, acf2)
  lag.out <- append(lag.out, lag2)
  lnr.out <- append(lnr.out, lnr)
}
lcnt <- as.data.frame(cbind(acf.out, lag.out, lnr.out))

plot(lcnt$lag.out,lcnt$acf.out,xlab="lag",ylab="acf",main="Fig. 7: Autocorrelation plot of model residuals")
abline(h=0.45,col="red")
abline(h=-0.45,col="red") # red line indicates signifcant corralations for indidvidual observations 
