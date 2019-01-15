library(TSA)

plot(data5.sim)
ld <- log(data5.sim)
plot(ld)
decomp.x.ad=decompose(data5.sim,type = "additive")
plot(decomp.x.ad)
periodogram(decomp.x.ad$seasonal)

rd <- decomp.x.ad$random
rd <- na.remove(rd)
acf(rd)
pacf(rd)
tsdisplay(rd)

fit1 <- sarima(ld, 4,0,1,1,1,1,12)
fit2 <- sarima(ld, 1,0,1,4,1,3,4)
fit3 <- sarima(ld, 1,1,2,1,1,1,12)
fit4 <- sarima(ld, 1,1,3,0,1,1,12)
fit1 # AIC -295.55
fit2 # AIC 248.27
fit3 # AIC -301.57
fit4 # AIC -304.22
sarima.for(lx,60,1,1,3,0,1,1,12,plot.all=TRUE)
?shapiro.test