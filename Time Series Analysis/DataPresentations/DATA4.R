library(astsa)
library(forecast)
library(TSA)
library(tseries)
x=data4.sim
plot(x)
plot(log(x))
tsdisplay(x)
lx=log(x)
lmod <- tslm(lx ~ trend)
res <- lmod$residuals
decomp.x.ad=decompose(res,type = "additive")
periodogram(decomp.x.ad$seasonal)
dres <- diff(res,1)
sres <- diff(dres,12)
tsdisplay(sres)


fit1=sarima(lmod$residuals,1,1,1,1,1,1,12) 
fit2=sarima(lmod$residuals,0,1,1,0,1,1,12) 
fit3=sarima(lmod$residuals,1,1,0,0,1,1,12) 

fit2

cast <- sarima.for(res,24,0,1,1,0,1,1,12,plot.all=TRUE)
othercast <- forecast(lmod, seq(144,168))
finalcast <- cast$pred + othercast$mean
plot(ts(c(data5.sim, finalcast)), ylab = "Data")
upper <- finalcast + 1.96*cast$se
lower <- finalcast - 1.96*cast$se
polygon(c(seq(145,168),seq(168,145)),c(upper,rev(lower)),col=gray(0.8), border = "grey")
lines(finalcast, col = "red")
abline(v = 200, col = "blue", lty = 2)