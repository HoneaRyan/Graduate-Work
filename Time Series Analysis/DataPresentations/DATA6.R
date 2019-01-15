library("quantmod")
library("TSA")
library("astsa")
library("fGarch")
library("forecast")
eps <- .Machine$double.eps ## defining a small number 

x=log(data6.sim)
plot(x)
diff_x <- diff(x)[-1]
plot(diff_x)
par(mfrow = c(2,1))
acf(diff_x)
pacf(diff_x)

fit1<-garchFit(~garch(1,1),data=diff_x) 
fit2<-garchFit(~garch(1,2),data=diff_x)
fit3<-garchFit(~garch(2,1),data=diff_x)
fit4<-garchFit(~garch(2,2),data=diff_x)

summary(fit1) # AIC 2.867915
summary(fit2) # AIC 2.817671
summary(fit3) # AIC 2.872124
summary(fit4) # AIC 2.819

fcast1=predict(fit2,n.ahead=30,plot=TRUE)