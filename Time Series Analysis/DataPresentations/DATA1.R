library('forecast')
library('tseries')
library('astsa')

data <- data1.sim
data <- ts(data)
plot(data)
data_diff <- diff(data)
plot(data_diff)
adf.test(data_diff)
par(mfrow=c(2,1))
acf(data_diff)
pacf(data_diff)

fit1<-Arima(data,order=c(2,1,1),method ="CSS-ML");
fit2<-Arima(data,order=c(2,1,2),method ="CSS-ML");
fit3<-Arima(data,order=c(2,1,0),method = "CSS-ML");
fit4<-Arima(data,order=c(3,1,1),method = "CSS-ML");
AIC(fit1,fit2,fit3,fit4)
fit1
tsdiag(fit1)
fcast=forecast(fit1,h=30)
plot(fcast)