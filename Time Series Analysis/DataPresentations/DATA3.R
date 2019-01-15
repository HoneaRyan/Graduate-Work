library('forecast')
library('tseries')
library('astsa')

data <- data3.sim
data  <- ts(data)
plot(data)
adf.test(data)
tsdisplay(data)

fit1 <- arima(data,order=c(2,0,1), method ="CSS-ML")
fit2 <- arima(data,order=c(1,0,1), method ="CSS-ML")
fit3 <- arima(data,order=c(0,0,1), method ="CSS-ML")
AIC(fit1,fit2,fit3)
tsdiag(fit3)
fcast=forecast(fit3,h=30)
plot(fcast)
fcast
