library('forecast')
library('tseries')
library('astsa')

data <- data2.sim
plot(data)
lmod <- tslm(data ~ trend)
new_data <- lmod$residuals
new_data <- ts(new_data)
plot(new_data)
data_diff_s <- diff(new_data,5)
par(mfrow = c(2,1))
acf(data_diff_s)
pacf(data_diff_s)

fit1 <- sarima(new_data, 1,1,0,1,1,1,5)
fit2 <- sarima(new_data, 5,0,0,1,1,1,5)
fit3 <- sarima(new_data, 0,1,1,1,1,1,5)
cast <- sarima.for(new_data,50,5,0,0,1,1,1,5,plot.all=TRUE)
forecast(lmod, seq(201,250))
finalcast <- cast$pred + othercast$mean
plot(ts(c(data2.sim, finalcast)), ylab = "Data")
upper <- finalcast + 1.96*cast$se
lower <- finalcast - 1.96*cast$se
polygon(c(seq(201,250),seq(250,201)),c(upper,rev(lower)),col=gray(0.8), border = "grey")
lines(finalcast, col = "red")
abline(v = 200, col = "blue", lty = 2)