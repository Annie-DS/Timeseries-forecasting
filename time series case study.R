#IMPORTING DATA
data <- read.csv("I:/UK Outward Passengers Movement (1)/UK Outward Passengers Movement.csv",header=TRUE,sep = ",")
#DEFINE DATA AS TIME SERIES
myts <- ts(data$Total,start=c(1996,1),end = c(2005,4),frequency = 4)
plot(myts)

#DECOMPOSITION
decompose(myts,type = c("multiplicative"))

plot(decompose(myts,type = c("multiplicative")))

#ARIMA
acf(myts) 
#P=5
pacf(myts) 
#Q=3
require(tseries)
require(forecast)
adf.test(myts) 
#D=0

fit1 <- arima(myts,order=c(5,0,3))
summary(fit1)
#MAPE=2.959626

fit2 <- auto.arima(myts)
summary(fit2)
#MAPE=2.429914

fit3 <- ets(myts)
summary(fit3)
accuracy(fit3$fitted, myts)
forecast(fit3,4)
plot(forecast(fit3,4))
#MAPE=1.75779

# In fit3 we have got the lowest MAPE,so we will choose fit 3.