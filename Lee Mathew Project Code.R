library(TSA)
library(fpp)
library(Metrics)
library(car)
library(lmtest)
library(randtests)
library(tseries)
library(stats)
library(fGarch)
library(ggplot2)

setwd("C:/Users/leeme/Desktop/Usciences/Time Series")
rain <- read.csv("rain.csv")

#Basic EDA and Differencing/Log Difference
ggtsdisplay(rain$prec, main = ' ')

lambda <- BoxCox.lambda(rain$prec)
print(lambda)

par(mfrow=c(3,1), mar=c(1,4,2,4))
diff.rain <- diff(rain$prec)
plot(diff.rain, type='l', main='Difference Rain') 

log.rain <- log(rain$prec)
plot(log.rain,type='l', main='Log Rain') 

difflog.rain <- diff(log.rain)
plot(difflog.rain, type='l', main='Difference of Log-Rain') 

ggtsdisplay(difflog.rain, main='') 

#Train/Test Split
train <- log.rain[1:100]
test <- log.rain[101:136] 

#Fitting ARIMA Models
aic.fits <- matrix(0,6,6)  
for (i in 0:5) for (j in 0:5) { 
  fits <- arima(train, order=c(i,1,j), include.mean=TRUE)
  aic.fits[i+1,j+1] <- fits$aic
} 
aic.fits
print(sort(aic.fits))

#Top 5 (is lowest bc negatives?)
ARMA.fit1 <- arima(train, order=c(1,1,5), include.mean=TRUE)
ARMA.fit2 <- arima(train, order=c(1,1,2), include.mean=TRUE)
ARMA.fit3 <- arima(train, order=c(2,1,5), include.mean=TRUE)
ARMA.fit4 <- arima(train, order=c(0,1,5), include.mean=TRUE)
ARMA.fit5 <- arima(train, order=c(1,1,4), include.mean=TRUE)

ARMA.fit <- ARMA.fit5
AIC = -2*ARMA.fit$loglik+2*(length(ARMA.fit$coef)+2)
print(AIC)
BIC = AIC +floor(log(ARMA.fit$nobs)-2)*(length(ARMA.fit$coef)+2)
print(BIC)
AICc = AIC +(2*(length(ARMA.fit$coef)+2)*(length(ARMA.fit$coef)+3))/(ARMA.fit$nobs-length(ARMA.fit$coef)-3)
print(AICc)

#MSE, MAE, MAPE
forecast1 <- predict(ARMA.fit1, 36) 
forecast2 <- predict(ARMA.fit2, 36)
forecast3 <- predict(ARMA.fit3, 36)
forecast4 <- predict(ARMA.fit4, 36)
forecast5 <- predict(ARMA.fit5, 36)

forecast = forecast5
mse(test,forecast$pred)
mae(test,forecast$pred)
mape(test,forecast$pred)

#Fitting Model
ARMA.fit <- arima(diff.rain, order=c(0,1,5), include.mean=TRUE)
ARMA.fit 
res.fit <- ARMA.fit$residuals

#Checking Residuals 
checkresiduals(ARMA.fit)
bartels.rank.test(res.fit)
shapiro.test(res.fit)
wilcox.test(res.fit)
bptest(res.fit ~ c(seq(1, length(res.fit))))

par(mfrow=c(2,1), mar=c(1,4,2,4))
qqnorm(res.fit); qqline(res.fit)
McLeod.Li.test(y=res.fit, lwd=2)


#Forecasting Next 3 Values
forecast(rain$prec, 3)
plot(forecast(rain$prec, 3))

