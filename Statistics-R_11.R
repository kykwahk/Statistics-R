
###############################
## R을 이용한 통계데이터분석 ##
## (곽기영, 도서출판 청람)   ## 
###############################

#######################
## 제11장 시계열분석 ##
#######################

########################
## 11.1 시계열 데이터 ##
########################

url <- "http://jse.amstat.org/datasets/utility.dat.txt"
utility <- read.table(url)
utility

utility.ts <- ts(data=utility[[7]], start=c(1990, 9), frequency=12)
utility.ts
class(utility.ts)

# [그림 11-1]
windows(width=7.0, height=5.5)
plot(utility.ts)

start(utility.ts)
end(utility.ts)

frequency(utility.ts)
deltat(utility.ts)

time(utility.ts)

cycle(utility.ts)

window(utility.ts, start=c(1991, 1), end=c(1992, 6))

window(utility.ts, start=c(1991, 1), frequency=1)
window(utility.ts, start=c(1991, 7), frequency=1)

window(utility.ts, start=c(1990, 9), frequency=2)

window(utility.ts, start=c(1991, 1), frequency=4)

#############################
## 11.2 시계열 데이터 분해 ##
#############################

nhtemp
frequency(nhtemp)

# [그림 11-2]
install.packages("forecast")
library(forecast)
windows(width=7.0, height=5.5)
old.par <- par(mfrow=c(2,2))
ylim <- c(min(nhtemp), max(nhtemp))
plot(nhtemp, ylim=ylim, col="dimgray", lwd=2, 
     main="Base Time Series", ylab="Temperature")
plot(ma(nhtemp, 3), ylim=ylim, col="red", lwd=2, 
     main="Simple Moving Average (k=3)", ylab="Temperature")
plot(ma(nhtemp, 7), ylim=ylim, col="green3", lwd=2,
     main="Simple Moving Average (k=7)", ylab="Temperature")
plot(ma(nhtemp, 11), ylim=ylim, col="blue", lwd=2,
     main="Simple Moving Average (k=11)", ylab="Temperature")
par(old.par)

# [그림 11-3]
windows(width=8.0, height=8.0)
old.par <- par(mfrow=c(2,1))
plot(window(co2, start=c(1985, 1), end=c(1996, 12)), col="salmon", lwd=2,
     main="Additive Trend, Seasonal, Irregular Components", 
     xlab="Year", ylab="CO2 Concentration (Parts Per Million)")
plot(AirPassengers, col="cornflowerblue", lwd=2,
     main="Multiplicative Trend, Seasonal, Irregular Components",
     xlab="Year", ylab="Air Passengers (Thousand Persons)")
par(old.par)

data(co2)
co2 <- window(co2, start=c(1985, 1), end=c(1996, 12))
co2

co2.decomp <- stl(co2, s.window="periodic")
co2.decomp

# [그림 11-4]
windows(width=7.0, height=5.5)
plot(co2.decomp, col="darkcyan", col.range="skyblue", lwd=2,
     main="Decomposition of CO2 Concentration Time Series")

co2
co2.decomp$time.series

co2.adj <- co2 - co2.decomp$time.series[, "seasonal"]
co2.adj

# [그림 11-5]
windows(width=7.0, height=5.5)
plot(co2.adj, col="tomato", lwd=2,
     main="CO2 Concentration Time Series without Seasonal Effect",
     xlab="Year", ylab="CO2 Concentration (Parts Per Million)")

# [그림 11-6]
library(forecast)
windows(width=7.0, height=9.0)
old.par <- par(mfrow=c(2,1))
monthplot(co2, col="slateblue", lwd=2, main="Month Plot", 
          xlab="Month", ylab="CO2 Concentration (Parts Per Million)")
seasonplot(co2, col="sienna", lwd=2, year.labels=TRUE, main="Season Plot", 
           ylab="CO2 Concentration (Parts Per Million)")
par(old.par)

AirPassengers

# [그림 11-7]
windows(width=8.0, height=8.0)
old.par <- par(mfrow=c(2,1))
plot(AirPassengers, col="maroon", lwd=2,
     main="Air Passengers",
     xlab="Year", ylab="Air Passengers (Thousands)")
lair <- log(AirPassengers)
plot(lair, col="navy", lwd=2,
     main="Log Transformation of Air Passengers",
     xlab="Year", ylab="Air Passengers (Log(Thousands))")
par(old.par)

lair.decomp <- stl(lair, s.window="periodic")

# [그림 11-8]
windows(width=7.0, height=5.5)
plot(lair.decomp, col="chocolate", col.range="orange", lwd=2,
     main="Decomposition of Log Transformed Air Passengers")

lair.decomp$time.series
exp(lair.decomp$time.series)

#######################
## 11.3 지수예측모델 ##
#######################

## 단순지수평활법(simple exponential smoothing)

LakeHuron

windows(width=7.0, height=5.5)
plot(LakeHuron)
mean(LakeHuron)

library(forecast)
lake.ets <- ets(LakeHuron, model="ANN")
lake.ets

lake.ets.pred <- forecast(lake.ets, h=1)
lake.ets.pred

# [그림 11-10]
windows(width=7.0, height=5.5)
plot(lake.ets.pred, col="royalblue", lwd=2,
     main="Forecast for Annual Level of Lake Huron", 
     xlab="Year", ylab="Level (Feet)")

accuracy(lake.ets)

## 홀트지수평활법(Holt exponential smoothing)

install.packages("fpp")
library(fpp)
elecsales

library(forecast)
elecsales.ets <- ets(elecsales, model="AAN")
elecsales.ets
accuracy(elecsales.ets)

elecsales.ets.pred <- forecast(elecsales.ets, h=5)
elecsales.ets.pred

# [그림 11-11]
windows(width=7.0, height=5.5)
plot(elecsales.ets.pred, col="royalblue", lwd=2,
     flty=3, flwd=3, shadecols=c("lavender", "mistyrose"),  
     main="Forecast for Electricity Sales in South Australia",
     xlab="Year", ylab="Electricity Sales (GWh)")

## 홀트윈터스(Holt-Winters exponential smoothing)

library(forecast)
lair.ets <- ets(log(AirPassengers), model="AAA")
lair.ets
accuracy(lair.ets)

lair.ets.pred <- forecast(lair.ets, h=12)
lair.ets.pred

# [그림 11-12]
windows(width=7.0, height=5.5)
plot(lair.ets.pred, col="salmon", lwd=2, fcol="indianred1", flwd=3,
     main="Forecast for Air Passengers", 
     xlab="Year", ylab="Air Passengers (Log(Thousand Persons))")

air.mean <- exp(lair.ets.pred$mean)
air.lower <- exp(lair.ets.pred$lower)
air.upper <- exp(lair.ets.pred$upper)
air.pred <- cbind(air.mean, air.lower, air.upper)
air.pred

## 완화추세(damped trend)와 모델자동선택

library(fpp)
austourists

austourists.ets <- ets(austourists)
austourists.ets

# [그림 11-13] 
windows(width=7.0, height=5.5)
plot(forecast(austourists.ets, h=12), col="cornflowerblue", lwd=2,
     flty=1, flwd=3, fcol="royalblue", shadecols=c("mistyrose", "salmon"),
     main="Forecast for International Tourists to Australia",
     xlab="Year", ylab="Total Visitor Nights")

########################
## 11.4 ARIMA예측모델 ##
########################

## 정상성과 자기상관(stationarity and autocorrelation)

# [그림 11-14]
windows(width=7.0, height=5.5)
old.par <- par(mfrow=c(2,2))
plot(AirPassengers, col="red", lwd=2,
     main="(a) Air Passengers", xlab="Year", ylab="Persons (1,000)")
install.packages("fpp2")
library(fpp2)
plot(goog200, col="blue", lwd=2,
     main="(b) Google Stock Prices", xlab="Day", ylab="Dollars")
plot(Nile, col="green3", lwd=2,
     main="(c) Flow of the River Nile", xlab="Year", ylab="Flow")
plot(nottem, col="mediumorchid4", lwd=2,
     main="(d) Temperatures at Nottingham", xlab="Year", ylab="Fahrenheit")
par(old.par)

Nile

lag(Nile, 0)
lag(Nile, 1)
lag(Nile, 2)

library(fpp2)
library(forecast)
head(goog200)
ndiffs(goog200)
dgoog200 <- diff(goog200)
head(dgoog200)

# [그림 11-16]
windows(width=7.0, height=5.5)
old.par <- par(mfrow=c(2,2))
plot(goog200, col="cornflowerblue", lwd=2,
     main="(a) Google Stock Prices", xlab="Day", ylab="Dollars")
plot(dgoog200, col="salmon", lwd=2,
     main="(b) Google Stock Prices\nTransformed by Differencing", 
     xlab="Day", ylab="Dollars")
Acf(goog200, lwd=2, main="Original Data")
Acf(dgoog200, lwd=2, main="Differenced Data")
par(old.par)

library(tseries)
adf.test(goog200)
adf.test(dgoog200)

# [그림 11-17]
windows(width=7.0, height=5.5)
old.par <- par(mfrow=c(2,2))
plot(AirPassengers, col="darkolivegreen4", lwd=2, main="(a) Non-stationary Time Series")
plot(log(AirPassengers), col="firebrick", lwd=2, main="(b) Constant Variance")
plot(diff(AirPassengers), col="darksalmon", lwd=2, main="(c) Constant Mean")
plot(diff(log(AirPassengers)), col="goldenrod", lwd=2, main="(d) Stationary Time Series")
par(old.par)

## ARMA모델과 ARIMA모델

library(tseries)
adf.test(Nile)

library(forecast)
ndiffs(Nile)

dNile <- diff(Nile)
adf.test(dNile)

# [그림 11-18]
windows(width=7.0, height=8.0)
old.par <- par(mfrow=c(2,1))
plot(Nile, col="darkviolet", lwd=2, main="Flow of the River Nile: Original",
     xlab="Year", ylab="Flow")
plot(dNile, col="dodgerblue", lwd=2, main="Flow of the River Nile: Differenced",
     xlab="Year", ylab="Differenced Flow")
par(old.par)

# [그림 11-19]
windows(width=7.0, height=8.0)
old.par <- par(mfrow=c(2,1))
Acf(dNile, lwd=2, main="Autocorrelation for the River Nile")
Pacf(dNile, lwd=2, main="Partial Autocorrelation for the River Nile")
par(old.par)

Acf(dNile, plot=FALSE)
Pacf(dNile, plot=FALSE)

Nile.arima <- arima(Nile, order=c(0, 1, 1))
Nile.arima
accuracy(Nile.arima)

# [그림 11-21]
windows(width=7.0, height=5.5)
old.par <- par(mfrow=c(1,2))
hist(Nile.arima$residuals, col="mistyrose", prob=TRUE, 
     main="Histogram of Residuals", xlab="Residuals")
xfit <- seq(min(Nile.arima$residuals), max(Nile.arima$residuals), length.out=40)
yfit <- dnorm(xfit, mean=mean(Nile.arima$residuals), sd=sd(Nile.arima$residuals))
lines(xfit, yfit, col="tomato", lwd=2)
qqnorm(Nile.arima$residuals, pch=21, col="black", bg="gold", 
       main="Q-Q Plot of Residuals")
qqline(Nile.arima$residuals, col="royalblue", lwd=2)
par(old.par)

Box.test(Nile.arima$residuals, type="Ljung-Box")

Nile.arima.pred <- forecast(Nile.arima, h=5)
Nile.arima.pred

# [그림 11-22]
windows(width=7.0, height=5.5)
plot(Nile.arima.pred, col="darkgreen", lwd=2, flty=1, flwd=3,
     fcol="royalblue", shadecols=c("mistyrose", "salmon"),
     main="Forecast for Flow of the River Nile",
     xlab="Year", ylab="Flow")

## 계절 성분(seasonal factor)과 모델자동선택

library(forecast)
gas

gas.arima <- auto.arima(gas)
gas.arima
accuracy(gas.arima)

arima(gas, order=c(2, 1, 1), seasonal=list(order=c(0, 1, 1), period=12))

forecast(gas.arima, h=5*12)

# [그림 11-23]
windows(width=7.0, height=5.5)
plot(forecast(gas.arima, h=5*12), col="darkorange", lwd=2,
     flty=1, flwd=3, fcol="orangered", shadecols=c("lavender", "skyblue"),
     main="Australian Monthly Gas Production",
     xlab="Year", ylab="Monthly Production")

# [그림 11-24]
install.packages("ggfortify")
library(ggfortify)
library(scales)
windows(width=7.0, height=5.5)
autoplot(forecast(gas.arima, h=5*12), ts.colour="cornflowerblue", ts.size=1, 
         predict.colour="salmon", predict.linetype="solid",
         predict.size=1, conf.int.fill="tomato") +
  scale_y_continuous(labels=comma) +
  labs(x="", y="Monthly Production",
       title="Australian Monthly Gas Production") +
  theme_minimal() +
  theme(plot.title=element_text(face="bold", size=14),
        axis.line=element_line(),
        axis.ticks=element_line(),
        axis.text.x=element_text(size=10))
