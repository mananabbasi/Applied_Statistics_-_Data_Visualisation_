install.packages("TTR") 
install.packages("forecast") 
library(TTR) 
library(forecast)

kings <- scan("http://robjhyndman.com/tsdldata/misc/kings.dat",skip=3) 
kings
kingstimeseries <- ts(kings) 
kingstimeseries 
births <- scan("http://robjhyndman.com/tsdldata/data/nybirths.dat")  
birthstimeseries <- ts(births, frequency=12, start=c(1946,1)) 
birthstimeseries 

souvenir <- scan("http://robjhyndman.com/tsdldata/data/fancy.dat")  
souvenirtimeseries <- ts(souvenir, frequency=12, start=c(1987,1)) 
souvenirtimeseries 

plot.ts(kingstimeseries) 

plot.ts(birthstimeseries) 

plot.ts(souvenirtimeseries) 

logsouvenirtimeseries <- log(souvenirtimeseries) 
plot.ts(logsouvenirtimeseries)

library("TTR")

kingstimeseriesSMA3 <- SMA(kingstimeseries,n=3) 
plot.ts(kingstimeseriesSMA3)

kingstimeseriesSMA8 <- SMA(kingstimeseries,n=8) 
plot.ts(kingstimeseriesSMA8)
birthstimeseriescomponents <- decompose(birthstimeseries) 

birthstimeseriescomponents$seasonal
plot(birthstimeseriescomponents)

birthstimeseriescomponents <- decompose(birthstimeseries) 
birthstimeseriesseasonallyadjusted <- birthstimeseries - birthstimeseriescomponents$seasonal

plot(birthstimeseriesseasonallyadjusted) 

rain <- scan("http://robjhyndman.com/tsdldata/hurst/precip1.dat",skip=1)  
rainseries <- ts(rain,start=c(1813)) 
plot.ts(rainseries)


rainseriesforecasts <- HoltWinters(rainseries, beta=FALSE, gamma=FALSE) 
rainseriesforecasts 

rainseriesforecasts$fitted 
plot(rainseriesforecasts)

rainseriesforecasts$SSE 

HoltWinters(rainseries, beta=FALSE, gamma=FALSE, l.start=23.56) 
library("forecast") 

rainseriesforecasts2 <- forecast(rainseriesforecasts, h=8) 
rainseriesforecasts2 

plot(rainseriesforecasts2) 

acf(rainseriesforecasts2$residuals, lag.max=20 , na.action = na.pass) 

Box.test(rainseriesforecasts2$residuals, lag=20, type="Ljung-Box") 
plot.ts(rainseriesforecasts2$residuals) 



plotForecastErrors <- function(forecasterrors) {
  # make a histogram of the forecast errors
  mybinsize <- IQR(forecasterrors) / 4  
  mysd <- sd(forecasterrors) 
  mymin <- min(forecasterrors) - mysd * 5  
  mymax <- max(forecasterrors) + mysd * 3 
  
  # generate normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(10000, mean = 0, sd = mysd) 
  mymin2 <- min(mynorm) 
  mymax2 <- max(mynorm) 
  
  if (mymin2 < mymin) { 
    mymin <- mymin2 
  }
  if (mymax2 > mymax) { 
    mymax <- mymax2 
  }
  
  # create bins for the histogram
  mybins <- seq(mymin, mymax, mybinsize)
  
  # make a red histogram of the forecast errors, with the normally distributed data overlaid
  hist(forecasterrors, col = "red", freq = FALSE, breaks = mybins) 
  # freq = FALSE ensures the area under the histogram = 1
  
  # generate histogram data for normally distributed data
  myhist <- hist(mynorm, plot = FALSE, breaks = mybins)
  
  # plot the normal curve as a blue line on top of the histogram of forecast errors
  points(myhist$mids, myhist$density, type = "l", col = "blue", lwd = 2) 
}

# Removing any NA values from residuals
rainseriesforecasts2$residuals <- rainseriesforecasts2$residuals[!is.na(rainseriesforecasts2$residuals)]

# Plotting the forecast errors
plotForecastErrors(rainseriesforecasts2$residuals)


skirts <- scan("http://robjhyndman.com/tsdldata/roberts/skirts.dat",skip=5)  
skirtsseries <- ts(skirts,start=c(1866)) 
plot.ts(skirtsseries)

skirtsseriesforecasts <- HoltWinters(skirtsseries, gamma=FALSE) 
 skirtsseriesforecasts 
 
 plot(skirtsseriesforecasts)
 
 HoltWinters(skirtsseries, gamma=FALSE, l.start=608, b.start=9)
 
 skirtsseriesforecasts2 <- forecast(skirtsseriesforecasts, h=19) 
 plot(skirtsseriesforecasts2) 
 
 acf(na.omit(skirtsseriesforecasts2$residuals), lag.max=20) 
 Box.test(na.omit(skirtsseriesforecasts2$residuals), lag=20, type="Ljung-Box" , 
            na.action = na.pass) 
