# Time Series - PPT Tutorial

######################################################################

# We need to establish the working directory where the file is located within.
# set the working directory

setwd("C:/nwu/R/Timeseries")


######################################################################
kings <- scan("http://robjhyndman.com/tsdldata/misc/kings.dat",skip=3)
kingstimeseries <- ts(kings)


library("TTR")
kingstimeseriesSMA3 <- SMA(kingstimeseries,n=3)
kingstimeseriesSMA8 <- SMA(kingstimeseries,n=8)

kingtimeseriesdiff1 <- diff(kingstimeseries, differences=1)

library("forecast") # load the "forecast" R library
kingstimeseriesarima <- arima(kingstimeseries, order=c(0,1,1)) # fit an ARIMA(0,1,1) model
kingstimeseriesforecasts <- forecast.Arima(kingstimeseriesarima, h=5)

acf(kingtimeseriesdiff1, lag.max=20, plot=TRUE)
pacf(kingtimeseriesdiff1, lag.max=20, plot=TRUE)

acf(kingstimeseriesforecasts$residuals, lag.max=20, plot=TRUE)

######################################################################

births <- scan("http://robjhyndman.com/tsdldata/data/nybirths.dat")
birthstimeseries <- ts(births, frequency=12, start=c(1946,1))

birthstimeseriescomponents <- decompose(birthstimeseries)
birthstimeseriesseasonallyadjusted <- birthstimeseries - birthstimeseriescomponents$seasonal

######################################################################

souvenir <- scan("http://robjhyndman.com/tsdldata/data/fancy.dat")
souvenirtimeseries <- ts(souvenir, frequency=12, start=c(1987,1))

logsouvenirtimeseries <- log(souvenirtimeseries)
souvenirtimeseriesforecasts <- HoltWinters(logsouvenirtimeseries)

souvenirtimeseriesforecasts2 <- forecast.HoltWinters(souvenirtimeseriesforecasts, h=48)
acf(souvenirtimeseriesforecasts2$residuals, lag.max=20, plot=FALSE)

######################################################################

rain <- scan("http://robjhyndman.com/tsdldata/hurst/precip1.dat",skip=1)
rainseries <- ts(rain,start=c(1813))

rainseriesforecasts <- HoltWinters(rainseries, beta=FALSE, gamma=FALSE)
rainseriesforecasts$fitted

rainseriesforecasts2 <- forecast.HoltWinters(rainseriesforecasts, h=8)
acf(rainseriesforecasts2$residuals, lag.max=20, plot=FALSE)

######################################################################

skirts <- scan("http://robjhyndman.com/tsdldata/roberts/skirts.dat",skip=5)

skirtsseries <- ts(skirts,start=c(1866))

skirtsseriesforecasts <- HoltWinters(skirtsseries, gamma=FALSE)
skirtsseriesforecasts2 <- forecast.HoltWinters(skirtsseriesforecasts, h=19)
plot.ts(skirtsseriesforecasts2$residuals)  

skirtsseriesdiff1 <- diff(skirtsseries, differences=1)
skirtsseriesdiff2 <- diff(skirtsseries, differences=2)

acf(skirtsseriesforecasts2$residuals, lag.max=20, plot=FALSE)
######################################################################

volcanodust <- scan("http://robjhyndman.com/tsdldata/annual/dvi.dat", skip=1)

volcanodustseries <- ts(volcanodust,start=c(1500))


acf(volcanodustseries, lag.max=20, plot=FALSE)
pacf(volcanodustseries, lag.max=20, plot=FALSE)

volcanodustseriesarima <- arima(volcanodustseries, order=c(2,0,0))
volcanodustseriesforecasts <- forecast.Arima(volcanodustseriesarima, h=31)


######################################################################


# dataexport <- data.frame(kingstimeseries)
# dataexport <- data.frame(birthstimeseries)
# dataexport <- data.frame(souvenirtimeseries)
# dataexport <- data.frame(logsouvenirtimeseries)
# dataexport <- data.frame(kingstimeseriesSMA3)
# dataexport <- data.frame(kingstimeseriesSMA8)
# dataexport <- data.frame(rainseries)
# dataexport <- data.frame(skirtsseries)
# dataexport <- data.frame(volcanodustseries)

# dataexport <- data.frame(birthstimeseriescomponents$x)
# dataexport <- data.frame(birthstimeseriescomponents$seasonal)
# dataexport <- data.frame(birthstimeseriescomponents$trend)
# dataexport <- data.frame(birthstimeseriescomponents$random)
# dataexport <- data.frame(birthstimeseriesseasonallyadjusted)

# dataexport <- data.frame(rainseriesforecasts$fitted)
# dataexport <- data.frame(rainseriesforecasts2)
# dataexport <- data.frame(rainseriesforecasts2$residuals)

# dataexport <- data.frame(skirtsseriesforecasts$fitted)
# dataexport <- data.frame(skirtsseriesforecasts2)
# dataexport <- data.frame(skirtsseriesforecasts2$residuals)

# dataexport <- data.frame(souvenirtimeseriesforecasts$fitted)
# dataexport <- data.frame(souvenirtimeseriesforecasts2)
# dataexport <- data.frame(souvenirtimeseriesforecasts2$residuals)

# dataexport <- data.frame(skirtsseriesdiff1)
# dataexport <- data.frame(skirtsseriesdiff2)
# dataexport <- data.frame(kingtimeseriesdiff1)

# dataexport <- data.frame(kingstimeseriesforecasts)
# dataexport <- data.frame(kingstimeseriesforecasts$residuals)

# dataexport <- data.frame(volcanodustseriesforecasts)
# dataexport <- data.frame(volcanodustseriesforecasts$residuals)


volcanodustseriesforecasts
kingstimeseriesforecasts$residuals

write.csv(dataexport, file='C:/Users/Derek/Documents/RPackages/Time Series/dataexport.csv', row.names=T)
