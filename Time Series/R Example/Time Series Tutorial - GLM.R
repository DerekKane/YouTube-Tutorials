# Time Series Tutorial - Generalized Linear Regression

#######################################################################

# http://cran.r-project.org/doc/contrib/Fox-Companion/appendix-timeseries-regression.pdf

setwd("C:/Users/Derek/Documents/RPackages/Time Series")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load the Hartnagel dataset from the Car package
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(car)

data(Hartnagel)
mydata <- Hartnagel

attach(mydata)

#######################################################################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Plot Womens Conviction Rate Across Time
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

plot(mydata$year, mydata$fconvict, type="o", ylab="Convictions Per 100,000 Women")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Preliminary Multiple Linear Regression under OLS Assumptions 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

OLSRegress <- lm(fconvict ~ tfr + partic + degrees + mconvict)

summary(OLSRegress)

# note: R2 is pretty strong; degrees & mconvict are not significant;
# womens crime rate (fconvict) declines with fertility (tfr) and 
# increases with labor force participation (partic).

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Residual Plot of Residuals
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

plot(year, residuals(OLSRegress), type="o")
abline(h=0, lty=2)

# the spread of residuals indicate that autocorrelation is 
# significant in the model and needs to be accounted for before 
# we can use the model.

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ACF and PACF Review
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

acf(residuals(OLSRegress),lag.max=20, plot=TRUE)
pacf(residuals(OLSRegress),lag.max=20, plot=TRUE)


acf(residuals(OLSRegress),lag.max=20, plot=FALSE)
pacf(residuals(OLSRegress),lag.max=20, plot=FALSE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Durbin-Watson Statistic
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

durbinWatsonTest(OLSRegress, max.lag=5)

# Three of the first 5 autocorrelations are significant, including the first.

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Develop the GLM Model
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library("nlme")

# Set the decimal length so that we can see the coefficents correctly.

options(digits=5)

GLMRegress <- gls(fconvict ~ tfr + partic + degrees + mconvict,
                  correlation=corARMA(p=2), method="ML")

summary(GLMRegress)

# Specifying the correlation structure as corARMA(p=2) fits an
# AR(2) process for the errors. That is the Moving Average component
# is implicitly of order q=0, and hence is absent.

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Statistical Tests to determine correct model 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Likelihood Tests and ANOVA

GLMRegress.AR0 <- update(GLMRegress, correlation=NULL)
GLMRegress.AR1 <- update(GLMRegress, correlation=corARMA(p=1))
GLMRegress.AR3 <- update(GLMRegress, correlation=corARMA(p=3))

# A lower AIC indicates a superior modeling approach.

anova(GLMRegress, GLMRegress.AR0, GLMRegress.AR1, GLMRegress.AR3)

# these two are close in AIC but the AR2 is simplier than AR3 
# therefore we will use this model.

anova(GLMRegress, GLMRegress.AR3)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Forecast
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

GLMRegress <- gls(fconvict ~ tfr + partic + mconvict + 0,
                  correlation=corARMA(p=2), method="ML")

# After running the regression, We also need to create the predicted values and residuals for the reg1 model.

GLMRegress_hat<-fitted(GLMRegress) # predicted values
as.data.frame(GLMRegress_hat)

OLSRegress_hat<-fitted(OLSRegress) # predicted values
as.data.frame(OLSRegress_hat)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Export the datasets
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

dataexport <- data.frame(mydata)
dataexport <- data.frame(OLSRegress_hat)

write.csv(dataexport, file='C:/Users/Derek/Documents/RPackages/Time Series/dataexport.csv', row.names=T)

