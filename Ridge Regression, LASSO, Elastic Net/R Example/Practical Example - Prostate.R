#####################################################################################
# Ridge Regression, LASSO, and Elastic Net Tutorial
#####################################################################################

# install.packages("ridge")


library(ridge)
library(glmnet)
library(lasso2)
library(colorspace)
library(lars)
library(elasticnet)
library(MASS)


data(Prostate)
mydata <- Prostate

# Convert the dataset into a matrix for further processing.

y <- as.numeric(Prostate[,9])
x <- as.matrix(Prostate[,1:8])


#####################################################################################
# Ridge Regression Tutorial.
#####################################################################################

# Using R's automatic selection methods to select the biasing constant:
# R calls this constant "lambda"

select(lm.ridge(lpsa~ lcavol + lweight + age + lbph + svi + lcp + gleason + pgg45, 
                data=mydata, lambda = seq(0,1,0.001)))

# The generalized cross-validation (GCV) criterion says
# the optimal biasing constant is 1. The value of 1 here is telling us that we need
# to extend the lambda sequence to a larger range.

select(lm.ridge(lpsa~ lcavol + lweight + age + lbph + svi + lcp + gleason + pgg45, 
                data=mydata, lambda = seq(0,10,0.1)))


# now we have a GVC = 6.5 specified for lambda in this case which falls in the 
# lambda range. This is good.

#####################################################################################
# Ridge Model
#####################################################################################

# We will create a train model with a range of values for lambda instead of 
# an exact value. This will help us create our diagnostic plots.
# Later we will specify the lambda in the model.


ridge.train <- lm.ridge(lpsa~ lcavol + lweight + age + lbph + svi + lcp + gleason + pgg45,
                        data=mydata, lambda = seq(0,10,0.1))




#####################################################################################
# Ridge GVC Plot
#####################################################################################

plot(seq(0,10,0.1), ridge.train$GCV, main="GCV of Ridge Regression", type="l", 
     xlab=expression(lambda), ylab="GCV")

# The optimal lambda is given by

lambda.ridge <- seq(0,10,0.1)[which.min(ridge.train$GCV)]

# Lets add the line to our plot
abline(v=lambda.ridge, lty=2, col="red")


# Now lets take a look at the Ridge from our train model.

colors <- rainbow_hcl(8, c = 65, l = 65) # 8 is the number or ind. variables

plot.new()
matplot(seq(0,10,0.1), coef(ridge.train)[,-1], xlim=c(0,11), type="l",xlab=expression(lambda), 
        ylab=expression(hat(beta)), col=colors, lty=1, lwd=2, main="Ridge coefficients")
abline(v=lambda.ridge, lty=2)
text(rep(10, 9), coef(ridge.train)[length(seq(0,10,0.1)),-1], colnames(mydata)[-9], pos=4, col=colors)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Here is a graph that is more visually friendly for novice ridge users.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

threshold <- seq(0,1000,0.1)

ridge.train2 <- lm.ridge(lpsa~ lcavol + lweight + age + lbph + svi + lcp + gleason + pgg45,
                         data=mydata, lambda = threshold)



colors <- rainbow_hcl(8, c = 65, l = 65) # 8 is the number or ind. variables

plot.new()
matplot(threshold, coef(ridge.train2)[,-1], xlim=c(0,1005), type="l",xlab=expression(lambda), 
        ylab=expression(hat(beta)), col=colors, lty=1, lwd=2, main="Ridge coefficients")
abline(v=lambda.ridge, lty=2)
text(rep(10, 9), coef(ridge.train2)[length(threshold),-1], colnames(mydata)[-9], pos=4, col=colors)



#####################################################################################
# Final Ridge Model
#####################################################################################

# now we have a GVC = 6.5 specified for lambda in this case which falls in the 
# lambda range. This is good.

ridge.reg <- lm.ridge(lpsa~ lcavol + lweight + age + lbph + svi + lcp + gleason + pgg45,
                      data=mydata, lambda = lambda.ridge)

# Printing the ridge-regression coefficient estimates for this problem:

ridge.reg




#####################################################################################
# Predict Results 
#####################################################################################

# There's no predict() method for "ridgelm" objects. We have to build the formula by
# hand. 

pred.ridge <- coef(ridge.reg)[1] + coef(ridge.reg)[2]*mydata[,1] + 
  coef(ridge.reg)[3]*mydata[,2] + coef(ridge.reg)[4]*mydata[,3] + 
  coef(ridge.reg)[5]*mydata[,4] + coef(ridge.reg)[6]*mydata[,5] + 
  coef(ridge.reg)[7]*mydata[,6] + coef(ridge.reg)[8]*mydata[,7] +
  coef(ridge.reg)[9]*mydata[,8]

# or we can predict a variable.

mydata$YHat <- coef(ridge.reg)[1] + coef(ridge.reg)[2]*mydata[,1] + 
  coef(ridge.reg)[3]*mydata[,2] + coef(ridge.reg)[4]*mydata[,3] + 
  coef(ridge.reg)[5]*mydata[,4] + coef(ridge.reg)[6]*mydata[,5] + 
  coef(ridge.reg)[7]*mydata[,6] + coef(ridge.reg)[8]*mydata[,7] +
  coef(ridge.reg)[9]*mydata[,8]

MSE.Ridge<-(sum((mydata$lpsa-pred.ridge)^2))/nrow(mydata)
# RMSE.Ridge<-sqrt((sum((mydata$lpsa-pred.ridge)^2))/nrow(mydata))

MSE.Ridge

# 0.4601

###############################################################################
# Build a Lasso using Glmnet
###############################################################################

# Remember that an alpha=1 is a LASSO, 0 is a Ridge, and everything between 
# is elastic net 

lassoreg.cv <- cv.glmnet(x,y, alpha=1) 

# Plot the fit

plot(lassoreg.cv)

# calculate the minimum lambda

lassoreg.cv$lambda.min

# The lambda value is 0.03914843 for my example. The value may differ upon runs.

###############################################################################
# Compute the lasso predictions

lassofits <- glmnet(x,y,alpha=1, nlambda=100)

# plot the fits

plot(lassofits)
plot(lassofits, xvar = "lambda", label = TRUE)
plot(lassofits, xvar = "dev", label = TRUE)

# Calculate the predictions with the minimal lambda

lassopred <- predict(lassofits,x,s= lassoreg.cv$lambda.min)
# mydata$yhat <- predict(lassofits,x,s= lassoreg.cv$lambda.min)


# Here is how we find the coefficients for the Lasso.

lassocoef <- predict(lassofits,x,s=lassoreg.cv$lambda.min, type="coefficients")

lassocoef

# notice that the lcp was removed by the lasso (it has a value of 0)

# Lets calculate the prediction error.

sum1 =0
tt<-nrow(mydata) # testset

for (i in 1:tt){
  sum1 <- sum1 + (lassopred[i]-mydata[i,9])^2
}

sumg <- sum1/tt
sumg

# 0.4681

# Therefore the error of the lasso is 0.46818


###############################################################################
# Build an elastic net using glmnet
###############################################################################

# alpha between 0 and 1 are examples of the elastic net.
# in this example we will use 0.2

elasticreg.cv <- cv.glmnet(x,y, alpha=0.2) 

# Plot the fit

plot(elasticreg.cv)


# determine the minimum lambda

elasticreg.cv$lambda.min

# 0.1120109

###############################################################################

# Compute the elastic net predictions

elasticfits <- glmnet(x,y,alpha=0.2, nlambda=100)


# plot the fits

plot(elasticfits)
plot(elasticfits, xvar = "lambda", label = TRUE)
plot(elasticfits, xvar = "dev", label = TRUE)

# Calculate the predictions with the minimal lambda


elasticpred <- predict(elasticfits,x,s= elasticreg.cv$lambda.min)

# Here is how we find the coefficients for the Elastic.

elasticcoef <- predict(elasticfits,x,s=elasticreg.cv$lambda.min, type="coefficients")

elasticcoef

# notice that no variables were removed by the elastic net.

# Lets calculate the prediction error.

sum1 =0
tt<-nrow(mydata) # testset

for (i in 1:tt){
  sum1 <- sum1 + (elasticpred[i]-mydata[i,9])^2
}

sumg <- sum1/tt
sumg

# 0.4715593

###############################################################################
# Multiple Linear Regression

ridge.linreg<- lm(lpsa~ lcavol + lweight + age + lbph + svi + lcp + gleason + pgg45,
                      data=mydata)

summary(ridge.linreg)


# Lets drop the variables greater than p = 0.05

ridge.linreg2<- lm(lpsa~ lcavol + lweight + svi,
                  data=mydata)

summary(ridge.linreg2)

# Lets calculate the prediction error.

linearpred <- predict(ridge.linreg2, newdata = mydata, type = "response")

# Lets calculate the predictive performance now.

sum1 =0
tt<-nrow(mydata) # testset

for (i in 1:tt){
  sum1 <- sum1 + (linearpred[i]-mydata[i,9])^2
}

sumg <- sum1/tt
sumg
