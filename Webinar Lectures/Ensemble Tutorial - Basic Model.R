#################################################################################
# Ensemble Tutorial - Basic Example
#################################################################################

# This is how I am creating a sample dataset using a bagging technique.
# Notice that X2 and X3 contain distinct nonlinear tendencies

set.seed(10)
y<-c(1:1000)
x1<-c(1:1000)*runif(1000,min=0,max=2)
x2<-(c(1:1000)*runif(1000,min=0,max=2))^2
x3<-log(c(1:1000)*runif(1000,min=0,max=2))

mydata2 <- data.frame(y, x1, x2, x3)

mydata2


#################################################################################
# Load the libraries
#################################################################################

install.packages("Rtools")



library(randomForest)
library(e1071)

#################################################################################
# Data Split
#################################################################################

set.seed(1234)
ind <- sample(2, nrow(mydata2), replace=TRUE, prob=c(0.7, 0.3))
trainData2 <- mydata2[ind==1,]
testData2 <- mydata2[ind==2,]



#################################################################################
# Build the basic models
#################################################################################


# This is a multiple linear regression model to see the fit

lm_fit<-lm(y~x1+x2+x3, data=trainData2)
lm_predictions<-predict(lm_fit,testData2)
regerror<-sqrt((sum((testData2$y-lm_predictions)^2))/nrow(testData2))

# This is a Random Forest ensemble model

rf_fit<-randomForest(y~x1+x2+x3,data=trainData2,ntree=500)
rf_predictions<-predict(rf_fit,newdata=testData2)
RFerror<-sqrt((sum((testData2$y-rf_predictions)^2))/nrow(testData2))

# This is a SVM model

svm_fit<-svm(y~x1+x2+x3,data=trainData2)
svm_predictions<-predict(svm_fit,newdata=testData2)
SVMerror<-sqrt((sum((testData2$y-svm_predictions)^2))/nrow(testData2))


# The different errors for the basic models.

regerror # Regression Error is 165.02
RFerror # Random Forest Error is 133.30
SVMerror # SVM Error is 127.83


# SVM is the best performer. Lower is better.

#################################################################################
# Ensemble Technique
#################################################################################

# Ensemble with 50% regression and 50% Random Forest

predictions<-(lm_predictions+rf_predictions)/2 # This creates the 50% and 50% weighting
Ens1error<-sqrt((sum((testData2$y-predictions)^2))/nrow(testData2))

# Ensemble with 50% Random Forest and 50% SVM

predictions<-(svm_predictions+rf_predictions)/2
Ens2error<-sqrt((sum((testData2$y-predictions)^2))/nrow(testData2))


# The different errors for all the models.

regerror # Regression Error is 165.02
RFerror # Random Forest Error is 133.30
SVMerror # SVM Error is 127.83

Ens1error # Ensemble build 1 error is 139.958
Ens2error # Ensemble build 2 error is 127.16

# The 2nd ensemble technique is better than the individual performance of the models.
