#################################################################################
# R Tutorial for Binary Classification Exercise.
#################################################################################

# Set the working directory.

setwd("C:/Users/Derek/Documents/RPackages/Predictive Model Example")


#################################################################################
# Load the libraries
#################################################################################

# Load R Libraries for the analysis

# install.packages("car")

library(ggplot2)
library(reshape2)
library(car)
library(corrplot)
library(e1071)
library(randomForest)
library(ROCR)
library(caret)


#################################################################################
# Load the dataset and Data Munging
#################################################################################

# Load the dataset from a csv file

mydata <- read.csv("breastcancer.csv")
attach(mydata)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Begin Data Munging Tasks
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

summary(mydata)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Remove the unnecessary variable


mydata$Sample.Code <- NULL

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Drop erroneous observations in the Bare.Nuclei variable labeled as "?"

Values <- c("1","2","3","4","5","6","7","8","9","10")

mydata <- mydata[mydata$Bare.Nuclei %in% Values,]

# Convert the Bare.Nuclei variable to integer from factor

mydata$Bare.Nuclei <- as.integer(mydata$Bare.Nuclei)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# the class variable (0 or 1) need to be converted into catgorical values

# Reencode variable from 0 to 1 to Cancer or No Cancer.

mydata$Class[mydata$Class=="0"] <- "No Cancer"
mydata$Class[mydata$Class=="1"] <- "Cancer"

# And Back again...

mydata$Class[mydata$Class=="No Cancer"] <- 0
mydata$Class[mydata$Class=="Cancer"] <- 1

mydata$Class <- as.integer(mydata$Class)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Handling of missing values.

# This will remove any observations that are missing from the dataset (NULL)  

na.omit(mydata)


#################################################################################
# Exploratory Data Analysis
#################################################################################

# Run a simple summary to determine if the dataset is complete

summary(mydata)


#This will provide the standard deviations in the data 
sapply(mydata, sd)

# Correlation Matrix

cormatrix  <- round(cor(mydata), digits=2)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Corrplot library of correlations graphics

corrplot(cormatrix)

# Heatmap of correlations using ggplot2

qplot(x=Var1, y=Var2, data=melt(cor(mydata, use="p")), 
      fill=value, geom="tile") +   scale_fill_gradient2(limits=c(-1, 1))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# R standard graphics
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# scatterplot for all variables


plot(mydata)
title('Scatterplot')

# scatterplot for the first 4 variables

plot.new()
plot(mydata[,1:4])
title('Scatterplot of first 4 variables')

plot.new()
plot(Uniformity.of.Cell.Size~Uniformity.of.Cell.Shape)
title('Basic Scatterplot')

#Histogram for Uniformity.of.Cell.Size

plot.new()
hist(Uniformity.of.Cell.Size)

plot.new()
boxplot(Uniformity.of.Cell.Size)
title('Boxplot of Cell Size')

#################################################################################
# Better graphics from the ggplot library

# ggplot2 of the scatterplot for 1 variable

ggplot(mydata, aes(x=Uniformity.of.Cell.Size, y=Uniformity.of.Cell.Shape)) + geom_point(size=2.5)

# Scatterplot for 1 variable grouped by Class

ggplot(mydata, aes(x=Uniformity.of.Cell.Size, y=Uniformity.of.Cell.Shape, colour=Class)) + geom_point(size=2.5)

# plot Matrix

plotmatrix(mydata[,1:4], colour="gray20") +
  geom_smooth(method="lm")


# GGPlot2 of Histogram
# overlay histogram, empirical density, and normal density

p0 = qplot(Uniformity.of.Cell.Size, geom = 'blank') + geom_line(aes(y = ..density.., colour = 'Empirical'), stat = 'density') + stat_function(fun = dnorm, aes(colour = 'Normal')) + geom_histogram(aes(y = ..density..), alpha = 0.4) + scale_colour_manual(name = 'Density', values = c('red', 'blue')) + opts(legend.position= c(0.85,0.85))

print(p0)

# Nice looking ggplot2 histogram

ggplot(mydata, aes(x=Uniformity.of.Cell.Size)) +
  geom_histogram(binwidth=1, fill="white", colour="black")

# Split by Class

ggplot(mydata, aes(x=Uniformity.of.Cell.Size)) +
  geom_histogram(binwidth=1, fill="white", colour="black") +
  facet_grid(Class ~ .)


#################################################################################
# Data splitting for training and testing
#################################################################################

# This code will split the dataset into a train and test set.
# We will use a 70% training & 30% testing split here. prob=c(0.7, 0.3))
# The set seed allows for the analysis to be recreated.

set.seed(1234)
ind <- sample(2, nrow(mydata), replace=TRUE, prob=c(0.7, 0.3))
trainData <- mydata[ind==1,]
testData <- mydata[ind==2,]


#################################################################################
# Model 1. - Logistic Regression Model
#################################################################################

# Select the variables to use based off of the forward selection procedure.
# Lower AIC indicates a better model.

# Forward Elimination
forwardtest <- step(glm(Class~1, data = trainData), direction="forward", scope=~ Clump.Thickness + Uniformity.of.Cell.Size + Uniformity.of.Cell.Shape + Marginal.Adhesion + 
                      Single.Epithelial.Cell.Size + Bare.Nuclei + Bland.Chromatin + Normal.Nucleoli + Mitoses )


#capture.output(forwardtest,file="test2b.doc")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Here is the logistic regression model based off of the results.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

mylogit <- glm(Class ~ Uniformity.of.Cell.Shape + Clump.Thickness + 0, data = trainData, family = "binomial")

summary(mylogit)

# capture.output(summary(mylogit),file="test2a.doc")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# this section will evaluate the models fit and performance.

# Residual Plot
residualPlots(mylogit, layout=c(1, 3))

# influence Plot
influenceIndexPlot(mylogit, vars=c("Cook", "hat"), id.n=3)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## CIs using profiled log-likelihood
confint(mylogit)

## CIs using standard errors
confint.default(mylogit)

## Put the coefficients and CI in a format onto a useful scale.

exp(mylogit$coefficients)
exp(confint(mylogit))

## odds ratios only
exp(coef(mylogit))

## odds ratios and 95% CI
exp(cbind(OR = coef(mylogit), confint(mylogit)))

#################################################################################
# Model 2. - Support Vector Machines
#################################################################################

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create a SVM for tuning
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


tuned <- tune.svm(Class ~ Clump.Thickness + Uniformity.of.Cell.Size + Uniformity.of.Cell.Shape
                  + Marginal.Adhesion + Single.Epithelial.Cell.Size + Bare.Nuclei
                  + Bland.Chromatin + Normal.Nucleoli + Mitoses,
                  data = trainData, gamma = 10^(-6:-1), cost = 10^(-1:1))


summary(tuned)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create a SVM from the tuned parameters
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

SVMModel <- svm(Class ~ Clump.Thickness + Uniformity.of.Cell.Size + 
                  Uniformity.of.Cell.Shape+ Marginal.Adhesion + 
                  Single.Epithelial.Cell.Size + Bare.Nuclei + 
                  Bland.Chromatin + Normal.Nucleoli + Mitoses, 
                data = trainData, gamma=0.1, cost=1)

print(SVMModel)
summary(SVMModel)



#################################################################################
# Model 3. - Random Forest Model
#################################################################################

RandomForestModel <- randomForest(Class ~ Clump.Thickness + Uniformity.of.Cell.Size + 
                                    Uniformity.of.Cell.Shape+ Marginal.Adhesion + 
                                    Single.Epithelial.Cell.Size + Bare.Nuclei + 
                                    Bland.Chromatin + Normal.Nucleoli + Mitoses, 
                                  data=trainData, ntree=500, mtry=5, importance=TRUE)

print(RandomForestModel)
importance(RandomForestModel)

plot.new()
varImpPlot(RandomForestModel)

plot.new()
varImpPlot(RandomForestModel, type=1, pch=19, col=1, cex=1.0, main="")
abline(v=20, col="blue")

plot.new()
varImpPlot(RandomForestModel, type=2, pch=19, col=1, cex=1.0, main="")
abline(v=20, col="blue")


#################################################################################
# Evaluate the models performance
#################################################################################

# this is the function which creates the prediction onto the new dataset.

LogisticModel <- predict(mylogit, testData, type="response")
SVMResult <- predict(SVMModel, testData, type="response")
RFResult <- predict(RandomForestModel, testData, type="response")

# This will create the results as a new column on the dataset.

testData$YHat1 <- predict(mylogit, testData, type="response")
testData$YHat2 <- predict(SVMModel, testData, type="response")
testData$YHat3 <- predict(RandomForestModel, testData, type="response")


# These are threshold parameter setting controls.

Predict <- function(t) ifelse(LogisticModel > t , 1,0) #t is the threshold for which the confusion
Predict2 <- function(t) ifelse(SVMResult > t , 1,0) #t is the threshold for which the confusion
Predict3 <- function(t) ifelse(RFResult > t , 1,0) #t is the threshold for which the confusion


confusionMatrix(Predict(0.5), testData$Class) # Logistic Regression
confusionMatrix(Predict2(0.5), testData$Class) # SVM
confusionMatrix(Predict3(0.5), testData$Class) # Random Forest


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ROC for unpruned model
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

pred1 <- prediction(testData$YHat1, testData$Class)
pred2 <- prediction(testData$YHat2, testData$Class)
pred3 <- prediction(testData$YHat3, testData$Class)

perf <- performance(pred1, "tpr", "fpr" )
perf2 <- performance(pred2, "tpr", "fpr")
perf3 <- performance(pred3, "tpr", "fpr")

plot.new()
plot(perf, col = "green", lwd = 2.5)
plot(perf2, add = TRUE, col = "blue", lwd = 2.5)
plot(perf3, add = TRUE, col = "orange", lwd = 2.5)
abline(0,1,col="Red", lwd=2.5, lty = 2)

title('ROC Curve')
legend(0.8,0.4,c("Logistic","SVM","RF"),        
lty=c(1,1,1), # gives the legend appropriate symbols (lines)
lwd=c(1.5,1.5,1.5),col=c("green","blue", "orange")) # gives the legend lines the correct color and width

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# AUC calculation metrics

fit.auc1 <- performance(pred1, "auc")
fit.auc2 <- performance(pred2, "auc")
fit.auc3 <- performance(pred3, "auc")

fit.auc1 # Logistic regression - AUC 0.8236
fit.auc2 # SVM - AUC 0.9981
fit.auc3 # Random Forest - AUC 0.9975


#################################################################################
# Additional: Ensemble Technique Basics
#################################################################################

# Create a new variable that contains an average of the 3 modeling results.
# 33% Logistic, 33% SVM, and 33% Random Forest

testData$YHat4 <- ((testData$YHat1 + testData$YHat2 + testData$YHat3)/3)

Threshold <- function(t) ifelse(testData$YHat4 > t , 1,0) #t is the threshold for which the confusion

confusionMatrix(Threshold(0.5), testData$Class) # Ensemble Technique

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

pred4 <- prediction(testData$YHat4, testData$Class)
perf4 <- performance(pred4, "tpr", "fpr")

plot(perf4, add = TRUE, col = "black", lwd = 2.5)


legend(0.8,0.4,c("Logistic","SVM","RF","Ensemble"),        
       lty=c(1,1,1,1), # gives the legend appropriate symbols (lines)
       lwd=c(1.5,1.5,1.5,1.5),col=c("green","blue","orange","black")) # gives the legend lines the correct color and width

fit.auc4 <- performance(pred4, "auc")
fit.auc4 # Ensemble Technique - AUC 0.9967

