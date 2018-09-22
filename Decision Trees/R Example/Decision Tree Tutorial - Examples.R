###################################################################
# Decision Tree Tutorial 
###################################################################

# Set working Directory 

setwd("C:/Users/Derek/Dropbox/DKane R Tutorial/Decision Trees/R Example/")


# Load Libraries

library(caret)
library(rpart)
library(rpart.plot)

library(C50)
library(rattle)
library(party)
library(partykit)
library(RWeka)
library(randomForest)
library(ROCR)


# Load the data

diabetes <- read.csv("Diabetes.csv")
churn <- read.csv("Churn.csv")

mydata <- diabetes

##################################################################

# This code will split the dataset into a train and test set.
# We will use a 70% training & 30% testing split here. prob=c(0.7, 0.3))

set.seed(1234)
ind <- sample(2, nrow(mydata), replace=TRUE, prob=c(0.7, 0.3))
trainData <- mydata[ind==1,]
testData <- mydata[ind==2,]


##################################################################
# CART model building 
##################################################################

fit <- rpart(Class~.,method="class", data=trainData)

printcp(fit) # display the results 
plotcp(fit) # visualize cross-validation results 
summary(fit) # detailed summary of splits

# plot tree 

plot(fit, uniform=TRUE, 
     main="Classification Tree for Diabetes")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Prune the Decision Tree
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

pfit<- prune(fit, cp=fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])

# This code will prune based on the specified cp value.
# Ex. cp = 0.043 will produce a tree of size 3.
# pfit<- prune(fit, cp=0.043)


# plot the pruned tree 
plot(pfit, uniform=TRUE, 
     main="Pruned Classification Tree for Diabetes")
text(pfit, use.n=TRUE, all=TRUE, cex=.8)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Fancy plot of the decision tree with Rattle
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

fancyRpartPlot(pfit)
# fancyRpartPlot(fit)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Build a RPART decision tree using the party package
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

party.pfit <- as.party(pfit)
plot(party.pfit)

# party.fit <- as.party(fit)
# plot(party.fit)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ROCR and AUC
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

fit_testing <- predict(fit, testData, type="class")
confusionMatrix(fit_testing, testData$Class)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ROC for unpruned model
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

testData$YHat <- predict(fit, testData, type="prob")

fit.scores <- prediction(testData$YHat[,2], testData$Class)
fit.perf <- performance(fit.scores, "tpr", "fpr")

# Plot the ROC curve

plot(fit.perf, col = "green", lwd = 1.5)
abline(0,1,col="Red")
title("ROC Curve")

# AUC for the decision tree

fit.auc <- performance(fit.scores, "auc")
fit.auc


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ROC for pruned model
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

testData$YHat <- predict(pfit, testData, type="prob")

pfit.scores <- prediction(testData$YHat[,2], testData$Class)
pfit.perf <- performance(pfit.scores, "tpr", "fpr")

# Plot the ROC curve

plot(pfit.perf, col = "green", lwd = 1.5)
abline(0,1,col="Red")
title("ROC Curve")

# AUC for the decision tree

pfit.auc <- performance(pfit.scores, "auc")
pfit.auc


##################################################################
# Random Forest Modeling
##################################################################

trainData$Class <- as.factor(trainData$Class)

# Build the random forest model

RandomForestModel <- randomForest(Class~., data=trainData, ntree=500, mtry=5, importance=TRUE)
print(RandomForestModel)
importance(RandomForestModel)

plot.new()
plot(RandomForestModel, log="y")
varImpPlot(RandomForestModel)

plot.new()
varImpPlot(RandomForestModel, type=1, pch=19, col=1, cex=1.0, main="")
abline(v=13, col="blue")

varImpPlot(RandomForestModel, type=2, pch=19, col=1, cex=1.0, main="")
abline(v=35, col="blue")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ROC for Random Forest
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

testData$YHat <- predict(RandomForestModel, testData, type="prob")

fit.scores <- prediction(testData$YHat[,2], testData$Class)
fit.perf <- performance(fit.scores, "tpr", "fpr")

# Plot the ROC curve

plot(fit.perf, col = "green", lwd = 1.5)
abline(0,1,col="Red")
title("ROC Curve")

# AUC for the decision tree

fit.auc <- performance(fit.scores, "auc")
fit.auc

# Confusion Matrix

fit_testing <- predict(RandomForestModel, testData, type="class")
confusionMatrix(fit_testing, testData$Class)



###################################################################
# Build a decision tree using C5.0 for Churn
###################################################################

# The decision variable class must be converted into a factor
# variable in order for the C50 to process correctly.

churn$Churn <- as.factor(churn$Churn)

# Run the c50 algorithm for a decision tree.

c50_tree_result<-C5.0(Churn~.,data=churn)

# display the summary

summary(c50_tree_result)

C5imp(c50_tree_result,metric='usage')
C5imp(c50_tree_result,metric='splits')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Run the c50 algorithm and show the decision rules.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

c50_rule_result<-C5.0(Churn~.,data=churn, rules=TRUE)

# display the summary

summary(c50_rule_result)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Test c50 algo predictions on same dataset but ignoring Churn variable
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

predictions <- predict(c50_rule_result, churn[,1:20], type="class")
table(predictions, churn$Churn)
