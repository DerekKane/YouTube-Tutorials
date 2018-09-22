# Neural Network - Classification Multiple Variables Example
############################################################################

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Set a Working Directory
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

setwd("C:/Users/Derek/Documents/RPackages/Neural Network")

############################################################################

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load Libraries
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library("neuralnet")

mydata <- iris

############################################################################

# The dataset contains information on different clients who received a loan at least 10 years ago. 
# The variables income (yearly), age, loan (size in euros) and LTI (the 
# loan to yearly income ratio) are available. Our goal is to devise 
# a model which predicts, based on the input variables LTI and age, 
# whether or not a default will occur within 10 years.


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Test and Training Sets
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

set.seed(1234567890)

# Create  the dummy variables for the Species

mydata$setosa <- as.numeric(mydata$Species == "setosa")
mydata$versicolor <- as.numeric(mydata$Species == "versicolor")
mydata$virginica <- as.numeric(mydata$Species == "virginica")


## extract a set to train the NN
trainset <- mydata[1:100, ]

## select the test set
testset <- mydata[101:150, ]


# Now we'll build a neural network with 4 hidden nodes (a neural 
# network is comprised of a input, hidden and output nodes). 
# The number of nodes is chosen here without a clear method, 
# however there are some rules of thumb. The lifesign option 
# refers to the verbosity. The ouput is not linear and we will use a 
# threshold value of 10%. The neuralnet package uses resilient 
# backpropagation with weight backtracking as its standard algorithm.


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Build a Neural Network
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Ex. 1 - Build the neural network which will fail. 
# speciesnet <- neuralnet(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, trainset, hidden = 4, lifesign = "minimal", 
#                       linear.output = FALSE, threshold = 0.1)

## Ex. 2 - To fix the NN, we need to hard code the different levels of the species 
# into the example.

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Train the neural network
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

speciesnet <- neuralnet(setosa + versicolor + virginica ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, trainset, hidden = 4, lifesign = "minimal", 
                        linear.output = FALSE, threshold = 0.1)



## plot the NN
plot(speciesnet, rep = "best")


# Once we've trained the neural network we are ready to test it. We use the testset 
# subset for this. The compute function is applied for computing 
# the outputs based on the LTI and age inputs from the testset.


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Test the neural network
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

mydata$Species2 <- factor(mydata$setosa + mydata$versicolor*2 + mydata$virginica*3, labels=names(mydata)[6:8]) 

#

# temp_test <- subset(testset, select = c("LTI", "age"))

# This will run the temp_test dataset through the NN we created on the
# training data.

speciesnet.results <- compute(speciesnet, testset)

# Let's have a look at what the neural network produced:

results <- data.frame(actual = testset$default10yr, prediction = creditnet.results$net.result)
results[100:115, ]

# We can round to the nearest integer to improve readability:

results$prediction <- round(results$prediction)
results[100:115, ]


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# View the neural net options parameters

names(creditnet)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# result matrix

creditnet$result.matrix

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# The given data is saved in creditnet$covariate and
# creditnet$response as well as in creditnet$data for the whole data
# set inclusive non-used variables. The output of the
# neural network, i.e. the fitted values o(x), is provided
# by creditnet$net.result:

out <- cbind(creditnet$covariate,creditnet$net.result[[1]])

dimnames(out) <- list(NULL, c("age", "parity","induced","spontaneous","nn-output"))

head(out)

# generalized weights

# The generalized weight expresses the effect of each
# ovariate xi and thus has an analogous interpretation
# as the ith regression parameter in regression models.
# However, the generalized weight depends on all
# other covariates. Its distribution indicates whether
# the effect of the covariate is linear since a small variance
# suggests a linear effect

# The columns refer to the four covariates: age (j =
# 1), parity (j = 2), induced (j = 3), and spontaneous (j=4)

head(creditnet$generalized.weights[[1]])

# visualization

plot(creditnet, rep="best")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Saving a dataframe as .csv
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

write.csv(mydata, file='C:/Users/Derek/Documents/RPackages/Neural Network/ResultsBinary.csv', row.names=F)
