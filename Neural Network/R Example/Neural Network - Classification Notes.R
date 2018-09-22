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

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create Dummy Variables for each Species
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

mydata[mydata$Species=="setosa","Output"]<- 2 
mydata[mydata$Species=="versicolor","Output"]<- 0 
mydata[mydata$Species=="virginica","Output"]<- 1 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create the Neural Network
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

nn1<-neuralnet(Output~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width,
               data=mydata,hidden=4,linear.output=FALSE) 


plot(nn1) 
nn1$result.matrix 



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Test and Training Sets
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

set.seed(1234567890)


## extract a set to train the NN
trainset <- mydata[1:100, ]

## select the test set
testset <- mydata[1:150, ]


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

speciesnet <- neuralnet(Output~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width,
                        data=trainset,hidden=4,linear.output=FALSE, threshold = 0.1)



## plot the NN
plot(speciesnet, rep = "best")


# Once we've trained the neural network we are ready to test it. We use the testset 
# subset for this. The compute function is applied for computing 
# the outputs based on the LTI and age inputs from the testset.


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Test the neural network
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# mydata$Species2 <- factor(mydata$setosa + mydata$versicolor*2 + mydata$virginica*3, labels=names(mydata)[6:8]) 

testset$Species = NULL
testset$Output = NULL


results.testset <- compute(speciesnet, testset)
testset$Output <- round(results.testset$net.result)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Saving a dataframe as .csv
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

write.csv(testset, file='C:/Users/Derek/Documents/RPackages/Neural Network/ResultsClass.csv', row.names=F)
