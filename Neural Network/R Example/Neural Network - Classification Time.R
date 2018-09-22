# Neural Network - Continous Numeric Time Series Example
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
library("car")
library("caret")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load the Hartnagel dataset from the Car package
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

data(Hartnagel)
mydata <- Hartnagel

attach(mydata)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Train the Neural Network
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

NNTrain  <- avNNet(fconvict ~ tfr + partic + degrees + mconvict, data=mydata, 
                   repeats=25, size=5, decay=0.3,linout=TRUE)

# The avNNet function from the caret package fits a feed-forward neural network with one hidden layer. The network specified here contains three nodes (size=3) 
# in the hidden layer. The decay parameter has been set to 0.1. 
# The argument repeats=25 indicates that 25 networks were trained 
# and their predictions are to be averaged. The argument linout=TRUE 
# indicates that the output is obtained using a linear function. 

# Prediction of data

testdata <- mydata

# testdata$year = NULL
# testdata$ftheft = NULL
# testdata$mtheft = NULL
# testdata$fconvict = NULL

# Predict new values based on NN model.

testdata$Prediction <- predict(NNTrain, mydata)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Saving a dataframe as .csv
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

write.csv(testdata, file='C:/Users/Derek/Documents/RPackages/Neural Network/ResultsTime.csv', row.names=F)


