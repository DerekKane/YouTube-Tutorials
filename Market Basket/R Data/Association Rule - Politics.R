#####################################################################
# Association Rule - Politics
#####################################################################

setwd("C:/Users/Derek/Documents/RPackages/Association Rules")

mydata <- read.csv("voting4.csv")

mydata2 <- mydata
mydata3 <- mydata

#####################################################################
# Load Library
#####################################################################

library(arules)
library(arulesViz)

#####################################################################
# First analysis - Democrat
#####################################################################

mydata2$Republican <- NULL


# we must first reshape the data by flipping it on its side.

# this will cross tabluate the inital table

mydata2b <- data.matrix(mydata2)


mydata_flip <- as.data.frame.matrix(mydata2b, stringsAsFactors = default.stringsAsFactors())

mydata_flip2 <- as(as.matrix(mydata_flip), "transactions")

#####################################################################
# Targeting specific items
#####################################################################

# Now that we know how to generate rules, limit the output, lets say we wanted to target items to generate rules. 
# There are two types of targets we might be interested in that are illustrated with an example of "whole milk":

# What are customers likely to buy before buying item x?
# What are customers likely to buy if they purchase item x?

rules<-apriori(data=mydata_flip2, parameter=list(supp=0.001,conf = 0.001, maxlen=4), 
               appearance = list(default="lhs",rhs="Democrat"),
               control = list(verbose=F))
rules<-sort(rules, decreasing=TRUE,by="confidence")


# We can eliminate these repeated rules using the follow snippet of code:

subset.matrix <- is.subset(rules, rules)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
rules.pruned <- rules[!redundant]
rules<-rules.pruned

dataexport<- as(rules, "data.frame")

#####################################################################

# dataexport<- as(rules, "data.frame");
# write.csv(dataexport, file='C:/Users/Derek/Documents/RPackages/Association Rules/dataexport.csv', row.names=T)

#####################################################################
# Second analysis - Republican
#####################################################################

mydata3$Democrat <- NULL


# we must first reshape the data by flipping it on its side.

# this will cross tabluate the inital table

mydata3b <- data.matrix(mydata3)


mydata_flip <- as.data.frame.matrix(mydata3b, stringsAsFactors = default.stringsAsFactors())

mydata_flip2 <- as(as.matrix(mydata_flip), "transactions")

#####################################################################
# Targeting specific items
#####################################################################

# Now that we know how to generate rules, limit the output, lets say we wanted to target items to generate rules. 
# There are two types of targets we might be interested in that are illustrated with an example of "whole milk":

# What are customers likely to buy before buying item x?
# What are customers likely to buy if they purchase item x?

rules<-apriori(data=mydata_flip2, parameter=list(supp=0.001,conf = 0.001, maxlen=4), 
               appearance = list(default="lhs",rhs="Republican"),
               control = list(verbose=F))
rules<-sort(rules, decreasing=TRUE,by="confidence")


# We can eliminate these repeated rules using the follow snippet of code:

subset.matrix <- is.subset(rules, rules)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
rules.pruned <- rules[!redundant]
rules<-rules.pruned

dataexport<- as(rules, "data.frame")

#####################################################################

# dataexport<- as(rules, "data.frame");
# write.csv(dataexport, file='C:/Users/Derek/Documents/RPackages/Association Rules/dataexport.csv', row.names=T)

