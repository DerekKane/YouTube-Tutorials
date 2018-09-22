# Association Rule - Groceries

#####################################################################

# We need to establish the working directory where the file is located within.
# set the working directory

setwd("C:/Users/Derek/Documents/RPackages/Association Rules")

mydata <- read.csv("Transaction2.csv")

#####################################################################
# Load Library
#####################################################################

library(arules)
library(arulesViz)

# we must first reshape the data by flipping it on its side.

# this will cross tabluate the inital table

mydata2 <- table(mydata$Transaction, mydata$Items)

mydata_flip <- as.data.frame.matrix(mydata2)
mydata_flip[is.na(mydata_flip)] <- 0

mydata_flip2 <- as(as.matrix(mydata_flip), "transactions")

#####################################################################
# this will create the association Rules.
#####################################################################

rules = apriori(mydata_flip2, parameter=list(supp=0.001, conf=0.8, target="rules")) 


#####################################################################
# this will prune the rules to create a smaller ruleset.
#####################################################################

# The first issue we see here is that the rules are not sorted. 
# Often we will want the most relevant rules first. Lets say we wanted to have the most likely rules. 
# We can easily sort by confidence by executing the following code:

rules<-sort(rules, by="confidence", decreasing=TRUE)

# Some Rules are perhaps excessively long. Lets say you wanted more concise rules. 
# That is also easy to do by adding a "maxlen" parameter to your apriori function:

rules <- apriori(mydata_flip2, parameter = list(supp = 0.001, conf = 0.8,maxlen=3))
inspect(rules[1:5])

# Sometimes, rules will repeat. Redundancy indicates that one item might 
# be a given. As an analyst you can elect to drop the item from the dataset. 
# Alternatively, you can remove redundant rules generated. 

# We can eliminate these repeated rules using the follow snippet of code:

subset.matrix <- is.subset(rules, rules)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
rules.pruned <- rules[!redundant]
rules<-rules.pruned

############################################################
# dataexport<- as(rules, "data.frame");
# write.csv(dataexport, file='C:/Users/Derek/Documents/RPackages/Association Rules/dataexport.csv', row.names=T)


#####################################################################
# Targeting specific items
#####################################################################

# Now that we know how to generate rules, limit the output, lets say we wanted to target items to generate rules. 
# There are two types of targets we might be interested in that are illustrated with an example of "whole milk":

# What are customers likely to buy before buying item x?
# What are customers likely to buy if they purchase item x?

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create a datset with 2 variables for later
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

variables = c("yogurt", "bottled beer", "tropical fruit")
testdata <- data.frame(variables)

#Create a temp table to hold the results

temp.ts <- data.frame(rules=character(),
                      support=numeric(),
                      confidence=numeric(),
                      lift=numeric(), 
                      variables=character(), 
                      stringsAsFactors=FALSE)


# Answering the first question we adjust our apriori() function as follows:
# Looping Function

for(i in 1:nrow(testdata)) {
  rules<-apriori(mydata_flip2, parameter=list(supp=0.001,conf = 0.08, minlen=2), 
                 appearance = list(default="lhs",rhs=testdata$variables[i]),
                 control = list(verbose=F))
  dataexport<- as(rules, "data.frame")
  dataexport$variables <- testdata$variables[i]
  temp.ts <- rbind(temp.ts, dataexport)
}

temp.ts

############################################################

# write.csv(temp.ts, file='C:/Users/Derek/Documents/RPackages/Association Rules/dataexport.csv', row.names=T)

# Likewise, we can set the left hand side to be item x and find its antecedents.

# Looping Function

for(i in 1:nrow(testdata)) {
  rules<-apriori(mydata_flip2, parameter=list(supp=0.001,conf = 0.08, minlen=2), 
                 appearance = list(default="rhs",lhs=testdata$variables[i]),
                 control = list(verbose=F))
  dataexport<- as(rules, "data.frame")
  dataexport$variables <- testdata$variables[i]
  temp.ts <- rbind(temp.ts, dataexport)
}

temp.ts

############################################################


# write.csv(temp.ts, file='C:/Users/Derek/Documents/RPackages/Association Rules/dataexport.csv', row.names=T)


