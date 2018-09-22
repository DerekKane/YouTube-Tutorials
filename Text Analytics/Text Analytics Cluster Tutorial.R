# Text Analytics Tutorial - Clustering Unknown News Feed
############################################################################

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Set a Working Directory
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

setwd("C:/Users/Derek/Documents/RPackages/Text Mining")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load the dataset
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Set Options - Important for text analytics

options(stringAsFactors = FALSE)


mydata <- read.csv("TextAnalysis.csv")

#########################################################################
# Step 1. - Initialize the packages.
#########################################################################

library("tm")
library("plyr")
library("class")
library("NLP")
library("SnowballC")
library("sentiment")
library("wordcloud")


########################################################################
# Remove Unneeded columns from table
########################################################################

# Duplicated the mydata dataframe.

mydata2 <- mydata

########################################################################
# Remove Unneeded columns from table
########################################################################

mydata2$title <- NULL
mydata2$News.Feed <- NULL
mydata2$Extraction.Date <- NULL
mydata2$X <- NULL


########################################################################
# Create a corpus
########################################################################

Corpus.mydata <- Corpus(VectorSource(mydata2$description))


########################################################################
# Corpus Cleanup
########################################################################

Corpus.mydata <- tm_map(Corpus.mydata, removeNumbers)
Corpus.mydata <- tm_map(Corpus.mydata, tolower)
Corpus.mydata <- tm_map(Corpus.mydata, removeWords, stopwords("english"))
Corpus.mydata <- tm_map(Corpus.mydata, removePunctuation)
Corpus.mydata <- tm_map(Corpus.mydata, stripWhitespace)

inspect(Corpus.mydata[1:3])


#########################################################################
# Step 3. - Generate the TDM.
#########################################################################

# Matrix with columns as the documents and rows as the terms.
# Corpus.TDM <- TermDocumentMatrix(Corpus.mydata)

# Matrix with columns as the terms and rows as the documents.
Corpus.TDM <- DocumentTermMatrix(Corpus.mydata)

# Remove Sparse Terms from Corpus.TDM
Corpus.TDM <- removeSparseTerms(Corpus.TDM, 0.99)

#########################################################################
# Step 3b. - Understand the corpus by frequency and association. 
#########################################################################


findFreqTerms(Corpus.TDM, lowfreq=10)

# which words are associated with "world"?

findAssocs(Corpus.TDM, 'world', 0.30)

#########################################################################
# Step 4. - Create a result dataset for review
#########################################################################

# Create a result dataset for review

mydata.df <- as.data.frame(inspect(Corpus.TDM))


#########################################################################
# kNN Analysis
#########################################################################

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Optional: Create a scree plot

#accumulator for cost results
cost_df <- data.frame()

#run kmeans for all clusters up to 100
for(i in 1:100){
  #Run kmeans for each level of i, allowing up to 100 iterations for convergence
  kmeans<- kmeans(x=Corpus.TDM, centers=i, iter.max=100)
  
  #Combine cluster number and cost together, write to df
  cost_df<- rbind(cost_df, cbind(i, kmeans$tot.withinss))
  
}
names(cost_df) <- c("cluster", "cost")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Use optimal no. of clusters in k-means #

k1=8

# K-Means Cluster Analysis
fit <- kmeans(mydata.df, k1) 

# get cluster means
aggregate(mydata,by=list(fit$cluster),FUN=mean)

# rename fit.cluster to classif

classif <- fit$cluster

# append cluster assignment
mydata.df <- data.frame(mydata.df, classif)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Saving a dataframe as .csv
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

write.csv(mydata.df, file='C:/Users/Derek/Documents/RPackages/Text Mining/cluster.csv', row.names=F)

