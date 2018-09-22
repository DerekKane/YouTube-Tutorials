# Text Analytics Tutotrial - Presidential Speech
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#########################################################################

# Outline of approach.

# 1. Initialize the directory
# 2. Clean the Text
# 3. Build the Term Document Matrix (TDM) 
# 4. Attach candidates name for each TDM
# 5. Merge the 2 matrices into single dataset.
# 6. Create a holdout sample. (Training and Test Set)
# 7. Create the KNN model.
# 8. Assess the models performance.

#########################################################################

# Step 1. - Initialize the directory.

library("tm")
library("plyr")
library("class")

# alternative approach for multiple libraries.

# libs <- c("tm", "plyr", "class")
# lapply(libs, require, character.only = TRUE)

# Set Options

options(stringAsFactors = FALSE)

# Set parameters

candidates <- c("romney", "obama")
pathname <- "C:/Users/Derek/Documents/RPackages/Text Mining"

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Set a Working Directory
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

setwd("C:/Users/Derek/Documents/RPackages/Text Mining")

#########################################################################

# Step 2. - Clean the text by removing unneeded aspects. 
# Ex. punctuation, stopwords, etc...

# We will create a function to loop through all of the individual 
# documents in the Romney and Obama folders and clean them with different
# methods. These collection of documents are called a "corpus" and there
# is a corpus for obama and romney.

cleanCorpus <- function(corpus) {
  corpus.tmp <- tm_map(corpus, removePunctuation)
  corpus.tmp <- tm_map(corpus.tmp, stripWhitespace)
  corpus.tmp <- tm_map(corpus.tmp, tolower)
  corpus.tmp <- tm_map(corpus.tmp, removeWords, stopwords("english"))
  return(corpus.tmp)
}

# The corpus.tmp and CLeanCorpus will be used in Step 3 when creating
# the TDM.

#########################################################################

# Step 3. - Generate the TDM.

# This code below will take a single candiate (from the candidates variable)
# and the pathname and will put the candidates name at the end of the pathname.
# Additionally, this will loop through all of the documents in both directory
# Call the function in Step 2, convert the results into a document matrix
# and apply additional cleaning functions against the new single dataset.
# Ex.removeSparseTerms 

generateTDM <- function(candidates, pathname){
  s.dir <- sprintf("%s/%s", pathname, candidates)
  s.cor <- Corpus(DirSource(directory = s.dir, encoding = "ANSI"))
  s.cor.cl <- cleanCorpus(s.cor)
  s.tdm <- TermDocumentMatrix(s.cor.cl)
  s.tdm <- removeSparseTerms(s.tdm, 0.7)
  result <- list(name = candidates, tdm = s.tdm)
}

# Create a result dataset for review

tdm <- lapply(candidates, generateTDM, path = pathname)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Additional Notes:

# The "s.dir <- sprintf("%s/%s", path, cand)" code will concatenate the
# 2 directories into a single new directory

# The "s.cor" turns the "s.dir" into a new corpus to be used by the 
# generateTDM function.

# Note: the function in step 2, CleanCorpus, is incorporated as a step
# in the generateTDM function in step 3. The step is:  
# s.cor.cl <- cleanCorpus(s.cor)

#########################################################################

# Step 4. - Append the name of the candidate to the tdm we created in step 3.

bindCandidateToTDM <- function(tdm) {
  s.mat <- t(data.matrix(tdm[["tdm"]]))
  s.df <- as.data.frame(s.mat, stringsAsFactors = FALSE)
  s.df <- cbind(s.df, rep(tdm[["name"]], nrow(s.df)))
  colnames(s.df)[ncol(s.df)] <- "targetcandidate"
  return(s.df)
}

# This will bind the function and tdm into a new dataset called candTDM.

candTDM <- lapply(tdm, bindCandidateToTDM)

# Here is a way to run a quick review
# str(candTDM)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Additional Notes:

# s.mat <- t() will transpose the matrix so that each speech (document)
# will appear as a rowin the dataset and each term as a column. 

# s.df will convert the matrix into a dataframe and allow for us to use cbind
# and other functions.

# The   s.df <- cbind(s.df, rep(tdm[["name"]], nrow(s.df)))
# colnames(s.df)[ncol(s.df)] 
# will put the name of the candidate giving each speech into
# the last column of the dataframe. The rep(tdm[["name"]] will find the 
# name of a candidate.
# The nrow(s.df) code provides the explanation on how many rows this
# function will need to add a candidates name to. Finally, the 
# colnames(s.df)[ncol(s.df)] <- "targetcandidate" creates the column called
# target candidate and ensures that all row entries are not NULL. 

#########################################################################

# Step 5. - Merge the matrices into a single dataset by stacking the Obama
# and Romney matrices on top of each other. This uses functions from the plyr
# package.

# this creates a dataset that can be exported, predicted upon, etc...

tdm.stack <- do.call(rbind.fill, candTDM)

# remove the NA to show a 0.

tdm.stack[is.na(tdm.stack)] <- 0

# lets look at the first 10 rows
# head(tdm.stack, n=10)

#########################################################################

# Step 6. - Create the train and test dataset for modeling. The train
# sample will contain 70% of the dataset.

train.idx <- sample(nrow(tdm.stack), ceiling(nrow(tdm.stack) * 0.7)) 
test.idx <- (1:nrow(tdm.stack)) [-train.idx]

#########################################################################

# Step 7. - Create the kNN model.

# This will include all of the rows and just the target candidate variable.

tdm.cand <- tdm.stack[, "targetcandidate"]

# This will have all of the variables except the target candidate variable.

tdm.stack.nl <-tdm.stack[, !colnames(tdm.stack) %in% "targetcandidate"] 

# Here is the kNN model. The knn(tdm.stack.nl[train.idx]) code specifies
# to use the train data and the dataset without the targetcandidate variable.

knn.pred <- knn(tdm.stack.nl[train.idx, ], tdm.stack.nl[test.idx, ], tdm.cand[train.idx])

# the tdm.stack.nl[train.idx, ] runs the knn algorythm, the tdm.stack.nl[test.idx, ]
# applies the knn on the train set to the test dataset, and the 
# tdm.cand[train.idx] gives the names of the candidates for the final results.

#########################################################################

# Step 9. - Determine the accuracy of the model

conf.mat <- table("Predicitons" = knn.pred, Actual = tdm.cand[test.idx])
conf.mat

# Formula for accuracy

(accuracy <- sum(diag(conf.mat)) / length(test.idx) * 100)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Saving a dataframe as .csv
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

write.csv(tdm.stack, file='C:/Users/Derek/Documents/RPackages/Text Mining/candTDM.csv', row.names=F)
