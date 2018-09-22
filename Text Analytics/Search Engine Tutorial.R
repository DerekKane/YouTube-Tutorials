# Text Analytics Tutorial - Search Engine
############################################################################

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Set a Working Directory
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

setwd("C:/Users/Derek/Documents/RPackages/Text Mining")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create a dataset
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

doc1 <- "Stray cats are running all over the place. I see 10 a day!"
doc2 <- "Cats are killers. They kill billions of animals a year."
doc3 <- "The best food in Columbus, OH is   the North Market."
doc4 <- "Brand A is the best tasting cat food around. Your cat will love it."
doc5 <- "Buy Brand C cat food for your cat. Brand C makes healthy and happy cats."
doc6 <- "The Arnold Classic came to town this weekend. It reminds us to be healthy."
doc7 <- "I have nothing to say. In summary, I have told you nothing."

# This combines them into a single data frame (database).

doc.list <- list(doc1, doc2, doc3, doc4, doc5, doc6, doc7)
N.docs <- length(doc.list)
names(doc.list) <- paste0("doc", c(1:N.docs))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Here is the search engine query we want to evaluate.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

query <- "Healthy cat food"

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load the libraries
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(tm)
library(SnowballC)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Build a corpus out of the documents and bring in our search query.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

my.docs <- VectorSource(c(doc.list, query))
my.docs$Names <- c(names(doc.list), "query")

my.corpus <- Corpus(my.docs)
my.corpus


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Standardizing the text data and clean off the junk.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# One of the nice things about the Corpus class is the tm_map function,
# which cleans and standardizes documents within a Corpus object. 
# Below are some of the transformations.

getTransformations()

# Lets get rid of punctuation

my.corpus <- tm_map(my.corpus, removePunctuation)
my.corpus$doc1

# Suppose we don't want to count "cats" and "cat" as two separate words. 
# Then we will use the stemDocument transformation to implement the famous 
# Porter Stemmer algorithm. 
# To use this particular transformation, first load the SnowballC package.

library(SnowballC)
my.corpus <- tm_map(my.corpus, stemDocument)
my.corpus$doc1

# Finally, remove numbers and any extra white space.

my.corpus <- tm_map(my.corpus, removeNumbers)
my.corpus <- tm_map(my.corpus, tolower)
my.corpus <- tm_map(my.corpus, stripWhitespace)
my.corpus$doc1

# We applied all these standardization techniques without much thought. For instance, we sacrificed 
# inflection in favor of fewer words. But at least the transformations make 
# sense on a heuristic level, much like the similarity concepts to follow.

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Document Similarity
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Here's a trick that's been around for a while: represent each document as a vector in \( \mathcal{R}^N \) (with \( N \) as the number of words) 
# and use the angle \( \theta \) between the vectors as a similarity measure. Rank 
# by the similarity of each document to the query and you have a search engine.


# One of the simplest things we can do is to count words within documents. 

term.doc.matrix.stm <- TermDocumentMatrix(my.corpus)
inspect(term.doc.matrix.stm[0:14, ])


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Sparsity and storage of the term document matrix
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# The matrices in tm are of type Simple Triplet Matrix where only the triples \( (i, j, value) \) are stored for non-zero values. To work directly 
# with these objects, you may use install the slam  [4]  package. We bear 
# some extra cost by making the matrix "dense" (i.e., storing all the zeros) below.

# install.packages("slam")

library(slam)

term.doc.matrix <- as.matrix(term.doc.matrix.stm)

cat("Dense matrix representation costs", object.size(term.doc.matrix), "bytes.\n", 
    "Simple triplet matrix representation costs", object.size(term.doc.matrix.stm), 
    "bytes.")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Variations on a theme
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# In term.doc.matrix, the dimensions of the document space are simple term frequencies. 
# This is fine, but other heuristics are available. For instance, rather than a linear increase in the 
# term frequency \( tf \), perhaps \( \sqrt(tf) \) or \( \log(tf) \) would provide 
# a more reasonable diminishing returns on word counts within documents.

# Rare words can also get a boost. The word "healthy" appears in only one document, whereas "cat" 
# appears in four. A word's document frequency \( df \) is the number
# of documents that contain it, and a natural choice is to weight words 
# inversely proportional to their \( df \)s. As with term frequency, we may 
# use logarithms or other transformations to achieve the desired effect. 

# The tm function weightTfIdf offers one variety of tfidf weighting, but below we build our own. 
# Visit the Wikipedia page for the SMART Information Retrieval System 
# for a brief history and a list of popular weighting choices. 

# Different weighting choices are often made for the query and the 
# documents. For instance, Manning et al.'s worked example  [5] uses
# \( idf \) weighting only for the query.

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Choice and implementation
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# For both the document and query, we choose tfidf weights of \( (1 + \log_2(tf)) \times \log_2(N/df) \), which are defined to be \( 0 \) 
# if \( tf = 0 \). Note that whenever a term does not occur in a specific 
# document, or when it appears in every document, its weight is zero. 

# We implement this weighting function across entire rows of the term 
# document matrix, and therefore our tfidf function must take a term frequency 
# vector and a document frequency scalar as inputs.

get.tf.idf.weights <- function(tf.vec, df) {
  # Computes tfidf weights from a term frequency vector and a document
  # frequency scalar
  weight = rep(0, length(tf.vec))
  weight[tf.vec > 0] = (1 + log2(tf.vec[tf.vec > 0])) * log2(N.docs/df)
  weight
}

cat("A word appearing in 4 of 6 documents, occuring 1, 2, 3, and 6 times, respectively: \n", 
    get.tf.idf.weights(c(1, 2, 3, 0, 0, 6), 4))

# Using apply, we run the tfidf weighting function on every row of the term document matrix. 
# The document frequency is easily derived from each row by the counting the non-zero entries (not including the query).

get.weights.per.term.vec <- function(tfidf.row) {
  term.df <- sum(tfidf.row[1:N.docs] > 0)
  tf.idf.vec <- get.tf.idf.weights(tfidf.row, term.df)
  return(tf.idf.vec)
}

tfidf.matrix <- t(apply(term.doc.matrix, c(1), FUN = get.weights.per.term.vec))
colnames(tfidf.matrix) <- colnames(term.doc.matrix)

tfidf.matrix[0:3, ]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Dot Product Geometry
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# A benefit of being in the vector space \( \mathcal{R}^N \) is the use of its dot product. For vectors \( a \) and \( b \), the geometric definition of 
# the dot product is \( a \cdot b = \vert\vert a\vert\vert \, \vert\vert b \vert \vert \cos \theta \),
# where \( \vert\vert \cdot \vert \vert \) is the euclidean norm 
# (the root sum of squares) and \( \theta \) is the angle between \( a \) and \( b \). 

# In fact, we can work directly with the cosine of \( \theta \). 
# For \( \theta \) in the interval \( [-\pi, -\pi] \), 
# the endpoints are orthogonality (totally unrelated documents) and the center, zero, is complete collinearity (maximally similar documents). We can see that the cosine decreases from 
# its maximum value of \( 1.0 \) as the angle departs from zero in either direction.

angle <- seq(-pi, pi, by = pi/16)
plot(cos(angle) ~ angle, type = "b", xlab = "angle in radians", main = "Cosine similarity by angle")

# We may furthermore normalize each column vector in our tfidf matrix so that its norm is one. 
# Now the dot product is \( \cos \theta \).

tfidf.matrix <- scale(tfidf.matrix, center = FALSE, scale = sqrt(colSums(tfidf.matrix^2)))
tfidf.matrix[0:18, ]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Matrix Multiplication : A Dot Product Machine
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Keeping the query alongside the other documents let us avoid repeating the same steps. 
# But now it's time to pretend it was never there. 

query.vector <- tfidf.matrix[, (N.docs + 1)]
tfidf.matrix <- tfidf.matrix[, 1:N.docs]

# With the query vector and the set of document vectors in hand, 
# it is time to go after the cosine similarities. These are simple 
# dot products as our vectors have been normalized to unit length. 

# Recall that matrix multiplication is really just a sequence of 
# vector dot products. The matrix operation below returns values 
# of \( \cos \theta \) for each document vector and the query vector.

doc.scores <- t(query.vector) %*% tfidf.matrix

# With scores in hand, rank the documents by their 
# cosine similarities with the query vector.

results.df <- data.frame(doc = names(doc.list), score = t(doc.scores), 
                         text = unlist(doc.list))
results.df <- results.df[order(results.df$score, decreasing = TRUE), ]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Results - How did our search engine do?
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

options(width = 2000)
print(results.df, row.names = FALSE, right = FALSE, digits = 2)

# Our "best" document, at least in an intuitive sense, comes out 
# ahead with a score nearly twice as high as its nearest competitor. 
# Notice however that this next competitor has nothing to do with cats. 
# This is due to the relative rareness of the word "healthy" in the documents 
# and our choice to incorporate the inverse document frequency weighting for
# both documents and query. Fortunately, the profoundly uninformative document 7 has been ranked dead last.


write.csv(results.df, file='C:/Users/Derek/Documents/RPackages/Text Mining/searchresults.csv', row.names=F)
