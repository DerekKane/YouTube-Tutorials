# Text Analytics Tutorial - Facebook Sentiment Analysis
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


mydata <- read.csv("Facebook.csv")

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

# Set Options - Important for text analytics

options(stringAsFactors = FALSE)

# Duplicated the mydata dataframe.

mydata2 <- mydata

########################################################################
# Remove Unneeded columns from table
########################################################################

mydata2$ID <- NULL
mydata2$Name <- NULL
mydata2$Type <- NULL
mydata2$Gender <- NULL
mydata2$Birthday <- NULL
mydata2$Relationship <- NULL


########################################################################
# Create a corpus
########################################################################

Corpus.mydata <- Corpus(VectorSource(mydata2$Message))


########################################################################
# Corpus Cleanup
########################################################################

Corpus.mydata <- tm_map(Corpus.mydata, removeNumbers)
Corpus.mydata <- tm_map(Corpus.mydata, tolower)
Corpus.mydata <- tm_map(Corpus.mydata, removeWords, stopwords("english"))
Corpus.mydata <- tm_map(Corpus.mydata, removePunctuation)
Corpus.mydata <- tm_map(Corpus.mydata, stripWhitespace)

inspect(Corpus.mydata[1:3])

########################################################################
# Create Sentiment Score
########################################################################

# classify emotion

class_emo = classify_emotion(Corpus.mydata, algorithm="bayes", prior=1.0)

# get emotion best fit

emotion = class_emo[,7]

# substitute NA's by "unknown"

emotion[is.na(emotion)] = "unknown"

########################################################################

# classify polarity

class_pol = classify_polarity(Corpus.mydata, algorithm="bayes")

# get polarity best fit

polarity = class_pol[,4]


########################################################################
# Create Dataframe with Results. 
########################################################################

mydata = data.frame(text=mydata, emotion=emotion,
                     polarity=polarity, stringsAsFactors=FALSE)

# sort data frame

mydata = within(mydata,emotion <- factor(emotion, 
                levels=names(sort(table(emotion), decreasing=TRUE))))



########################################################################
# Generate Word Cloud 
########################################################################

#generate wordcloud
wordcloud(Corpus.mydata,min.freq = 1, scale=c(5,0.5),colors=brewer.pal(8, "Dark2"),
          random.color= TRUE, random.order = FALSE)


# separating text by emotion

emos = levels(factor(mydata$polarity))
nemo = length(emos)
emo.docs = rep("", nemo)

for (i in 1:nemo)  
{
  tmp = Corpus.mydata[polarity == emos[i]]
  emo.docs[i] = paste(tmp, collapse=" ")
  }

# create new corpus

corpus2 = Corpus(VectorSource(emo.docs))
tdm = TermDocumentMatrix(corpus2)
tdm = as.matrix(tdm)
colnames(tdm) = emos

# comparison word cloud

comparison.cloud(tdm, colors = brewer.pal(nemo, "Dark2"),
                 
                 scale = c(3,.5), random.order = FALSE, title.size = 1.5, vfont=c("gothic english","plain"))




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Saving a dataframe as .csv
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

write.csv(mydata, file='C:/Users/Derek/Documents/RPackages/Text Mining/mydata.csv', row.names=F)


########################################################################
# Twitter Analysis
########################################################################

# We need to establish the working directory where the file is located within.
# set the working directory

setwd("C:/Users/Derek/Documents/RPackages/Social Media")

########################################################################

# http://www.slideshare.net/fullscreen/ajayohri/twitter-analysis-by-kaify-rais/3
# https://sites.google.com/site/miningtwitter/questions/sentiment/sentiment


library("ROAuth")
library("twitteR")
library("wordcloud")
library("tm")
library("plyr")
library("stringr")
library("ggplot2")
library("sentiment")
library("RColorBrewer")

# Windows users need to download the following file

download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")

# This step can be run first if we have already authenticated our twitter
# account.

load("twitter authentication.Rdata")
registerTwitterOAuth(Cred)



########################################################################
# Get tweets from twitter.
########################################################################

Var1 <- '#AdrianPeterson'

Tweets <- searchTwitter(Var1, n=1000, cainfo="cacert.pem")
Tweets.df <- twListToDF(Tweets)

########################################################################

mydata <- read.csv("Tweets.csv")

mydata$favorited <- NULL
mydata$favoriteCount <- NULL
mydata$replyToSN <- NULL
mydata$created <- NULL
mydata$truncated <- NULL
mydata$replyToSID <- NULL
mydata$id <- NULL
mydata$replyToUID <- NULL
mydata$statusSource <- NULL
mydata$screenName <- NULL
mydata$retweetCount <- NULL
mydata$isRetweet <- NULL
mydata$retweeted <- NULL
mydata$longitude <- NULL
mydata$latitude <- NULL


# write.csv(Tweets.df, file='C:/Users/Derek/Documents/RPackages/Social Media/Tweets.csv', row.names=F)

########################################################################
# Create a corpus
########################################################################

Corpus.mydata <- Corpus(VectorSource(mydata$text))


########################################################################
# Corpus Cleanup
########################################################################

Corpus.mydata <- tm_map(Corpus.mydata, removeNumbers)
Corpus.mydata <- tm_map(Corpus.mydata, tolower)
Corpus.mydata <- tm_map(Corpus.mydata, removeWords, stopwords("english"))
Corpus.mydata <- tm_map(Corpus.mydata, removePunctuation)
Corpus.mydata <- tm_map(Corpus.mydata, stripWhitespace)


########################################################################
# Generate Word Cloud 
########################################################################

#generate wordcloud
wordcloud(Corpus.mydata,min.freq = 5, scale=c(5,0.5),colors=brewer.pal(8, "Dark2"),
          random.color= TRUE, random.order = FALSE)




########################################################################
# Sentiment Analysis - Live Tweet Pull
########################################################################


# get the text from the tweets

some_txt = sapply(Tweets, function(x) x$getText())

########################################################################
# Prepare the tweets for text analysis.
########################################################################

# remove retweet entities
some_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", some_txt)
# remove at people
some_txt = gsub("@\\w+", "", some_txt)
# remove punctuation
some_txt = gsub("[[:punct:]]", "", some_txt)
# remove numbers
some_txt = gsub("[[:digit:]]", "", some_txt)
# remove html links
some_txt = gsub("http\\w+", "", some_txt)
# remove unnecessary spaces
some_txt = gsub("[ \t]{2,}", "", some_txt)
some_txt = gsub("^\\s+|\\s+$", "", some_txt)

# define "tolower error handling" function 

try.error = function(x)  
{
  # create missing value 
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x), error=function(e) e)
  # if not an error
  if (!inherits(try_error, "error"))
    y = tolower(x)
  # result
  return(y)
}

# lower case using try.error with sapply 
some_txt = sapply(some_txt, try.error)
# remove NAs in some_txt
some_txt = some_txt[!is.na(some_txt)]
names(some_txt) = NULL


########################################################################

# Perform Sentiment Analysis.

# classify emotion
class_emo = classify_emotion(Corpus.mydata, algorithm="bayes", prior=1.0)
# get emotion best fit
emotion = class_emo[,7]
# substitute NA's by "unknown"
emotion[is.na(emotion)] = "unknown"


# classify polarity
class_pol = classify_polarity(Corpus.mydata, algorithm="bayes")
# get polarity best fit
polarity = class_pol[,4]

########################################################################

# Create a data frame with the results.

sent_df = data.frame(text=mydata, emotion=emotion,
                     
                     polarity=polarity, stringsAsFactors=FALSE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# sent_df$SearchTerm <- Var1
# sent_df$Date <- Sys.Date()
# write.csv(sent_df, file='C:/Users/Derek/Documents/RPackages/Social Media/Tweets.csv', row.names=F)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# sort data frame

sent_df = within(sent_df,
                 
                 emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))

head(sent_df, n=10)

########################################################################

# Lets perform some plots and observe the results.

# plot distribution of emotions
ggplot(sent_df, aes(x=emotion)) +
  geom_bar(aes(y=..count.., fill=emotion)) +
  scale_fill_brewer(palette="Dark2") +
  labs(x="emotion categories", y="number of tweets") +
  opts(title = "Sentiment Analysis of Tweets about Adrian Peterson \n(classification by emotion)",
       plot.title = theme_text(size=12))


# plot distribution of polarity

ggplot(sent_df, aes(x=polarity)) +
  geom_bar(aes(y=..count.., fill=polarity)) +
  scale_fill_brewer(palette="RdGy") +
  labs(x="polarity categories", y="number of tweets") +
  opts(title = "Sentiment Analysis of Tweets about Adrian Peterson \n(classification by polarity)",
       plot.title = theme_text(size=12))

########################################################################

# Separate the text by emotions and visualize 
# the words with a comparison cloud.

# separating text by emotion

emos = levels(factor(sent_df$emotion))
nemo = length(emos)
emo.docs = rep("", nemo)
for (i in 1:nemo)
{
  tmp = some_txt[emotion == emos[i]] 
  emo.docs[i] = paste(tmp, collapse=" ")
}

# remove stopwords
emo.docs = removeWords(emo.docs, stopwords("english"))

# create corpus
corpus = Corpus(VectorSource(emo.docs))

tdm = TermDocumentMatrix(corpus)
tdm = as.matrix(tdm)
colnames(tdm) = emos


########################################################################
# Generate Word Cloud 
########################################################################

#generate wordcloud
wordcloud(corpus,min.freq = 1, scale=c(5,0.5),colors=brewer.pal(8, "Dark2"),
          random.color= TRUE, random.order = FALSE)


# comparison word cloud
comparison.cloud(tdm, colors = brewer.pal(nemo, "Dark2"),
                 scale = c(3,.5), random.order = FALSE, title.size = 1.5)



########################################################################
########################################################################
########################################################################

# Extra

x <- 2349
sprintf("Substitute in a string or number: %s", x)

sprintf("Can have multiple %s occurrences %s", x, "- got it?")

Var1 <- '#ChicagoBulls'

Tweets <- searchTwitter(Var1, n=100, cainfo="cacert.pem")
Tweets.df <- twListToDF(Tweets)

write.csv(Tweets.df, file=sprintf('C:/Users/Derek/Documents/RPackages/Social Media/%s.csv', Var1), row.names=F)



