# Text Analytics Tutorial - NLP
############################################################################

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Set a Working Directory
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

setwd("C:/Users/Derek/Documents/RPackages/Text Mining")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load the dataset
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Set Options - Important for text analytics

mydata <- read.csv("Facebook.csv")
options(stringAsFactors = FALSE)

#########################################################################
# Initialize the packages.
#########################################################################

# Make sure that we are running 32 bit version of R!!!!

library("openNLP")
library("openNLPdata")
library("tm")
library("NLP")
library("openNLPmodels.en")

########################################################################
# Remove Unneeded columns from table
########################################################################

mydata$ID <- NULL
mydata$Name <- NULL
mydata$Type <- NULL
mydata$Gender <- NULL
mydata$Birthday <- NULL
mydata$Relationship <- NULL

#########################################################################
# Create some text to manipulate with NLP
#########################################################################

s <- paste(c("Pierre Vinken, 61 years old, will join the board as a ",
             "nonexecutive director on November 29. ",
             "Mr. Vinken is chairman of Chicago , ",
             "the Dutch publishing group."),
           collapse = "")

# s <- as.String(mydata$Message)

s <- as.String(s)


#########################################################################
# Sentence Splitting
#########################################################################

# Break apart the text into separate sentances.

sent_token_annotator <- Maxent_Sent_Token_Annotator()
a1 <- annotate(s, sent_token_annotator)

#########################################################################
# Tokenization
#########################################################################

# Find the individual words in each sentence.

word_token_annotator <- Maxent_Word_Token_Annotator()
word_token_annotator
a2 <- annotate(s, word_token_annotator, a1)
a2

#########################################################################
# Part of Speech Tagging
#########################################################################

pos_tag_annotator <- Maxent_POS_Tag_Annotator()
pos_tag_annotator
a3 <- annotate(s, pos_tag_annotator, a2)
a3

## Variant with POS tag probabilities as (additional) features.
head(annotate(s, Maxent_POS_Tag_Annotator(probs = TRUE), a2))

## Determine the distribution of POS tags for word tokens.
a3w <- subset(a3, type == "word")
tags <- sapply(a3w$features, `[[`, "POS")
tags
table(tags)

## Extract token/POS pairs (all of them): easy.
sprintf("%s/%s", s[a3w], tags)

## Extract pairs of word tokens and POS tags for second sentence:
a3ws2 <- annotations_in_spans(subset(a3, type == "word"),
                              subset(a3, type == "sentence")[2L])[[1L]]
sprintf("%s/%s", s[a3ws2], sapply(a3ws2$features, `[[`, "POS"))

#########################################################################
# Named Entity Recognition
#########################################################################

# requires package openNLPmodels.en 
# from http://datacube.wu.ac.at

## Entity recognition for persons.
entity_annotator <- Maxent_Entity_Annotator(kind="person")
entity_annotator
annotate(s, entity_annotator, a2)

## Directly:
entity_annotator(s, a2)

## And slice ...
s[entity_annotator(s, a2)]

## Variant with sentence probabilities as features.
annotate(s, Maxent_Entity_Annotator(probs = TRUE), a2)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Entity recognition for locations.
entity_annotator <- Maxent_Entity_Annotator(kind="location")
entity_annotator
annotate(s, entity_annotator, a2)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Entity recognition for date.
entity_annotator <- Maxent_Entity_Annotator(kind="date")
entity_annotator
annotate(s, entity_annotator, a2)

#########################################################################
# Chunker - Shallow Parsing
#########################################################################
## Chunking needs word token annotations with POS tags.

sent_token_annotator <- Maxent_Sent_Token_Annotator()
word_token_annotator <- Maxent_Word_Token_Annotator()
pos_tag_annotator <- Maxent_POS_Tag_Annotator()
a3 <- annotate(s,
               list(sent_token_annotator,
                    word_token_annotator,
                    pos_tag_annotator))

annotate(s, Maxent_Chunk_Annotator(), a3)
annotate(s, Maxent_Chunk_Annotator(probs = TRUE), a3)

#########################################################################
# Parse Annotator
#########################################################################

## Need sentence and word token annotations.
sent_token_annotator <- Maxent_Sent_Token_Annotator()
word_token_annotator <- Maxent_Word_Token_Annotator()
a2 <- annotate(s, list(sent_token_annotator, word_token_annotator))

parse_annotator <- Parse_Annotator()
## Compute the parse annotations only.
p <- parse_annotator(s, a2)

## Extract the formatted parse trees.
ptexts <- sapply(p$features, `[[`, "parse")
ptexts

## Read into NLP Tree objects.
ptrees <- lapply(ptexts, Tree_parse)
ptrees
