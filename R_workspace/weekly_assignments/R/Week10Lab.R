## Homework Week 10: Text	Mining
##    -Create our own termDocMatrix
#--- Preprocess Steps:----------------------------------------------------------------------

### Clear objects from Memory
rm(list=ls())
### Clear Console:
cat("\014")
### Set Working Directory
setwd("C:\\workspaces\\ms_datascience_su\\IST687-IntroDataScience\\R_workspace\\hw")

##---- Instructions ----------------------------------------------------------
# Word ranking scale ranking 'AFINN', scale of -5 to 5 (negative to positive)
# This homework task is to adapt the lab that we did in class, to compute the
# score for the MLK speech using the AFINN word list (as opposed to the positive
# and negative word lists).
# Learning Goals for this activity:
# A. Consider how a simple text mining technique can be applied to a variety of kinds of source data
# B. Provide practice in conditioning data to prepare for analysis
# C. Develop skill in the setup, execution, and interpretation of text mining analytics
# D. Increase familiarity with bringing external data sets into R.


#---- Load Required Packages -------------------------------------------------
if(!require("tm")){install.packages("tm")}
if(!require("wordcloud")){install.packages("wordcloud")}
if(!require("XML")){install.packages("XML")}

#---- Global Variable Assignments --------------------------------------------
sbaSpeechURL <- "http://www.historyplace.com/speeches/anthony.htm"
posNegWordsURL <- "https://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html"
posWordFile <- "positive-words.txt"
negWordFile <- "negative-words.txt"


##---- Lab Step 1: Read in the positive and negative word files ----
#1) Create two vectors of words, one for the positive words and one for the negative words 
#2) Note that when reading in the word files, there might be lines at the start and/or the end that will need to be removed (i.e. you should clean you dataset). 

## 1.1: Load pos / neg words
pos <- scan(posWordFile,character(0),sep='\n')
neg <- scan(negWordFile,character(0),sep='\n')

# 1.1.1: Clean word vectors
# Remove first 30 lines (header info)
pos <- pos[-1:-30]
neg <- neg[-1:-30]
head(pos,10)
head(neg,10)


##---- Step 2: Process in the MLK speech ----
#3) Read the text file 
#4) Create a term matrix 
#5) Create a list of counts for each word 

# 2.1: Read in the SBA Speech file
# returns a vector with as many elements as there are lines of text in the file
# grabs each line and is stored as a character string in a vector of character strings
sbaFile <- readLines(file("sba_speech.txt"))
sbaFile <- sbaFile[which(sbaFile != "")] # remove all blank lines

str(sbaFile)
head(sbaFile,10)

View(sbaFile)


# read in as html file
#sbaDocHtml <- htmlTreeParse(sbaSpeechURL, useInternal = TRUE)
#sba <- unlist(xpathApply(sbaDocHtml, '//p', xmlValue))
#str(sba)
#head(sba,10)

# 2.2: Create a term matrix
wordsVec <- VectorSource(sbaFile)
wordsCorpus <- Corpus(wordsVec)
wordsCorpus

# make all words in corpus lowercase
wordsCorpus <- tm_map(wordsCorpus, content_transformer(tolower))
wordsCorpus <- tm_map(wordsCorpus, removePunctuation)
wordscorpus <- tm_map(wordsCorpus, removeNumbers)
wordsCorpus <- tm_map(wordsCorpus, removeWords, stopwords("english"))

# create a term document matrix
tdm <- TermDocumentMatrix(wordsCorpus)
tdm
inspect(tdm)


# 2.3: Create a list of counts for each word
# The wordcloud function expects two vectors as input arguments
# - The first a list of the terms
# - The second a list of the frequencies of occurrence of the terms, both must be sorted

m <- as.matrix(tdm)
wordCounts <- rowSums(m)
wordCounts <- sort(wordCounts, decreasing=TRUE)
head(wordCounts)

cloudFrame <- data.frame(word=names(wordCounts),freq=wordCounts)
wordcloud(cloudFrame$word, cloudFrame$freq)

# prety up the wordcloud
wordcloud(names(wordCounts), wordCounts, min.freq = 2, max.words = 50, rot.per = .35, colors = brewer.pal(8,"Dark2"))

##---- Step 3: Determine how many positive words were in the speech ----
#6) Scale the number based on the total number of words in the speech 
#7) Hint: one way to do this is to use the 'match' function and then 'which' function 

# calculate the total number of words
totalWords <- sum(wordCounts)
totalWords

# vector of all the words
words <- names(wordCounts)
head(words,10)

# use match function, returns a vector of the positions of (first) matches of it's first argument in its second
matchedPosWords <- match(words,pos,nomatch = 0)
head(matchedPosWords,10)

matchedPosWords[8]
pos[1082]
words[8]

# now get the counts of all the words that did match
mPosCounts <- wordCounts[which(matchedPosWords != 0)]
# num of unique words
length(mPosCounts)

# Positive Words
mPosWords <- names(mPosCounts)
numPos <- sum(mPosCounts)
numPos



##---- Step 4: Determine how many negative words were in the speech ----
#8) Scale the number based on the total number of words in the speech 
#9) Hint: this is basically the same as step (3) 

# 8.1:
# Negative Words
negMatched <- match(words, neg, nomatch = 0)
negCounts <- wordCounts[which(negMatched != 0)]
numNeg <- sum(negCounts)
numWords <- names(negCounts)

numNeg
length(negCounts)

# ratio of positive to negative words in the speech
totalWords <- length(words)
ratioPos <- numPos/totalWords
ratioPos


ratioNeg <- numNeg/totalWords
ratioNeg

##---- Step 5: Redo the 'positive' and 'negative' calculations for each 25% of the speech 
# 10)Compare the results (ex. a simple barchart of the 4 numbers). ----
#


