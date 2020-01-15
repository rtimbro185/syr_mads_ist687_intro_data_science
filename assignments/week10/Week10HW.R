## Homework Week 10: Text	Mining
##    -Create our own termDocMatrix


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


#--- Preprocess Steps:----------------------------------------------------------------------

### Clear objects from Memory
rm(list=ls())
### Clear Console:
cat("\014")
### Set Working Directory
setwd("C:\\workspaces\\ms_datascience_su\\IST687-IntroDataScience\\R_workspace\\hw")


#---- Load Required Packages -------------------------------------------------
if(!require("tidytext")){install.packages("tidytext")}
if(!require("readtext")){install.packages("readtext")}
if(!require("OptimalCutpoints")){install.packages("OptimalCutpoints")}
if(!require("tm")){install.packages("tm")}
if(!require("wordcloud")){install.packages("wordcloud")}
if(!require("slam")){install.packages("slam")}
if(!require("ggplot2")){install.packages("ggplot2")}
if(!require("dplyr")) {install.packages("dplyr")}
if(!require("webshot")){install.packages("webshot")}

#---- Global Variable Assignments --------------------------------------------
mlkFileName <- "MLK.txt"
afinnFileName <- "AFINN.txt"


##---- TDM Step 1: Process in the MLK speech ----
#3) Read the text file 
#4) Create a term matrix 
#5) Create a list of counts for each word 

# TDM 1.1: Read in the MLK Speech file
# returns a vector with as many elements as there are lines of text in the file
# grabs each line and is stored as a character string in a vector of character strings
mlkFile <- readLines(file(mlkFileName))
mlkFile <- mlkFile[which(mlkFile != "")] # remove all blank lines

str(mlkFile)
head(mlkFile,10)

View(mlkFile)

# TDM 1.2: Create a term matrix
wordsVec <- VectorSource(mlkFile)
wordsCorpus <- Corpus(wordsVec)
wordsCorpus
View(wordsCorpus)

# make all words in corpus lowercase
wordsCorpus <- tm_map(wordsCorpus, content_transformer(tolower))
wordsCorpus <- tm_map(wordsCorpus, removePunctuation)
wordscorpus <- tm_map(wordsCorpus, removeNumbers)
wordsCorpus <- tm_map(wordsCorpus, removeWords, stopwords("english"))

# create a term document matrix
tdm <- TermDocumentMatrix(wordsCorpus)
tdm
View(tdm)
inspect(tdm)

# 1.3: Create a list of counts for each word
# The wordcloud function expects two vectors as input arguments
# - The first a list of the terms
# - The second a list of the frequencies of occurrence of the terms, both must be sorted

m <- as.matrix(tdm)
wordCounts <- rowSums(m)
wordCounts <- sort(wordCounts, decreasing=TRUE)
head(wordCounts,10)

# calculate the total number of words
totalWords <- sum(wordCounts)
totalWords

# vector of all the words
words <- names(wordCounts)
head(words,10)

# build a word cloud
cloudFrame <- data.frame(word=names(wordCounts),freq=wordCounts)
View(cloudFrame)
wordcloud(cloudFrame$word, cloudFrame$freq)

# prety up the wordcloud & save as 
wordcloud(names(wordCounts), wordCounts, min.freq = 2, max.words = 50, rot.per = .35, colors = brewer.pal(8,"Dark2"))


#---- AFINN Step 1: Load the data ---------------------------------------------------
# First read in the AFINN word list. Each line is both a word and a score (between -5 and 5)
# Split the line and create two vectors (one for words and one for scores).

# Read Affinity file
afinn <- read.delim(afinnFileName, sep="\t", header = FALSE)
str(afinn)
colnames(afinn) <- c("Word", "Score")
View(afinn)

# join the df match with afinn by "word" col in match and "Word" col in afinn
mergedTable <- merge(cloudFrame, afinn, by.x = "word", by.y = "Word")
str(mergedTable)
View(mergedTable)

#---- Step 2: Compute the overall score ----------------------------------------
# Compute the overall score for the MLK speech using the AFINN word list (as opposed
# to the positive and negative word lists).

overallScore <- sum(mergedTable$freq*mergedTable$Score)
overallScore

#---- Step 3: Compute the sentiment score --------------------------------------
# Compute the sentiment score for each quarter (25%) of the speech to see how this sentiment
# analysis is the same or different than what was computing with just the positive and negative word files.
getQuarterlySentimentScore <- function(table,quarter){
  score <- 0
  qRange <- round(nrow(table)/4)
  switch(quarter,
         "Q1" = {
           t <- table[1:qRange,]
           score <- sum(t$freq*t$Score)
           }, 
         "Q2" = {
           t <- table[(qRange+1):(qRange*2),]
           score <- sum(t$freq*t$Score)
           },
         "Q3" = {
           t <- table[(qRange*2+1):(qRange*3),]
           score <- sum(t$freq*t$Score)
           },
         "Q4" = {
           t <- table[(qRange*3+1):nrow(table),]
           score <- sum(t$freq*t$Score)
           },
         stop("Error, invalid quarter value. Acceptiable values are: 'Q1', 'Q2', 'Q3', 'Q4'
              ")
  )

  return(score)
}

mlkSplitQuarters <- c('Q1','Q2','Q3','Q4')
mlkSentimentScores <- NULL
for(e in mlkSplitQuarters){
  mlkSentimentScores <- c(mlkSentimentScores,getQuarterlySentimentScore(mergedTable,e))
  print(paste0(e," = ",getQuarterlySentimentScore(mergedTable,e)))
}
mlkSentimentScores

mlkSentQuartDf <- data.frame(mlkSplitQuarters,mlkSentimentScores)
colnames(mlkSentQuartDf) <- c('Quarter', 'Score')
View(mlkSentQuartDf)

#---- Step 4: Plot the results -------------------------------------------------
# 4 numbers via a bar chart
ggplot(data=mlkSentQuartDf) +
  geom_col(aes(x=Quarter, y=Score)) +
  labs(title="MLK Quarterly Sentiment Scores")
ggsave("MLK_Quarterly_Sentiment_Scores.jpg", width = 6, height = 6)





