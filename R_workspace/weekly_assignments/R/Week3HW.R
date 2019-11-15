# Clear objects from Memory
rm(list=ls())
# Clear Console:
cat("\014")

# Homework Week 3: Cleaning/munging Dataframes

##Objective: 
##In this lab, you need to read in a dataset and work on that dataset (in a dataframe) so that it
##can be useful. Then, we will explore the distribution within the dataset.


#Step 1: Create a function (named readStates) to read a CSV file into R
readStates <- function(){
  ## Read a URL that has the U.S. State Population dataset
  state.pops <- read.csv(url("http://www2.census.gov/programs-surveys/popest/tables/2010-2011/state/totals/nst-est2011-01.csv"), stringsAsFactors = FALSE)
  
  ## Call function to Clean dataframe
  clDf <- cleanDf(state.pops)
  
  return(clDf)
}


#Step 2: Function to Clean the dataframe
cleanDf <- function(df){
  if(!require("dplyr")) {install.packages("dplyr")}
  
  #Make all empty cells equal to NA
  df[df==""] <- NA
  
  #Clean NA Columns from Dataframe
  df <- df[ ,!apply(df,2,function(x) all(is.na(x)))]

  #Clean empty Rows from Dataframe
  df <- df[!apply(df,1,function(x) all(is.na(x))),]
  
  #Get column names from dataframe row 3
  #colNames <- as.character(as.vector(df[3,-1]))
  #colNames <- c("States",colNames)
  
  #Use Homework requirement, column names
  colNames <- c("stateName", "base2010", "base2011", "Jul2010", "Jul2011")
  
  #Rename colnames of the dataframe
  colnames(df) <- colNames
  
  #Get rows that contain state attributes
  df <- df[grep(pattern = "^\\.",df[,1]), ]
  
  #Renumber rownames
  rownames(df) <- NULL
  
  #Remove first character of State name that is a '.'
  df$stateName <- gsub("\\.","",df$stateName)
  
  #Convert column values from characters to numeric
  df$base2010 <- as.numeric(gsub(",","",df$base2010))
  df$base2011 <- as.numeric(gsub(",","",df$base2011))
  df$Jul2010 <- as.numeric(gsub(",","",df$Jul2010))
  df$Jul2011 <- as.numeric(gsub(",","",df$Jul2011)) 
  
  return(df)
}


#Step 3: Store and Explore the dataset
dfStates <- readStates()
str(dfStates)

#Test dataframe by calculating the mean for July2011 data
mean(dfStates$Jul2011)

#Step 4: Find the state with the Highest Population
state.max.pop <- dfStates[which.max(dfStates$Jul2011),1]
state.max.pop

## Sort the data, in increasing order, based on the July2011 data
orderedDfStates <- dfStates[order(dfStates$Jul2011),]
orderedDfStates

#Step 5: Explore the distribution of the states
## Functions
# input vect takes a vector
# input threshold takes a number
distOfStates <- function(vect, threshold){
  
  #return the percentage of the elements within the vector that is less than the same
  perc <- length(vect[vect < threshold])/length(vect)
  return(perc)
}

## Test Functions
#test with vector 'dfStates$Jul2011Num', and the mean of 'dfStates$Jul2011Num'

percentLess <- distOfStates(dfStates$Jul2011,mean(dfStates$Jul2011))
percentLess

## Additional Exploratory Methods ###
summary(dfStates$Jul2011)

require(stats)
plot(dfStates$Jul2011)
lines(lowess(dfStates$Jul2011))

with(dfStates,hist(Jul2011, breaks = "FD", col = "green"))
box()


