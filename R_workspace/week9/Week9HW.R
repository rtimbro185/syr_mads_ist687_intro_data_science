## Homework Week 9: Support Vector Machines

#--- Preprocess Steps:----------------------------------------------------------------------

### Clear objects from Memory
rm(list=ls())
### Clear Console:
cat("\014")
### Set Working Directory
setwd("C:\\workspaces\\ms_datascience_su\\IST687-IntroDataScience\\R_workspace\\hw")

#---- Global Variable Assignments --------------------------------------------


#---- Load Required Packages -------------------------------------------------
if(!require("devtools")) {install.packages("devtools")}
devtools::install_github("dkahle/ggmap")
if(!require("ggplot2")){install.packages("ggplot2")}
if(!require("dplyr")) {install.packages("dplyr")}
if(!require("e1071")) {install.packages("e1071")}
if(!require("arulesViz")) {install.packages("arulesViz")}
if(!require("gridExtra")) {install.packages("gridExtra")}
if(!require("caret")) {install.packages("caret")}
if(!require("kernlab")) {install.packages("kernlab")}
if(!require("arules")) {install.packages("arules")}


#---- Step 1: Load the data -------------------------
## Air Quality dataset

##-- 1.1: Clean the dataset
air <- airquality

#-- 1.2: Clean the data --------------------------------------------------

### Replace NA with column means
na.2.mean <- function(x){
  replace(x, is.na(x), mean(x, na.rm = TRUE))
}

cleanDataSet <- function(ds){
  #Make all empty cells equal to NA
  ds[ds==""] <- NA
  
  #Clean NA Columns from Dataframe
  ds <- ds[ ,!apply(ds,2,function(x) all(is.na(x)))]
  
  #Clean empty Rows from Dataframe
  ds <- ds[!apply(ds,1,function(x) all(is.na(x))),]
  
  # replace NA's in Ozone col with mean of col (where NA is discarded when calculating the mean)
  ds$Ozone[is.na(ds$Ozone)] <- mean(ds$Ozone,na.rm=TRUE)
  ds$Ozone <- round(ds$Ozone)
  ds$Solar.R[is.na(ds$Solar.R)] <- mean(ds$Solar.R,na.rm=TRUE)
  ds$Solar.R <- round(ds$Solar.R)
  
  return(ds)
}

clean.air <- cleanDataSet(air)

#-- 1.3: Understand the data ---------------------------------------------
str(clean.air)
summary(clean.air)
head(clean.air)

#---- Step 2: Create train and test data sets -------------------------
# Set repeatable random seed
set.seed(4)
partitionDataSet <- function(ds, fractionOfTest = 0.3){
  randoms <- runif(nrow(ds))
  cutoff <- quantile(randoms, fractionOfTest)
  testFlag <- randoms <= cutoff
  testingData <- ds[testFlag,]
  trainingData <- ds[!testFlag,]
  dataSetSplit <- list(trainingData=trainingData, testingData=testingData)
  return(dataSetSplit)
}



## Using techniques discussed in class, create two datasets - one for training and one for testing.
dim(clean.air)
clean.air[1:5,]
randIndex <- sample(1:nrow(clean.air))
randIndex
length(randIndex)

## Create a 2/3 cutpoint and round the number
cutPoint <- floor(2*nrow(clean.air)/3)
cutPoint

## Create train data set, contains the first 2/3 of overall data
train <- clean.air[randIndex[1:cutPoint],]
dim(train)
head(train)

## Create test data set, contains the rest of the 1/3 data that remains
test <- clean.air[randIndex[(cutPoint+1):nrow(clean.air)],]
dim(test)
head(test)

## Test exact split function
airDataSetSplits <- partitionDataSet(clean.air,0.33)
dim(airDataSetSplits$trainingData)
head(airDataSetSplits$trainingData)

dim(airDataSetSplits$testingData)
head(airDataSetSplits$testingData)

#---- Step 2.1: LM Model ---------------------------------------------------------------
airLmModel <- lm(Ozone ~ .,data=train)
summary(airLmModel)
airLmPred <- predict(airLmModel,test)
airLmPred

str(airLmPred)
compTable3 <- data.frame(test[,1],round(airLmPred))
colnames(compTable3) <- c("test","Pred")

# RMSE = 18.8
round(sqrt(mean((compTable3$test-compTable3$Pred)^2)),1)

#lm plot
compTable3$error <- abs(compTable3$test - compTable3$Pred)
plot3 <- data.frame(compTable3$error,test$Temp, test$Wind)
colnames(plot3) <- c("error","Temp","Wind")
plot.lm.Ozone <- ggplot(plot3, aes(x=Temp, y=Wind)) +
  geom_point(aes(size=error, color=error)) +
  ggtitle("Linear Model (LM), Airquality, Predict Ozone levels with Error dimension")
ggsave("LM_Scatter_Plot_Prediction_of_Ozone.jpg", width = 6, height = 6)
plot.lm.Ozone

#---- Step 3: Build a Model using KSVM & visualize the results -------------------------

##-- 3.1: Build a model (using the 'ksvm' function, trying to predict ozone). 
#         You can use all the possible attributes, or select the attributes that
#         you think would be the most helpful.

## Training Step - Ozone is the target predicting variable
# Kernel -> rdfdot: is the kernal function that projects the low-dimensional problem into higher-dimensional space
#           i.e., getting the maximum separation of distance between Ozone cases
# results: Training error = 0.081
#         Cross validation error = 568.72
#         Support Vectors = 91
ksvmOzoneOutput <- ksvm(Ozone ~ ., data=train, kernel="rbfdot", kpar="automatic", C=10, cross=10, prob.model=TRUE )
ksvmOzoneOutput

##-- 3.2: Test the model on the testing dataset, and compute the Root Mean Squared Error
ksvmOzonePred <- predict(ksvmOzoneOutput, test, type="votes")
ksvmOzonePred

# Create a comparison dataframe that contains the exact 'Ozone' value and the predicted 'Ozone' value
# use for RMSE calc
ksvmCompTable <- data.frame(test[,1],ksvmOzonePred[,1])
colnames(ksvmCompTable) <- c("test","Pred")
head(ksvmCompTable)

# Compute the Root Mean Squared Error - A smaller value indicates better model performance
# RMSE = 21.59642
sqrt(mean((ksvmCompTable$test - ksvmCompTable$Pred)^2))

##-- 3.3: Plot the results. Use a scatter plot. Have the x-axis represent temperature,
#         the y-axis represent wind, the point size and color represent the error,
#         as defined by the actual ozone level minus the predicted ozone level)

# Compute the absolute error for each case
ksvmCompTable$error <- abs(ksvmCompTable$test - ksvmCompTable$Pred)

# Create new dataframe contains error, temperature and wind
ksvmOzonePlotDf <- data.frame(ksvmCompTable$error, test$Temp, test$Wind, test$Ozone)
colnames(ksvmOzonePlotDf) <- c("error","Temp","Wind","Ozone")

# Plot results - using point size and color shade to illustrate how big the error is
plot.ksvm.Ozone <- ggplot(ksvmOzonePlotDf, aes(x=Temp, y=Wind)) +
  geom_point(aes(size=error, color=error)) +
  ggtitle("KSVM Scatter Plot, Prediction of Ozone with Error dimensions")
ggsave("KSVM_Scatter_Plot_Prediction_of_Ozone_With_Error.jpg", width = 6, height = 6)
plot.ksvm.Ozone

##-- 3.4: Compute models and plot the results for 'svm' (in the e1071 package)
#         
## Training Step - Ozone is the target predicting variable
# Kernel -> rdfdot: is the kernal function that projects the low-dimensional problem into higher-dimensional space
#           i.e., getting the maximum separation of distance between Ozone cases
svmOzoneOutput <- svm(Ozone ~ ., data=train, kernel="radial", C=10, cross=10, prob.model=TRUE )
svmOzoneOutput

## Test the model on the testing dataset, and compute the Root Mean Squared Error
svmOzonePred <- predict(svmOzoneOutput, test)
svmOzonePred
str(svmOzonePred)

# Create a comparison dataframe that contains the exact 'Ozone' value and the predicted 'Ozone' value
# use for RMSE calc
svmCompTable <- data.frame(select(test,'Ozone'),svmOzonePred)
colnames(svmCompTable) <- c("test","Pred")
head(svmCompTable)

# Compute the Root Mean Squared Error - A smaller value indicates better model performance
# RMSE = 16.54
sqrt(mean((svmCompTable$test - svmCompTable$Pred)^2))

##-- 3.3: Plot the results. Use a scatter plot. Have the x-axis represent temperature,
#         the y-axis represent wind, the point size and color represent the error,
#         as defined by the actual ozone level minus the predicted ozone level)

# Compute the absolute error for each case
svmCompTable$error <- abs(svmCompTable$test - svmCompTable$Pred)

# Create new dataframe contains error, temperature and wind
svmOzonePlotDf <- data.frame(round(svmCompTable$error,2), test$Temp, test$Wind, test$Ozone)
colnames(svmOzonePlotDf) <- c("error","Temp","Wind","Ozone")

# Plot results - using point size and color shade to illustrate how big the error is
plot.svm.Ozone <- ggplot(svmOzonePlotDf, aes(x=Temp, y=Wind)) +
  geom_point(aes(size=error, color=error)) +
  ggtitle("SVM Scatter Plot, Prediction of Ozone with Error dimensions")
ggsave("SVM_Scatter_Plot_Prediction_of_Ozone_With_Error.jpg", width = 6, height = 6)
plot.svm.Ozone


##-- 3.5: Show all three results (charts) in one window, using the grid.arrange function
ga3 <- grid.arrange(plot.ksvm.Ozone, plot.svm.Ozone, plot.lm.Ozone, ncol=3, nrow=2, top="Step 3 Model Comparisions")
ggsave(file="Grid_Arrange_KSVM-SVM-LM.jpg", ga3, width = 24, height = 12)


#---- Step 4: Create a 'goodOzone' variable -------------------------
##-- This variable should be either 0 or 1. It should be 0 if the ozone is below the average for all
#   the data observations, and 1 if it is equal to or above the average ozone observed.

avgOzone <- round(mean(clean.air$Ozone))
avgOzone
train$goodOzone <- ifelse(train$Ozone<avgOzone,0,1)
test$goodOzone <- ifelse(test$Ozone<avgOzone,0,1)
head(train)
head(test)

#---- Step 5: See if we can do a better job predicting 'good' and 'bad' days  -------------------------
train$goodOzone <- as.factor(train$goodOzone)
test$goodOzone <- as.factor(test$goodOzone)
train <- select(train,-'Ozone')
test <- select(test,-'Ozone')
str(train)
str(test)
##-- 5.1: Build a model (using the 'ksvm' function, trying to predict 'goodozone'). 
#         You can use all the possible attributes, or select the attributes that you think
#         would be the most helpful.
# Output Results:
#   Training error: 0.098
#   Cross Validation error: 0.354
#   Support Vectors: 61
ksvmOzoneOutputGood <- ksvm(goodOzone ~ ., data=train, kernel="rbfdot", kpar="automatic", C=10, cross=10, prob.model=TRUE)
ksvmOzoneOutputGood

##-- 5.2: Test the model on the testing dataset, and compute the percent of 'goodOzone' that was correctly predicted.
ksvmOzonePredGood <- predict(ksvmOzoneOutputGood, test)
ksvmOzoneCompGood1 <- data.frame(select(test,'goodOzone'), ksvmOzonePredGood)
colnames(ksvmOzoneCompGood1) <- c('test','Pred')
head(ksvmOzoneCompGood1)

# Percent of goodOzone that was correctly predicted
# Output: 0.705 or 70%
percKSVMCorrect <- length(which(ksvmOzoneCompGood1$test==ksvmOzoneCompGood1$Pred))/dim(ksvmOzoneCompGood1)[1]
percKSVMCorrect

# Confusion Matrix
# result output: 0 class, 18 identified correctly, 5 identified incorrectly
# result output: 1 class, 10 identified incorrectly, 18 identified correctly
ksvmResults <- table(test=ksvmOzoneCompGood1$test, pred=ksvmOzoneCompGood1$Pred)
print(ksvmResults)
length(ksvmOzoneCompGood1$test)

# Error & Accuracy Score Rate: 
ksvmErrorRate <- round((ksvmResults[1,][[2]] + ksvmResults[2,][[1]]) / nrow(ksvmOzoneCompGood1) *100,2)
ksvmErrorRate

ksvmAccuracyRate <- 100-ksvmErrorRate
ksvmAccuracyRate

##-- 5.3: Plot the results. Use a scatter plot. Have the x-axis represent temperature,
#         the y-axis represent wind, the shape representing what was predicted (good or bad day),
#         the color representing the actual value of 'goodozone' (i.e., if the actual ozone level was good)
#         and the size represent if the prediction was correct (larger symbols should be the observations the model got wrong)
ksvmOzoneCompGood1$correct <- ifelse(ksvmOzoneCompGood1$test==ksvmOzoneCompGood1$Pred,'correct','wrong')
plotksvmOzoneCompGood1 <- data.frame(ksvmOzoneCompGood1$correct,test$Temp,test$Wind,test$goodOzone,ksvmOzoneCompGood1$Pred)
colnames(plotksvmOzoneCompGood1) <- c("correct","Temp","Wind","goodOzone","Predict")
plot.ksvm.good <- ggplot(plotksvmOzoneCompGood1, aes(x=Temp,y=Wind)) +
  geom_point(aes(size=correct, color=goodOzone, shape=Predict)) +
  ggtitle("KSVM - Good/Bad Ozone Prediction")
ggsave("KSVM_Good_Bad_Ozone_Prediction.jpg", width = 6, height = 6)
plot.ksvm.good


##-- 5.3: Compute models and plot the results for 'svm' (in the e1071 package) and 'nb' (Naive Bayses)
#
## 5.3.1: Models & Plots for svm
svmOzoneOutputGood <- svm(goodOzone ~ ., data=train, kernel='radial', C=10, cross=10, prob.model=TRUE)
svmOzoneOutputGood

# Test the svm model
svmOzonePredGood <- predict(svmOzoneOutputGood, test)
svmOzoneCompGood1 <- data.frame(select(test,'goodOzone'), svmOzonePredGood)
colnames(svmOzoneCompGood1) <- c('test','Pred')
head(svmOzoneCompGood1)

# Percent of goodOzone that was correctly predicted
percSVMCorrect <- length(which(svmOzoneCompGood1$test==svmOzoneCompGood1$Pred))/dim(svmOzoneCompGood1)[1]
percSVMCorrect

# Confusion Matrix
# result output: 0 class, 21 identified correctly, 5 identified incorrectly
# result output: 1 class, 7 identified incorrectly, 18 identified correctly
svmResults <- table(test=svmOzoneCompGood1$test, pred=svmOzoneCompGood1$Pred)
print(svmResults)

# Error & Accuracy Score Rate: 
svmErrorRate <- round((svmResults[1,][[2]] + svmResults[2,][[1]]) / nrow(svmOzoneCompGood1) *100,2)
svmErrorRate

svmAccuracyRate <- 100-svmErrorRate
svmAccuracyRate


## Plot the results. Use a scatter plot. Have the x-axis represent temperature,
#     the y-axis represent wind, the shape representing what was predicted (good or bad day),
#     the color representing the actual value of 'goodozone' (i.e., if the actual ozone level was good)
#     and the size represent if the prediction was correct (larger symbols should be the observations the model got wrong)
svmOzoneCompGood1$correct <- ifelse(svmOzoneCompGood1$test==svmOzoneCompGood1$Pred,'correct','wrong')
plotSvmOzoneCompGood1 <- data.frame(svmOzoneCompGood1$correct,test$Temp,test$Wind,test$goodOzone,svmOzoneCompGood1$Pred)
colnames(plotSvmOzoneCompGood1) <- c("correct","Temp","Wind","goodOzone","Predict")
plot.svm.good <- ggplot(plotSvmOzoneCompGood1, aes(x=Temp,y=Wind)) +
  geom_point(aes(size=correct, color=goodOzone, shape=Predict)) +
  ggtitle("SVM - Good/Bad Ozone Prediction")
ggsave("SVM_Good_Bad_Ozone_Prediction.jpg", width = 6, height = 6)
plot.svm.good

## 5.3.2: Models & Plots for 'nb'
nbOzoneOutputGood <- naiveBayes(goodOzone ~ ., data=train)
nbOzoneOutputGood

# Test the naiveBase model
nbOzonePredGood <- predict(nbOzoneOutputGood, test)
nbOzoneCompGood1 <- data.frame(select(test,'goodOzone'), nbOzonePredGood)
colnames(nbOzoneCompGood1) <- c('test','Pred')
head(nbOzoneCompGood1)

# Percent of goodOzone that was correctly predicted
percNBCorrect <- length(which(nbOzoneCompGood1$test==nbOzoneCompGood1$Pred))/dim(nbOzoneCompGood1)[1]
percNBCorrect

# Confusion Matrix
# result output: 0 class, 19 identified correctly, 3 identified incorrectly
# result output: 1 class, 9 identified incorrectly, 20 identified correctly
nbResults <- table(test=nbOzoneCompGood1$test, pred=nbOzoneCompGood1$Pred)
print(nbResults)

nbErrorRate <- round((nbResults[1,][[2]] + nbResults[2,][[1]]) / nrow(nbOzoneCompGood1) *100,2)
nbErrorRate

nbAccuracyRate <- 100-nbErrorRate
nbAccuracyRate


## Plot the results. Use a scatter plot. Have the x-axis represent temperature,
#     the y-axis represent wind, the shape representing what was predicted (good or bad day),
#     the color representing the actual value of 'goodozone' (i.e., if the actual ozone level was good)
#     and the size represent if the prediction was correct (larger symbols should be the observations the model got wrong)
nbOzoneCompGood1$correct <- ifelse(nbOzoneCompGood1$test==nbOzoneCompGood1$Pred,'correct','wrong')
plotNBOzoneCompGood1 <- data.frame(nbOzoneCompGood1$correct,test$Temp,test$Wind,test$goodOzone,nbOzoneCompGood1$Pred)
colnames(plotNBOzoneCompGood1) <- c("correct","Temp","Wind","goodOzone","Predict")
plot.nb.good <- ggplot(plotNBOzoneCompGood1, aes(x=Temp,y=Wind)) +
  geom_point(aes(size=correct, color=goodOzone, shape=Predict)) +
  ggtitle("Niave Baise - Good/Bad Ozone Prediction")
ggsave("NB_Good_Bad_Ozone_Prediction.jpg", width = 6, height = 6)
plot.nb.good

##-- 5.5: Show all three results (charts) in one window, using the grid.array function (have two charts in one row)
ga5 <- grid.arrange(plot.ksvm.good, plot.svm.good, plot.nb.good, ncol=3, nrow=2, top="Step 5 Model Comparisions")
ggsave(file="Grid_Arrange_Good_KSVM-SVM-NB.jpg", ga5, width = 24, height = 12)

#---- Step 6: Which are the best Models for this data? -------------------------
## Review what you have done and state which is the best and why
#
# Answer: It's observed that the SVM and Naive Baise models have the heighest accuracy ratings, measured at 76.5%, 
#         for predicting the goodOzone Class. The result output calculation of Accuracy Rating for each model is shown below.
# 
## Output Results ## 
# Step 3 results:
# LM:   RMSE = 18.8
# KSVM: RMSE = 21.59642
# SVM:  RMSE = 16.54
#
# Step 5 results:
# KSVM: 
#    Accuracy Rate: 70.59%
#    Error Rate:  29.41%
# SVM:
#    Accuracy Rate: 76.47%
#    Error Rate: 23.53%
# Naive Baise: 
#    Accuracy Rate: 76.47%
#    Error Rate: 23.53%
#--------------------------------------------------------------------------------
