## Final Project: Analyze Hotel Survey Data - NPS


# ---Preprocess Steps:----------------------------------------------------------------------

### Clear objects from Memory
rm(list=ls())
### Clear Console:
cat("\014")

##---- Global Variables --------------------------------------
dataSetName <- "ProjectSurveyData.csv"
workingDir <- "C:\\workspaces\\ms_datascience_su\\IST687-IntroDataScience\\R_workspace\\project\\"
### Set Working Directory
setwd(workingDir)

##---- Required Libraries ------------------------------------
if(!require("stringr")){install.packages("stringr")}
if(!require("dplyr")) {install.packages("dplyr")}
if(!require("lubridate")) {install.packages("lubridate")}
if(!require("sqldf")){install.packages("sqldf")}
if(!require("ggplot2")){install.packages("ggplot2")}
if(!require("psych")){install.packages("psych")}
if(!require("reshape2")){install.packages("reshape2")}
if(!require("e1071")) {install.packages("e1071")}
if(!require("arulesViz")) {install.packages("arulesViz")}
if(!require("gridExtra")) {install.packages("gridExtra")}
if(!require("caret")) {install.packages("caret")}
if(!require("kernlab")) {install.packages("kernlab")}
if(!require("arules")) {install.packages("arules")}
if(!require("ggmap")){install.packages("ggmap")}
if(!require("maps")){install.packages("maps")}
if(!require("zipcode")){install.packages("zipcode")}
if(!require("mapproj")){install.packages("mapproj")}



# Register Google API
register_google(key="AIzaSyDdjiKuumlpQunYJxtMYEdEq5o32QJgJ28")

# ----Step 1: Data Preparation Steps-----------------------------------------------------------
### Function: Read DataSet
readDataSet <- function(ds){

  survey.ds <- read.csv(ds, header = TRUE, stringsAsFactors = FALSE)
  return(survey.ds)
  
}

## Function: Clean DataSet
### Replace NA with column means
na.2.mean <- function(x){
  replace(x, is.na(x), mean(x, na.rm = TRUE))
}
removeOutliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}
cleanDataSet <- function(ds){
  
  
  #Replace outliers
  #ds$Guest_Room_H <- removeOutliers(ds$Guest_Room_H)
  
  #Make all empty cells equal to NA
  ds[ds==""] <- NA
  
  #Clean NA Columns from Dataframe
  ds <- ds[ ,!apply(ds,2,function(x) all(is.na(x)))]
  
  #Clean empty Rows from Dataframe
  ds <- ds[!apply(ds,1,function(x) all(is.na(x))),]
  
  #Set Guest Country NA to 'NOT_LISTED'
  ds$Guest_Country_H[is.na(ds$Guest_Country_H)] <- 'NOT_LISTED'
  ds$Guest_Country_H[ds$Guest_Country_H=="United States Minor "] <- 'USA'
  
  #Set Character NA to 'NOT_LISTED'
  ds$Club.Type_PL[is.na(ds$Club.Type_PL)] <- 'NOT_LISTED'
  ds$STATE_R[is.na(ds$STATE_R)] <- 'NOT_LISTED'
  ds$GP_Tier[is.na(ds$GP_Tier)] <- 'NOT_LISTED'
  
  
  #Set Gendr to 'PNTA' where it's 'Prefer not to answer' or NA
  ds$Gender_H[is.na(ds$Gender_H) | ds$Gender_H == 'Prefer not to answer'] <- 'PNTA'
  
  #Replace column NA with mean values
  ds$LENGTH_OF_STAY_C <- round(na.2.mean(ds$LENGTH_OF_STAY_C))
  ds$NUMBER_OF_ROOMS_C <- round(na.2.mean(ds$NUMBER_OF_ROOMS_C))
  ds$ADULT_NUM_C <- round(na.2.mean(ds$ADULT_NUM_C))
  ds$F.B_Overall_Experience_H <- round(na.2.mean(ds$F.B_Overall_Experience_H))
  ds$F.B_FREQ_H <- round(na.2.mean(ds$F.B_FREQ_H))
  ds$Check_In_H <- round(na.2.mean(ds$Check_In_H))
  ds$Internet_Sat_H <- round(na.2.mean(ds$Internet_Sat_H))
  ds$Staff_Cared_H <- round(na.2.mean(ds$Staff_Cared_H))
  ds$Customer_SVC_H <- round(na.2.mean(ds$Customer_SVC_H))
  ds$Condition_Hotel_H <- round(na.2.mean(ds$Condition_Hotel_H))
  ds$Tranquility_H <- round(na.2.mean(ds$Tranquility_H))
  ds$Guest_Room_H <- round(na.2.mean(ds$Guest_Room_H))
  ds$Overall_Sat_H <- round(na.2.mean(ds$Overall_Sat_H))
  
  #Set Variables as Factors
  ds$MARKET_GROUP_C <- as.factor(ds$MARKET_GROUP_C) 
  ds$ROOM_TYPE_CODE_C <- as.factor(ds$ROOM_TYPE_CODE_C)
  ds$WALK_IN_FLG_C <- as.factor(ds$WALK_IN_FLG_C)
  ds$POV_CODE_C <- as.factor(ds$POV_CODE_C)
  ds$ENTRY_HOTEL_CODE_R <- as.factor(ds$ENTRY_HOTEL_CODE_R)
  ds$ROOM_TYPE_CODE_R <- as.factor(ds$ROOM_TYPE_CODE_R)
  ds$NPS_Type <- as.factor(ds$NPS_Type)
  ds$Booking_Channel <- as.factor(ds$Booking_Channel)
  ds$GP_Tier <- as.factor(ds$GP_Tier)
  ds$Relationship_PL <- as.factor(ds$Relationship_PL)
  ds$Location_PL <- as.factor(ds$Location_PL)
  ds$Class_PL <- as.factor(ds$Class_PL)
  ds$Type_PL <- as.factor(ds$Type_PL)
  ds$Category_PL <- as.factor(ds$Category_PL)
  ds$Region_PL <- as.factor(ds$Region_PL)
  ds$Brand_PL <- as.factor(ds$Brand_PL)
  ds$Dom.Int.l_PL <- as.factor(ds$Dom.Int.l_PL)
  ds$Currency_PL <- as.factor(ds$Currency_PL)
  ds$Ops.Region_PL <- as.factor(ds$Ops.Region_PL)
  ds$Country_PL <- as.factor(ds$Country_PL)
  ds$Award.Category_PL <- as.factor(ds$Award.Category_PL)
  ds$F.B_Overall_Experience_H <- as.factor(ds$F.B_Overall_Experience_H)
  ds$Check_In_H <- as.factor(ds$Check_In_H)
  ds$Internet_Sat_H <- as.factor(ds$Internet_Sat_H)
  ds$Staff_Cared_H <- as.factor(ds$Staff_Cared_H)
  ds$Customer_SVC_H <- as.factor(ds$Customer_SVC_H)
  ds$Condition_Hotel_H <- as.factor(ds$Condition_Hotel_H)
  ds$Tranquility_H <- as.factor(ds$Tranquility_H)
  ds$Guest_Room_H <- as.factor(ds$Guest_Room_H)
  ds$Overall_Sat_H <- as.factor(ds$Overall_Sat_H)
  ds$Likelihood_Recommend_H <- as.factor(ds$Likelihood_Recommend_H)
  ds$Language_H <- as.factor(ds$Language_H)
  ds$Gender_H <- as.factor(ds$Gender_H)
  ds$Club.Type_PL <- as.factor(ds$Club.Type_PL)
  ds$STATE_R <- as.factor(ds$STATE_R)
  ds$PACE_CATEGORY_R <- as.factor(ds$PACE_CATEGORY_R)
  
  #Convert Character Date's to Data Type: convert date info in format 'mm/dd/yyyy'
  ds$CHECK_IN_DATE_C <- as.Date(ds$CHECK_IN_DATE_C,"%m/%d/%Y")
  ds$CHECK_OUT_DATE_C <- as.Date(ds$CHECK_OUT_DATE_C,"%m/%d/%Y")
  ds$RESERVATION_DATE_R <- as.Date(ds$RESERVATION_DATE_R,"%m/%d/%Y")
  ds$LAST_CHANGE_DATE_R <- as.Date(ds$LAST_CHANGE_DATE_R,"%m/%d/%Y")
    
  #Convert Character Time's to POSIXct format - Use lubridate later for analysis
  ds$ENTRY_TIME_R <- as.POSIXct(paste("2019-01-01",ds$ENTRY_TIME_R, sep = " "), tz="UTC")
  
  #Set CHILDREN NUM Count to zero where it's NA
  ds$CHILDREN_NUM_C[is.na(ds$CHILDREN_NUM_C)] <- 0
  
  
  
  
  
  return(ds)
}

##----Util Functions:  -------------------------
# Create train and test data sets
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

## Classify Age Ranges, 1 through 8 -> NAs have been transformed to mean value of all age range observations
normNAAgeRange <- function(age.class){
  range <- ""
  if(age.class==1){
    range <- "00-17"
  }else if(age.class==2){
    range <- "18-25"
  }else if(age.class==3){
    range <- "26-35"
  }else if(age.class==4){
    range <- "36-45"
  }else if(age.class==5){
    range <- "46-55"
  }else if(age.class==6){
    range <- "56-65"
  }else if(age.class==7){
    range <- "66-75"
  }else if(age.class==8){
    range <- "76+"
  }else{
    range <- "00-00"
  }
  return(range)
}
classifyAgeRanges <- function(ds){
  ds$Age_Range_H_Class <- NA
  
  ds <- transform(ds, Age_Range_H_Class = ifelse(Age_Range_H == "00-17", 1, Age_Range_H_Class))
  ds <- transform(ds, Age_Range_H_Class = ifelse(Age_Range_H == "18-25", 2, Age_Range_H_Class))
  ds <- transform(ds, Age_Range_H_Class = ifelse(Age_Range_H == "26-35", 3, Age_Range_H_Class))
  ds <- transform(ds, Age_Range_H_Class = ifelse(Age_Range_H == "36-45", 4, Age_Range_H_Class))
  ds <- transform(ds, Age_Range_H_Class = ifelse(Age_Range_H == "46-55", 5, Age_Range_H_Class))
  ds <- transform(ds, Age_Range_H_Class = ifelse(Age_Range_H == "56-65", 6, Age_Range_H_Class))
  ds <- transform(ds, Age_Range_H_Class = ifelse(Age_Range_H == "66-75", 7, Age_Range_H_Class))
  ds <- transform(ds, Age_Range_H_Class = ifelse(Age_Range_H == "76+", 8, Age_Range_H_Class))
  ds <- transform(ds, Age_Range_H_Class = ifelse(is.na(Age_Range_H), round(mean(ds$Age_Range_H_Class, na.rm = TRUE)), Age_Range_H_Class))
  
  #replace NA values in the Age_Range_H variable
  ds$Age_Range_H[is.na(ds$Age_Range_H)] <- normNAAgeRange(round(mean(ds$Age_Range_H_Class)))
  
  
  return(ds) 
}

## Classify Likely Hood To Recommend by NPS grouping -> Transforms DataSet by adding classifier attributes for Promotor, Passive, Detractor
classifyLikelyhoodToRecommend_Class <- function(ds){
  ds$Likelihood_Recommend_Promotor <- NA
  ds$Likelihood_Recommend_Passive <- NA
  ds$Likelihood_Recommend_Detractor <- NA
  
  ds <- transform(ds, Likelihood_Recommend_Promotor = ifelse(Likelihood_Recommend_H == 9 | Likelihood_Recommend_H == 10,1,0))
  ds <- transform(ds, Likelihood_Recommend_Passive = ifelse(Likelihood_Recommend_H == 7 | Likelihood_Recommend_H == 8,1,0))
  ds <- transform(ds, Likelihood_Recommend_Detractor = ifelse(as.numeric(Likelihood_Recommend_H) <= 6,1,0))
  
  return(ds)
}
classifyLikelyhoodToRecommend_Type <- function(ds){
  ds$Likelihood_Recommend <- NA
  
  ds <- transform(ds, Likelihood_Recommend = ifelse(Likelihood_Recommend_H == 9 | Likelihood_Recommend_H == 10,'PROMOTER',ds$Likelihood_Recommend))
  ds <- transform(ds, Likelihood_Recommend = ifelse(Likelihood_Recommend_H == 7 | Likelihood_Recommend_H == 8,'PASSIVE',ds$Likelihood_Recommend))
  ds <- transform(ds, Likelihood_Recommend = ifelse(as.numeric(Likelihood_Recommend_H) <= 6,'DETRACTOR',ds$Likelihood_Recommend))
  
  return(ds)
}

## Computes SVM Models, returns Accuracy Rating, prints confusion matrix
computeSVMModels <- function(train,test,f){
  
  print(f)
  model <- svm(formula=f, data=train, kernel='radial', C=10, cross=10, prob.model=TRUE)
  print("Training Model:")
  print(model)
  
  # Test the svm model
  predictedValues <- predict(model, test)
  predictedComp <- data.frame(test$Promoter, predictedValues)
  colnames(predictedComp) <- c('test','Pred')
  #print(head(predictedComp))
  
  # Percent of dependent that was correctly predicted
  percAccuracy <- length(which(predictedComp$test==predictedComp$Pred))/dim(predictedComp)[1]
  print(paste0("Percent of dependent correctly predicted: ",percAccuracy))
  
  # Confusion Matrix
  # result output: 0 class, x identified correctly, x identified incorrectly
  # result output: 1 class, x identified incorrectly, x identified correctly
  confMatrixResutls <- table(test=predictedComp$test, pred=predictedComp$Pred)
  print("Confusion Matrix:")
  print(confMatrixResutls)
  
  # Error & Accuracy Score Rate: 
  errorRate <- round((confMatrixResutls[1,][[2]] + confMatrixResutls[2,][[1]]) / nrow(predictedComp) *100,2)
  accuracyRate <- 100-errorRate
  print(paste0("Accuracy Rate: ",accuracyRate,"| Error Rate: ",errorRate))
  
  # return accuracy rate
  # TODO: think returning model
  return(accuracyRate)
}

## Calculate NPS Score
## Subtracting the proportion of detractors from the proportion of promotors can converting it to a percent gets you the Net Promotor Score
getNPSScore <- function(typesTable){
  score <- NULL
  ndetractors <- typesTable[[1]]
  npassives <- typesTable[[2]]
  npromotors <- typesTable[[3]]
  total <- ndetractors+npassives+npromotors
  
  score <- npromotors/total - ndetractors/total
  print(paste0("NPS Score: ",round(score,2)))
  return(round(score,2))
}

# ---Step 1: Data Preparation Processing---------------------------------------------------------------------
### Read Data Set
surveyDataSet <- readDataSet(paste(workingDir,dataSetName, sep=""))
str(surveyDataSet)


### Clean Data Set
surveyDataSetCleaned <- cleanDataSet(surveyDataSet)
str(surveyDataSetCleaned)
head(surveyDataSetCleaned)

## Step 2: Data Exploration, Transformation and Feature Selection

#------ Data Exploration ------------------------------------------------------------------------

### Print Column Names
colnames(surveyDataSetCleaned)
str(surveyDataSetCleaned)
head(surveyDataSetCleaned)

surveyDataSetCleaned <- classifyAgeRanges(surveyDataSetCleaned)


# Evaluate Attribute Values

##---- NPS - Key Driver ---------------------------
### Likelihood_Recommend_H
length(unique(surveyDataSetCleaned$Likelihood_Recommend_H))
sort(table(surveyDataSetCleaned$Likelihood_Recommend_H), decreasing = TRUE)
summary(surveyDataSetCleaned$Likelihood_Recommend_H)
describe(as.numeric(surveyDataSetCleaned$Likelihood_Recommend_H))

## calculate NPS score
surveyDataSetCleaned <- classifyLikelyhoodToRecommend_Type(surveyDataSetCleaned)
tLTR <- table(surveyDataSetCleaned$Likelihood_Recommend)
overallNPSScore <- getNPSScore(tLTR)
overallNPSScore

## Visualizations
ggplot(data=surveyDataSetCleaned) +
  geom_bar(mapping = aes(Likelihood_Recommend_H)) +
  ggtitle('Likelihood to Recommend')
ggsave(filename='Bar_of_Likelihood_Recommend_H.jpg', width = 6, height = 6)

# plot proportions of Detractors, Passives, PROMOTERs
ggplot(data=surveyDataSetCleaned) +
  geom_bar(mapping = aes(Likelihood_Recommend)) +
  ggtitle('Likelihood to Recommend by NPS Type')
ggsave(filename='Bar_of_Likelihood_Recommend_Type.jpg', width = 6, height = 6)

# classify data set as being a Promoter or Not a Promoter
surveyDataSetCleaned$Promoter <- as.factor(ifelse(surveyDataSetCleaned$Likelihood_Recommend=="PROMOTER", 1, 0))
str(surveyDataSetCleaned)
View(head(surveyDataSetCleaned))

# breadk data frame into groups by NPS Type
detractorDf <- surveyDataSetCleaned[surveyDataSetCleaned$Likelihood_Recommend=='DETRACTOR',]
promoterDf <- surveyDataSetCleaned[surveyDataSetCleaned$Likelihood_Recommend=='PROMOTER',]
passiveDf <- surveyDataSetCleaned[surveyDataSetCleaned$Likelihood_Recommend=='PASSIVE',]

# create training and test data sets for prediction modeling
surveyDataSetSplit <- partitionDataSet(surveyDataSetCleaned,0.33)
train <- surveyDataSetSplit$trainingData
dim(train)
head(train)

test <- surveyDataSetSplit$testingData
dim(test)
head(test)

##---- SERVICE - Key Drivers ---------------------------------------------------------------
# Business Questions:
# - Which Service Categories have the most significant impact on an NPS PROMOTER scoring?
# --- Customer_SVC_H & Condition_Hotel_H & Guest_Room_H
service.attributes <- c('Likelihood_Recommend','Likelihood_Recommend_H','Overall_Sat_H','Guest_Room_H','Condition_Hotel_H','Staff_Cared_H','Check_In_H','Internet_Sat_H','Customer_SVC_H','Tranquility_H')
serviceDf <- surveyDataSetCleaned[service.attributes]
head(serviceDf)
summary(serviceDf)

serviceDf <- serviceDf %>%
  mutate(Likelihood_Recommend_H=as.numeric(Likelihood_Recommend_H),Overall_Sat_H=as.numeric(Overall_Sat_H), Guest_Room_H=as.numeric(Guest_Room_H), Condition_Hotel_H=as.numeric(Condition_Hotel_H),
         Staff_Cared_H=as.numeric(Staff_Cared_H), Check_In_H=as.numeric(Check_In_H), Internet_Sat_H=as.numeric(Internet_Sat_H), Customer_SVC_H=as.numeric(Customer_SVC_H),
         Tranquility_H=as.numeric(Tranquility_H))
summary(serviceDf)

## Visualize Descriptive Statistics via Boxplot
ggplot(data=serviceDf, aes(x=factor(0), y=Likelihood_Recommend_H)) +
  geom_boxplot(fill="gray",outlier.colour="red",outlier.shape=16 ,outlier.size=3, notch=TRUE) +
  coord_flip(ylim=c(0,10)) +
  labs(title="Boxplot of Likelihood_Recommend_H", x="", y="Rating Score")
ggsave(filename="Boxplot_of_Likelihood_Recommend_H.jpg", width = 6, height = 6)

service.ratings.melt <- melt(serviceDf,id="Likelihood_Recommend" )
g.box.service.ratings <- ggplot(service.ratings.melt) +
  geom_boxplot(aes(x=variable, y=value, color=Likelihood_Recommend), outlier.shape=16 ,outlier.size=3, notch=TRUE) +
  coord_flip(ylim=c(1,10)) + 
  labs(title="Boxplot of ServiceRatings", x="Service Rating Scores", y="Service Categories") +
  guides(color=guide_legend(title="NPS Types"))
g.box.service.ratings
ggsave(filename="Boxplot_of_ServiceRatings.jpg", width = 6, height = 6)
##

# PROMOTERs
promotorServiceDf <- sqldf("select * from serviceDf where Likelihood_Recommend = 'PROMOTER'")
summary(promotorServiceDf)


# Passives
passiveServiceDf <- sqldf("select * from serviceDf where Likelihood_Recommend = 'PASSIVE'")
summary(passiveServiceDf)

# Detractors
detractorServiceDf <- sqldf("select * from serviceDf where Likelihood_Recommend = 'DETRACTOR'")
summary(detractorServiceDf)

## Average scoring by NPS Type
service.ratings.avgs <- serviceDf %>%
  group_by(Likelihood_Recommend) %>%
  summarise(Overall_Sat_H_AVG=round(mean(Overall_Sat_H),1), Guest_Room_H_AVG=round(mean(Guest_Room_H),1), Condition_Hotel_H_AVG=round(mean(Condition_Hotel_H),1), 
            Staff_Cared_H_AVG=round(mean(Staff_Cared_H),1), Check_In_H_AVG=round(mean(Check_In_H),1), Internet_Sat_H_AVG=round(mean(Internet_Sat_H),1), 
            Customer_SVC_H_AVG=round(mean(Customer_SVC_H),1), Tranquility_H_AVG=round(mean(Tranquility_H),1),
            NPS_TYP_CNT=n(), NPS_TYP_PERC=(n()/nrow(serviceDf))*100)
View(service.ratings.avgs)


## Median scoring by NPS Type
service.ratings.med <- serviceDf %>%
  group_by(Likelihood_Recommend) %>%
  summarise(Overall_Sat_H_MED=round(median(Overall_Sat_H),1), Guest_Room_H_MED=round(median(Guest_Room_H),1), Condition_Hotel_H_MED=round(median(Condition_Hotel_H),1), 
            Staff_Cared_H_MED=round(median(Staff_Cared_H),1), Check_In_H_MED=round(median(Check_In_H),1), Internet_Sat_H_MED=round(median(Internet_Sat_H),1), 
            Customer_SVC_H_MED=round(median(Customer_SVC_H),1), Tranquility_H_MED=round(median(Tranquility_H),1),
            NPS_TYP_CNT=n(), NPS_TYP_PERC=(n()/nrow(serviceDf))*100)
View(service.ratings.med)

# Melt data frame
col.remove <- which(colnames(service.ratings.avgs)=="NPS_TYP_CNT" | colnames(service.ratings.avgs)== "NPS_TYP_PERC")
service.ratings.avgs.melt <- melt(service.ratings.avgs[,-col.remove],id="Likelihood_Recommend")

# Point Plot - Horizontal - Service Rating Averages per Service Category
g.point.service.avgs.ratings <- ggplot(service.ratings.avgs.melt) +
  geom_point(aes(x=variable, y=value, shape=Likelihood_Recommend), size=6, color="green",alpha=.3) +
  coord_flip(ylim=c(1,10)) +
  labs(title="Average of Service Ratings by NPS Type", y="Service Rating Average", x="Service Categories") +
  guides(shape=guide_legend(title="NPS Types"))
g.point.service.avgs.ratings
ggsave(filename="Average_of_Service_Ratings_by_NPS_Type.jpg", width = 6, height = 6)

# Bar Plot - Vertically Stacked, Summed Averages of Service Ratings
ggplot(service.ratings.avgs.melt, aes(x=variable,y=value, fill=Likelihood_Recommend)) +
  geom_bar(stat="identity") +
  theme(axis.text.x=element_text(angle=90,hjust=1)) +
  labs(title="Summed Averages of Service Ratings", y="Summed Averges of Service Ratings", x="Service Rating Categories") + 
  guides(fill=guide_legend(title="NPS Types"))
ggsave(filename="Summed_Averages_of_Service_Ratings.jpg", width = 6, height = 6)



#-- LM Modeling on Dependent Variable, Likelihood_Recommend_H
lmGuestRoom <- lm(Likelihood_Recommend_H ~ Guest_Room_H, data=serviceDf)
sumLMGuestRoom <- summary(lmGuestRoom)
range(serviceDf$Guest_Room_H)
newGuestRoom <- data.frame(Guest_Room_H=10)
predict(lmGuestRoom,newGuestRoom,type="response")

## Create Training and Test data for SVM predictions
serviceDataSetSplits <- partitionDataSet(serviceDf,0.33)
serviceTrain <- serviceDataSetSplits$trainingData
dim(serviceTrain)
head(serviceTrain)

serviceTest <- serviceDataSetSplits$testingData
dim(serviceTest)
head(serviceTest)

#Classify dataset as being either a 'Promoter' or 'Not a Promoter'(i.e. a Detractor)
serviceTrain$Promoter <- ifelse(serviceTrain$Likelihood_Recommend=="PROMOTER", 1, 0)
serviceTest$Promoter <- ifelse(serviceTest$Likelihood_Recommend=="PROMOTER", 1, 0)
removeAttributes <- c('Likelihood_Recommend','Likelihood_Recommend_H','Overall_Sat_H')
serviceTrain <- serviceTrain[,!(names(serviceTrain) %in% removeAttributes)]
serviceTest <- serviceTest[,!(names(serviceTest) %in% removeAttributes)]
str(serviceTrain)
str(serviceTest)
serviceTrain$Promoter <- as.factor(serviceTrain$Promoter)
serviceTest$Promoter <- as.factor(serviceTest$Promoter)


##---- Primary Service Rating Predictors of Promoter -----------------------##
## Guest_Room_H | Condition_Hotel_H | Customer_SVC_H
## Guest_Room_H:
## - 
##-- Compute models and plot the results for 'svm' (in the e1071 package)
formula <- Promoter ~ Guest_Room_H + Customer_SVC_H + Condition_Hotel_H
computeSVMModels(serviceTrain,serviceTest,formula)




###----- Guest_Room_H: Guest room satisfaction metric ------------------------
# 10 Unique value on a 1 to 10 scale
length(unique(serviceDf$Guest_Room_H))
sort(table(serviceDf$Guest_Room_H), decreasing = TRUE)
describe(as.numeric(serviceDf$Guest_Room_H))

## Visualizations
ggplot(data=serviceDf) +
  geom_bar(mapping = aes(Guest_Room_H)) +
  ggtitle('Guest Room Satisfaction')
ggsave(filename='Guest_Room_Satisfaction.jpg', width = 6, height = 6)

# Bivariat plots
plot.biv.guest.likelihood <- ggplot(serviceDf, aes(x=Likelihood_Recommend_H, y=Guest_Room_H)) +
  geom_point() +
  stat_smooth(method='lm',col='red') +
  labs(title="Likelihood to Recommend dependent on Guest Room Ratings", x="Overall Likelihood to Recommend", y="Guest Room Service Rating")
plot.biv.guest.likelihood
ggsave(filename='Likelihood_to_Recommend_dependenton_Guest_Room.jpg', width = 6, height = 6)

# Compute SVM Prediction Accuracy Score
formula <- Promoter ~ Guest_Room_H
computeSVMModels(serviceTrain,serviceTest,formula)

# plot results


#p.guest.promotor <- ggplot()



###----- Condition_Hotel_H: Condition of hotel metric -----------------
# 10 Unique value on a 1 to 10 scale
length(unique(serviceDf$Condition_Hotel_H))
sort(table(serviceDf$Condition_Hotel_H), decreasing = TRUE)
describe(as.numeric(serviceDf$Condition_Hotel_H))

## Visualizations
ggplot(data=serviceDf) +
  geom_bar(mapping = aes(Condition_Hotel_H)) +
  ggtitle('Condition of Hotel Satisfaction Ratings')
ggsave(filename='Condition_of_Hotel_Satisfaction.jpg', width = 6, height = 6)

# Bivariat plots
plot.biv.cond.likelihood <- ggplot(serviceDf, aes(x=Likelihood_Recommend_H, y=Condition_Hotel_H)) +
  geom_point() +
  stat_smooth(method='lm',col='red') +
  labs(title="Likelihood to Recommend dependent on Hotel Condition Ratings", x="Overall Likelihood to Recommend", y="Hotel Condition Service Rating")
plot.biv.cond.likelihood
ggsave(filename='Likelihood_to_Recommend_dependenton_Condition.jpg', width = 6, height = 6)

# Compute SVM Prediction Accuracy Score
formula <- Promoter ~ Condition_Hotel_H
computeSVMModels(serviceTrain,serviceTest,formula)

# plot results

###----- Customer_SVC_H: Quality of customer service metric ----------------
# 10 Unique value on a 1 to 10 scale
length(unique(serviceDf$Customer_SVC_H))
sort(table(serviceDf$Customer_SVC_H), decreasing = TRUE)
describe(as.numeric(serviceDf$Customer_SVC_H))

## Visualizations
ggplot(data=serviceDf) +
  geom_bar(mapping = aes(Customer_SVC_H)) +
  ggtitle('Customer Service Hotel Satisfaction Ratings')
ggsave(filename='Customer_Service_Hotel_Satisfaction.jpg', width = 6, height = 6)

# Bivariot Plot
plot.biv.cust.likelhood <- ggplot(serviceDf, aes(x=Likelihood_Recommend_H, y=Customer_SVC_H)) +
  geom_point() +
  stat_smooth(method='lm',col='red') +
  labs(title="Likelihood to Recommend dependent on Customer Service Ratings", x="Overall Likelihood to Recommend", y="Customer Service Rating")
plot.biv.cust.likelhood
ggsave(filename='Likelihood_to_Recommend_dependenton_Customer_Service.jpg', width = 6, height = 6)

# Compute SVM Prediction Accuracy Score
formula <- Promoter ~ Customer_SVC_H
computeSVMModels(serviceTrain,serviceTest,formula)

# plot results

##-------------------------------------------------------------------------------##



# Show all three results (charts) in one window, using the grid.arrange function
ga3 <- grid.arrange(plot.biv.cust.likelhood, plot.biv.guest.likelihood, plot.biv.cond.likelihood, ncol=1, nrow=3, top="Top 3, Best Service factors for predicting promotors")
ggsave(file="Grid_Arrange_biv_service.jpg", ga3, width = 8, height = 8)

##---- GEOGRAPHY - Key Drivers -------------------
##
# Question: What geolocation has the lowest scoring for service factors, Customer_SVC_H, Condition_Hotel_H, Guest_Room_H
# Service - What Service factors are the best predictors of promoters? 
#   Focusing on improving those Service factors, what hotel geolocation regions should be targeted?
###-------------------------------------------------------------------
#-> 93% of the Top 3 Service detractors are within the United States
###-------------------------------------------------------------------
#-> 94% of Guest Room detractor's are within the United States
#-> 93% of Condition Hotel detractors are within the United States
#-> 92% of Customer Service detractors are within the United States
#---------------------------------------------------------------------

keep <- c('Hotel.Name.Long_PL','Customer_SVC_H','Condition_Hotel_H','Guest_Room_H','Country_PL','City_PL','STATE_R','Location_PL','Likelihood_Recommend_H','Property.Latitude_PL','Property.Longitude_PL')
geo.service.detractors <- detractorDf[,(names(detractorDf) %in% keep)]
geo.service.detractors$Property.Latitude_PL <- plyr::round_any(as.numeric(geo.service.detractors$Property.Latitude_PL), accuracy=.00001, f=floor)
geo.service.detractors$Property.Longitude_PL <- plyr::round_any(as.numeric(geo.service.detractors$Property.Longitude_PL), accuracy=.00001, f=floor)
summary(geo.service.detractors)
head(geo.service.detractors)
describe(as.numeric(geo.service.detractors$Guest_Room_H))
describe(as.numeric(geo.service.detractors$Condition_Hotel_H))
describe(as.numeric(geo.service.detractors$Customer_SVC_H))


# Fix incorrect States
library(zipcode)
z <- data("zipcode")
z <- zipcode
z$latitude <- as.character(z$latitude)
z$longitude <- as.character(z$longitude)

state <- z$state
lat <- plyr::round_any(as.numeric(z$latitude), accuracy=.00001, f=floor)
long <- plyr::round_any(as.numeric(z$longitude), accuracy=.00001, f=floor)

stateLatLong <- data.frame(state,lat,long)
head(stateLatLong)

geoDetractors <- geo.service.detractors

#HERE---

# break dataset down by service type, then remove observations that are 7 or heigher (i.e. focus on detractor level scoring)
remove1 <- c('Customer_SVC_H','Condition_Hotel_H')
gsd.guestRoom <- geo.service.detractors[,!names(geo.service.detractors) %in% remove1]
summary(gsd.guestRoom)
gsd.guestRoom.detractors <- gsd.guestRoom[as.numeric(gsd.guestRoom$Guest_Room_H)<=6,]
summary(gsd.guestRoom.detractors)
head(gsd.guestRoom.detractors)
guest.detract.tbl <- table(gsd.guestRoom.detractors$Country_PL,gsd.guestRoom.detractors$Guest_Room_H)
guest.detract.tbl

remove2 <- c('Customer_SVC_H','Guest_Room_H')
gsd.conditionHotel <- geo.service.detractors[,!names(geo.service.detractors) %in% remove2]
summary(gsd.conditionHotel)
gsd.conditionHotel.detractors <- gsd.conditionHotel[as.numeric(gsd.conditionHotel$Condition_Hotel_H)<=6,]
summary(gsd.conditionHotel.detractors)
head(gsd.conditionHotel.detractors)
cond.detract.tbl <- table(gsd.conditionHotel.detractors$Country_PL,gsd.conditionHotel.detractors$Condition_Hotel_H)
cond.detract.tbl

remove3 <- c('Guest_Room_H','Condition_Hotel_H') 
gsd.customerSVC <- geo.service.detractors[,!names(geo.service.detractors) %in% remove3]
summary(gsd.customerSVC)
gsd.customerSVC.detractors <- gsd.customerSVC[as.numeric(gsd.customerSVC$Customer_SVC_H)<=6,]
summary(gsd.customerSVC.detractors)
head(gsd.customerSVC.detractors)
cust.detract.tbl <- table(gsd.customerSVC.detractors$Country_PL,gsd.customerSVC.detractors$Customer_SVC_H)
cust.detract.tbl

## Subquestion: What proportion of detrctors fall inside the US versus outside?
# 94% of Guest Room detractor's are within the United States
num.us.guestR.detractors <- nrow(gsd.guestRoom.detractors[gsd.guestRoom.detractors$Country_PL=='United States',])
us.guestR.detractor.proportion <- num.us.guestR.detractors/nrow(gsd.guestRoom.detractors)
round(us.guestR.detractor.proportion,2)*100

# 93% of Condition Hotel detractors are within the United States
num.us.conditionH.detractors <- nrow(gsd.conditionHotel.detractors[gsd.conditionHotel.detractors$Country_PL=='United States',])
us.conditionH.detractor.proportion <- num.us.conditionH.detractors/nrow(gsd.conditionHotel.detractors)
round(us.conditionH.detractor.proportion,2)*100

# 92% of Customer Service detractors are within the United States
num.us.customerSVC.detractors <- nrow(gsd.customerSVC.detractors[gsd.customerSVC.detractors$Country_PL=='United States',])
us.customerSVC.detractor.proportion <- num.us.customerSVC.detractors/nrow(gsd.customerSVC.detractors)
round(us.customerSVC.detractor.proportion,2)*100

#-> 93% of the Top 3 Service detractors are within the United States
total.us.detractors <- num.us.guestR.detractors+num.us.conditionH.detractors+num.us.customerSVC.detractors
total.detractos <- nrow(gsd.guestRoom.detractors)+nrow(gsd.conditionHotel.detractors)+nrow(gsd.customerSVC.detractors)
total.us.detractor.proportion <- total.us.detractors/total.detractos
round(total.us.detractor.proportion,2)*100

#################################################################################################################################################
## Function to classify the service rating in to levels, LOW[1-2], MID[3-4], HIGH[5-6]
classifyDetractorRange <- function(ds, serviceType){
  ds$Detractor_Level <- NA
  switch(serviceType,
         "GUEST" = {
           ds <- transform(ds, Detractor_Level = ifelse(Guest_Room_H == 1, "LOW", Detractor_Level))
           ds <- transform(ds, Detractor_Level = ifelse(Guest_Room_H == 2, "LOW", Detractor_Level))
           ds <- transform(ds, Detractor_Level = ifelse(Guest_Room_H == 3, "MID", Detractor_Level))
           ds <- transform(ds, Detractor_Level = ifelse(Guest_Room_H == 4, "MID", Detractor_Level))
           ds <- transform(ds, Detractor_Level = ifelse(Guest_Room_H == 5, "HIGH", Detractor_Level))
           ds <- transform(ds, Detractor_Level = ifelse(Guest_Room_H == 6, "HIGH", Detractor_Level))
         },
         "CUSTOMER" = {
           ds <- transform(ds, Detractor_Level = ifelse(Customer_SVC_H == 1, "LOW", Detractor_Level))
           ds <- transform(ds, Detractor_Level = ifelse(Customer_SVC_H == 2, "LOW", Detractor_Level))
           ds <- transform(ds, Detractor_Level = ifelse(Customer_SVC_H == 3, "MID", Detractor_Level))
           ds <- transform(ds, Detractor_Level = ifelse(Customer_SVC_H == 4, "MID", Detractor_Level))
           ds <- transform(ds, Detractor_Level = ifelse(Customer_SVC_H == 5, "HIGH", Detractor_Level))
           ds <- transform(ds, Detractor_Level = ifelse(Customer_SVC_H == 6, "HIGH", Detractor_Level))
         },
         "CONDITION" = {
           ds <- transform(ds, Detractor_Level = ifelse(Condition_Hotel_H == 1, "LOW", Detractor_Level))
           ds <- transform(ds, Detractor_Level = ifelse(Condition_Hotel_H == 2, "LOW", Detractor_Level))
           ds <- transform(ds, Detractor_Level = ifelse(Condition_Hotel_H == 3, "MID", Detractor_Level))
           ds <- transform(ds, Detractor_Level = ifelse(Condition_Hotel_H == 4, "MID", Detractor_Level))
           ds <- transform(ds, Detractor_Level = ifelse(Condition_Hotel_H == 5, "HIGH", Detractor_Level))
           ds <- transform(ds, Detractor_Level = ifelse(Condition_Hotel_H == 6, "HIGH", Detractor_Level))
           
         }
         
         )

  return(ds) 
}
##---- Plot US Maps ------
us.map <- map_data("state")

removeThemeAxis <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank()
)
## GUEST_ROOM DETRACTORS US ##
# filter detractor data frames to focus on US
gsd.us.guestRoom.detractors <- gsd.guestRoom.detractors[gsd.guestRoom.detractors$Country_PL=='United States',]
gsd.us.guestRoom.detractors <- classifyDetractorRange(gsd.us.guestRoom.detractors,"GUEST")
head(gsd.us.guestRoom.detractors[order(gsd.us.guestRoom.detractors[,3]),])

# Table of unique hotels by STATE_R
guestDf <- gsd.us.guestRoom.detractors
guestDf$STATE_R <- as.character(guestDf$STATE_R)
names(guestDf)

guestDf$state_name <- tolower(state.name[match(guestDf$STATE_R,state.abb)])
guestDf <- subset(guestDf,!is.na(state_name))
head(guestDf[order(guestDf[,2],decreasing = TRUE),],5)

guestRoomDetractors.by.state.hotelCount <- sqldf("select STATE_R, count('Hotel.Name.Long_PL') as Hotel_Count,
                                                 round(avg(Guest_Room_H),2) as Guest_Room_Avg,
                                                 count(case when Detractor_Level='LOW' then 1 else null end) as Detractor_Low_Cnt,
                                                 count(case when Detractor_Level='MID' then 1 else null end) as Detractor_Mid_Cnt,
                                                 count(case when Detractor_Level='HIGH' then 1 else null end) as Detractor_High_Cnt
                                                 from guestDf where STATE_R <> 'NOT_LISTED' group by STATE_R")
head(guestRoomDetractors.by.state.hotelCount[order(guestRoomDetractors.by.state.hotelCount[,2],decreasing = TRUE),],5)

# Some state abreviations are wrong in the survey data set, remove them from the dataset
guestRoomDetractors.by.state.hotelCount$state_name <- tolower(state.name[match(guestRoomDetractors.by.state.hotelCount$STATE_R,state.abb)])
guestRoomDetractors.by.state.hotelCount <- subset(guestRoomDetractors.by.state.hotelCount,!is.na(state_name))


#--- Map Plotting Detractors - Guest Room Rating, US Map by State & Hotel Count
# Top 5 States with the most number of Hotels with a Guest Room Service Rating as Detractors:
# 1: TX with 125 Hotels
# 2: CA with 86 Hotels
# 3: FL with 68 Hotels
# 4: NC with 45 Hotels
# 5: IL with 37 Hotles
#---
guestRoomDetractors.by.state.hotelCount[order(guestRoomDetractors.by.state.hotelCount[,2],decreasing = TRUE),]
map.detractors.guestR <- ggplot(data=guestRoomDetractors.by.state.hotelCount, mapping=aes(map_id=state_name))
map.detractors.guestR <- map.detractors.guestR + geom_map(map=us.map, mapping=aes(fill=Hotel_Count))
map.detractors.guestR <- map.detractors.guestR + scale_fill_gradient2(low="#559999", mid="grey90", high="#BB650B",midpoint = mean(guestRoomDetractors.by.state.hotelCount$Hotel_Count))
map.detractors.guestR <- map.detractors.guestR + expand_limits(x=us.map$long, y=us.map$lat)

map.detractors.guestR <- map.detractors.guestR + coord_map("polyconic")
map.detractors.guestR <- map.detractors.guestR + ggtitle("Guest Room Service Rating, Detractors by State") + theme(plot.title=element_text(hjust=0.5))
map.detractors.guestR <- map.detractors.guestR + guides(fill=guide_legend(title="State Hotel Count")) + removeThemeAxis
map.detractors.guestR
ggsave("U.S._Map_of_Service_Detractors_Guest_Room.jpg", width = 6, height = 6)




## CONDITION HOTEL DETRACTORS
gsd.us.conditionHotel.detractors <- gsd.conditionHotel.detractors[gsd.conditionHotel.detractors$Country_PL=='United States',]
gsd.us.conditionHotel.detractors <- classifyDetractorRange(gsd.us.conditionHotel.detractors,"CONDITION")
head(gsd.us.conditionHotel.detractors[order(gsd.us.conditionHotel.detractors[,4]),])
conditionDf <- gsd.us.conditionHotel.detractors
conditionDetractors.by.state.hotelCount <- sqldf("select STATE_R, count('Hotel.Name.Long_PL') as Hotel_Count,
                                                 round(avg(Condition_Hotel_H),2) as Condition_Avg,
                                                 count(case when Detractor_Level='LOW' then 1 else null end) as Detractor_Low_Cnt,
                                                 count(case when Detractor_Level='MID' then 1 else null end) as Detractor_Mid_Cnt,
                                                 count(case when Detractor_Level='HIGH' then 1 else null end) as Detractor_High_Cnt
                                                 from conditionDf where STATE_R <> 'NOT_LISTED' group by STATE_R")
head(conditionDetractors.by.state.hotelCount[order(conditionDetractors.by.state.hotelCount[,2],decreasing = TRUE),],5)

#--- Map Plotting Detractors - Condition Hotel Rating, US Map by State & Hotel Count
# Top 5 States with the most number of Hotels with a Hotel Condition Service Rating as Detractors:
# 1: TX with 109 Hotels
# 2: CA with 83 Hotels
# 3: FL with 53 Hotels
# 4: NC with 38 Hotels
# 5: NY with 33 Hotles
#---
conditionDetractors.by.state.hotelCount[order(conditionDetractors.by.state.hotelCount[,2],decreasing = TRUE),]
# Some state abreviations are wrong in the survey data set, remove them from the dataset
conditionDetractors.by.state.hotelCount[order(conditionDetractors.by.state.hotelCount[,2],decreasing = TRUE),]
conditionDetractors.by.state.hotelCount$state_name <- tolower(state.name[match(conditionDetractors.by.state.hotelCount$STATE_R,state.abb)])
conditionDetractors.by.state.hotelCount <- subset(conditionDetractors.by.state.hotelCount,!is.na(state_name))

conditionDetractors.by.state.hotelCount[order(conditionDetractors.by.state.hotelCount[,2],decreasing = TRUE),]
map.detractors.condition <- ggplot(data=conditionDetractors.by.state.hotelCount, mapping=aes(map_id=state_name))
map.detractors.condition <- map.detractors.condition + geom_map(map=us.map, mapping=aes(fill=Hotel_Count))
map.detractors.condition <- map.detractors.condition + scale_fill_gradient2(low="#559999", mid="grey90", high="#BB650B",midpoint = mean(conditionDetractors.by.state.hotelCount$Hotel_Count))
map.detractors.condition <- map.detractors.condition + expand_limits(x=us.map$long, y=us.map$lat)
map.detractors.condition <- map.detractors.condition + coord_map()
map.detractors.condition <- map.detractors.condition + ggtitle("Hotel Condition Service Rating, Detractors by State") + theme(plot.title=element_text(hjust=0.5))
map.detractors.condition <- map.detractors.condition + guides(fill=guide_legend(title="State Hotel Count")) + removeThemeAxis
map.detractors.condition
ggsave("U.S._Map_of_Service_Detractors_Condition.jpg", width = 6, height = 6)


## CUSTOMER SERVICE DETRACTORS
gsd.us.customerSVC.detractors <- gsd.customerSVC.detractors[gsd.customerSVC.detractors$Country_PL=='United States',]
gsd.us.customerSVC.detractors <- classifyDetractorRange(gsd.us.customerSVC.detractors,"CUSTOMER")
head(gsd.us.customerSVC.detractors[order(gsd.us.customerSVC.detractors[,3]),])
customerSVCDf <- gsd.us.customerSVC.detractors
customerDetractors.by.state.hotelCount <- sqldf("select STATE_R, count('Hotel.Name.Long_PL') as Hotel_Count,
                                                 round(avg(Customer_SVC_H),2) as CustomerSVC_Avg,
                                                 count(case when Detractor_Level='LOW' then 1 else null end) as Detractor_Low_Cnt,
                                                 count(case when Detractor_Level='MID' then 1 else null end) as Detractor_Mid_Cnt,
                                                 count(case when Detractor_Level='HIGH' then 1 else null end) as Detractor_High_Cnt
                                                 from customerSVCDf where STATE_R <> 'NOT_LISTED' group by STATE_R")
head(customerDetractors.by.state.hotelCount[order(customerDetractors.by.state.hotelCount[,2],decreasing = TRUE),],5)

#--- Map Plotting Detractors - Customer Service Rating, US Map by State & Hotel Count
# Top 5 States with the most number of Hotels with a Hotel Condition Service Rating as Detractors:
# 1: TX with 102 Hotels
# 2: CA with 80 Hotels
# 3: FL with 47 Hotels
# 4: IL with 39 Hotels
# 5: NY with 32 Hotles
#---
customerDetractors.by.state.hotelCount[order(customerDetractors.by.state.hotelCount[,2],decreasing = TRUE),]
# Some state abreviations are wrong in the survey data set, remove them from the dataset
customerDetractors.by.state.hotelCount$state_name <- tolower(state.name[match(customerDetractors.by.state.hotelCount$STATE_R,state.abb)])
customerDetractors.by.state.hotelCount <- subset(customerDetractors.by.state.hotelCount,!is.na(state_name))

customerDetractors.by.state.hotelCount[order(customerDetractors.by.state.hotelCount[,2],decreasing = TRUE),]
map.detractors.customer <- ggplot(data=customerDetractors.by.state.hotelCount, mapping=aes(map_id=state_name))
map.detractors.customer <- map.detractors.customer + geom_map(map=us.map, mapping=aes(fill=Hotel_Count))
map.detractors.customer <- map.detractors.customer + scale_fill_gradient2(low="#559999", mid="grey90", high="#BB650B",midpoint = mean(customerDetractors.by.state.hotelCount$Hotel_Count))
map.detractors.customer <- map.detractors.customer + expand_limits(x=us.map$long, y=us.map$lat)
map.detractors.customer <- map.detractors.customer + coord_map()
map.detractors.customer <- map.detractors.customer + ggtitle("Customer Service Rating, Detractors by State") + theme(plot.title=element_text(hjust=0.5))
map.detractors.customer <- map.detractors.customer + guides(fill=guide_legend(title="State Hotel Count")) + removeThemeAxis
map.detractors.customer
ggsave("U.S._Map_of_Service_Detractors_Customer.jpg", width = 6, height = 6)


#-------------------------------------------------------------------------------------------------------------------------------------------




### Country_PL
length(unique(table(surveyDataSetCleaned$Country_PL)))
sort(table(surveyDataSetCleaned$Country_PL), decreasing = TRUE)

### Evealuate Country_PL in the detractor portion of the dataset
length(unique(table(detractorDf$Country_PL)))
sort(table(detractorDf$Country_PL), decreasing = TRUE)


### Country_PL - Visualizations

### City_PL
length(unique(surveyDataSetCleaned$City_PL))
sort(table(surveyDataSetCleaned$Country_PL), decreasing = TRUE)

### Evealuate City_PL in the detractor portion of the dataset
length(unique(detractorDf$City_PL))
sort(table(detractorDf$City_PL), decreasing = TRUE)


### City_PL - Visualizations
boxplot(table(surveyDataSetCleaned$City_PL))
barplot(table(surveyDataSetCleaned$City_PL))

### Location_PL
length(unique(surveyDataSetCleaned$Location_PL))
sort(table(surveyDataSetCleaned$Location_PL), decreasing = TRUE)

### Evealuate Location_PL in the detractor portion of the dataset
length(unique(detractorDf$Location_PL))
sort(table(detractorDf$Location_PL), decreasing = TRUE)


### STATE_R
length(unique(surveyDataSetCleaned$STATE_R))
sort(table(surveyDataSetCleaned$STATE_R), decreasing = TRUE)

### Evealuate STATE_R in the detractor portion of the dataset
length(unique(detractorDf$STATE_R))
sort(table(detractorDf$STATE_R), decreasing = TRUE)

### STATE_R - Visualizations


#---- Map Plotting World ----
#####################################################################
#####################################################################
# surveyDataSetCleaned$City_PL
# surveyDataSetCleaned$Property.Latitude_PL
# surveyDataSetCleaned$Property.Longitude_PL
# surveyDataSetCleaned$NPS_Type
# surveyDataSetCleaned$Condition_Hotel_H

sds <- surveyDataSetCleaned
sds$NPS <- NA
sds$NPS <- ifelse(sds$NPS_Type=="Detractor", 0, ifelse(sds$NPS_Type=="Passive", 1, 2))

countPeople <- function(ds) {
  tap <- tapply(ds$NPS, ds$City_PL, length)
  return(tap)
}


meanCondition <- function(ds) {
  tap <- tapply(ds$Condition_Hotel_H, ds$City_PL, mean)
  return(tap)
}

meanGuestRoom <- function(ds){
  tap <- tapply(ds$Guest_Room_H, ds$City_PL, mean)
  return(tap)
}

meanCustService <- function(ds){
  tap <- tapply(ds$Customer_SVC_H, ds$City_PL, mean)
  return(tap)
}

sds$Condition_Hotel_H <- as.numeric(sds$Condition_Hotel_H)

createNPSDS <- function(ntype) {
  count <- countPeople(sds[sds$NPS_Type==ntype,])
  meancondition <- meanCondition(sds[sds$NPS_Type==ntype,])*20
  
  ds <- data.frame(count, meancondition)
  ds$cityname <- NA
  ds$latitude <- NA
  ds$longitude <- NA
  #View(ds_det)
  
  for(i in 1:nrow(ds)) {
    ds$cityname[i] <- rownames(ds)[i]
    ds$latitude[i] <- sds$Property.Latitude_PL[sds$City_PL==rownames(ds)[i]][1]
    ds$longitude[i] <- sds$Property.Longitude_PL[sds$City_PL==rownames(ds)[i]][1]
  }
  rownames(ds) <- NULL
  
  return(ds)
}

ds_det <- createNPSDS("Detractor")
ds_pas <- createNPSDS("Passive")
ds_pro <- createNPSDS("Promoter")

View(ds_det)
View(ds_pas)
View(ds_pro)

###################
## World Map
###################
#install.packages("maps")
world <- map_data("world")
breakC <- c(50, 100, 200)

createWorldMap <- function(ds, Title, Filename) {
  worldmap <- ggplot() +
    geom_polygon(data = world, aes(x = long, y = lat, group = group),
                 fill="white", colour="gray50", alpha=1) +
    coord_fixed(1.3)
  worldmap <- worldmap + 
    geom_point(data=ds, aes(x=ds$longitude, y=ds$latitude, color=ds$meancondition, size=ds$count)) 
  worldmap <- worldmap + 
    scale_colour_gradientn(limits=c(50, 200), colours=c("red", "black","deepskyblue"),
                           breaks=breakC, labels=format(breakC))
  worldmap <- worldmap +
    labs(title=Title, x="", y="", color="Condition", size="Count")
  ggsave(filename=Filename, width = 6, height = 6)
  return(worldmap)  
}

worldmap_det <- createWorldMap(ds_det, "Detractor", "WorldMap_Detractor.jpg")
worldmap_det
worldmap_pas <- createWorldMap(ds_pas, "Passive", "WorldMap_Passive.jpg")
worldmap_pas
worldmap_pro <- createWorldMap(ds_pro, "Promoter", "WorldMap_Promoter.jpg")
worldmap_pro








#---- Test Zone --------------------

#------ Data Feature Selection ------------------------------------------------------------------

