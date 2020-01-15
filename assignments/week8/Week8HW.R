## Homework Week 8: Viz Map HW: Making Predictions - Explore Antelope Populations

#--- Data Details -------------------------------------------------------------------
# 4 columns, 8 years of observations (n=8)
# 1st Column: Number of fawn in a given spring
# 2nd Column: Population of adult antelope
# 3rd column: Annual precipitation that year
# 4th column: value representing how bad the winter was during that year
# 

#--- Preprocess Steps: ----------------------------------------------------------------------

### Clear objects from Memory
rm(list=ls())
### Clear Console:
cat("\014")
### Set Working Directory
setwd("C:\\workspaces\\ms_datascience_su\\IST687-IntroDataScience\\R_workspace\\hw")

#---- Global Variable Assignments --------------------------------------------
antelopeDataSetURL <- "http://college.cengage.com/mathematics/brase/understandable_statistics/7e/students/datasets/mlr/excel/mlr01.xls"
# Path to local Perl Interpreter - Needed to gdata package for read.xls()
perlPath <- "C:\\Strawberry\\perl\\bin\\perl"

#---- Load Required Packages -------------------------------------------------
if(!require("devtools")) {install.packages("devtools")}
if(!require("RCurl")) {install.packages("RCurl")}
if(!require("gdata")) {install.packages("gdata")}
if(!require("ggplot2")) {install.packages("ggplot2")}
if(!require("MASS")) {install.packages("MASS")}
#if(!require("tidyverse")) {install.packages("tidyverse")}

# ---- Step 1: Load the data -----------------------------------------------------------------------------
## 1.1: Read the data:
readDataSetasXLSX <- function(dsURL,locPerlPath){
  
  df <- read.xls(dsURL, perl=locPerlPath)
  
  return(data.frame(df))
}

antelopeDf <- readDataSetasXLSX(antelopeDataSetURL,perlPath)

# ---- Step 2: Explore the data -----------------------------------------------------------------------------
str(antelopeDf)

# ---- Step 3: Clean the data -----------------------------------------------------------------------------
# Rename Column Headings
newColumnNames <- c('Fawn_Cnt','Adult_Antelope_Pop','Annual_Percipitation','Bad_Winter_Scale')
colnames(antelopeDf) <- newColumnNames

# ---- Step 4: Create bivariate plots -----------------------------------------------------------------------------
#
# Create bivariate plots of number of baby fawns versus adult antelope population,
# the precipitation that year, and the severity of the winter. Your code should produce
# three separate plots. Make sure the Y-axis and X-axis are labeled. Keeping in mind
# that the number of fawns is the outcome (or dependent) variable, which axis should
# it go on in your plots?
# Attribute Type: 
#   - Precipitation: Independent / Numeric:Continuous
#   - Severity of Winter: Independent / Categorical (Scale[1-5])
#   - Baby Count: Dependent / Numeric:Continuous
#   - Adult Population: Independent / Numeric:Continuous


## Plots - Focus is on the dependent variable, Baby Fawn Count

# Plot 1: Number of baby fawns versus adult antelope population
# x-axis: Adult_Antelope_Pop
# y-axis: Fawn_Cnt
g.fawn.adult <- ggplot(antelopeDf, aes(x=Adult_Antelope_Pop, y=Fawn_Cnt)) +
  geom_point() +
  stat_smooth(method='lm',col='red') +
  labs(title="Bivariate Plot: \nNumber of baby fawns born \n  versus \nAdult antelope population", x="Annual Adult Antelope Population", y="Number of Fawns born in Spring")
g.fawn.adult
ggsave("Bivariate_Plot_Baby_Fawns_Adult_Population.jpg", width = 6, height = 6)

# Plot 2: Number of baby fawns given the precipitation that year
# x-axis: Annual_Percipitation
# y-axis: Fawn_Cnt
g.fawn.percipitation <- ggplot(antelopeDf, aes(x=Annual_Percipitation, y=Fawn_Cnt)) +
  geom_point() +
  stat_smooth(method='lm',col='red') +
  labs(title="Bivariate Plot: \nNumber of baby fawns born \n  versus \nAnnual percipitation", x="Annual Percipitation", y="Number of Fawns born in Spring")
g.fawn.percipitation
ggsave("Bivariate_Plot_Baby_Fawns_Precipitation.jpg", width = 6, height = 6)

# Plot 3: Number of baby fawns given the severity of the winter
# x-axis: Bad_Winter_Scale
# y-axis: Fawn_Cnt
g.fawn.winter <- ggplot(antelopeDf, aes(x=Bad_Winter_Scale, y=Fawn_Cnt)) +
  geom_point() +
  stat_smooth(method='lm',col='red') +
  labs(title="Bivariate Plot: ", x="Winter Severity Rating", y="Number of Fawns born in Spring")
g.fawn.winter
ggsave("Bivariate_Plot_Baby_Fawns_Bad_Weather_Rating.jpg", width = 6, height = 6)

# ---- Step 5: Create regression models -----------------------------------------------------------------------------
#
# Create three regression models of increasing complexity using lm().

# Step 5.1: Predict the number of fawns from the severity of the winter
# Output: Multiple R-squared = 0.5459 | p-value = 0.03626
fawn.winter.lm <- lm(Fawn_Cnt ~ Bad_Winter_Scale, data=antelopeDf)
sum.f.w <- summary(fawn.winter.lm)
sum.f.w
range(antelopeDf$Bad_Winter_Scale)
newWinterData <- data.frame(Bad_Winter_Scale=3)
predict(fawn.winter.lm,newWinterData,type="response")

# Step 5.2: Predict the number of fawns from two variables (one should be the severity of the winter)
# Output: Adjusted R-squared = 0.8439 | p-value = 0.004152
fawn.AntelopePop.Winter.lm <- lm(Fawn_Cnt ~ Adult_Antelope_Pop+Bad_Winter_Scale, data=antelopeDf)
sum.f.a.w <- summary(fawn.AntelopePop.Winter.lm)
sum.f.a.w
range(antelopeDf$Adult_Antelope_Pop)
newAntelopeWinterData <- data.frame(Adult_Antelope_Pop=9, Bad_Winter_Scale=1)
predict(fawn.AntelopePop.Winter.lm,newAntelopeWinterData,type='response')

# Step 5.3: Predict the number of fawns from the three other variables
# Output: Adjusted R-squared = 0.955 | p-value = 0.001229
fawn.AntelopePop.Percipitation.Winter.lm <- lm(Fawn_Cnt ~ Adult_Antelope_Pop+Annual_Percipitation+Bad_Winter_Scale, data=antelopeDf)
sum.f.a.p.w <- summary(fawn.AntelopePop.Percipitation.Winter.lm)
sum.f.a.p.w
range(antelopeDf$Annual_Percipitation)
newAntelopePopPercipitationWinterData <- data.frame(Adult_Antelope_Pop=5, Annual_Percipitation=14 ,Bad_Winter_Scale=4)
predict(fawn.AntelopePop.Percipitation.Winter.lm, newAntelopePopPercipitationWinterData, type='response')

# Step 5.4: Questions to Answer
# Question 5.4.1: Which model works best?
# The third model, fawn.AntelopePop.Percipitation.Winter.lm, it has an Adjust R-squared score of 0.955 and p-value of 0.001 (statistically significant)
paste("Adjusted R-squared:",sum.f.a.p.w$adj.r.squared)
paste("P-Value:")
sum.f.a.p.w$coefficients[,4]

# Question 5.4.2: Which of the predictors are statistically significant in each model?
# Model 1: Number of fawns from the severity of the winter
# Answer: Bad Winter Rating has a predictor confidence of 55% accuracy
paste("Multiple R-squared:",sum.f.w$r.squared)
paste("P-Value:")
sum.f.w$coefficients[,4]

# Model 2: Number of fawns from the Adult Antelope Population and Bad Winter Rating
# Answer: Adult Antelope Population has a prediction confidence accuracy of 84%, Bad Winter is not statistically significant in this model
paste("Adjusted R-squared:",sum.f.a.w$adj.r.squared)
paste("P-Value:")
sum.f.a.w$coefficients[,4]

# Model 3: number of fawns from the Adult Antelope Population, Annual Percipitation and Bad Winter Rating
# Answer: Adult Antelope Population, Annual Percipitation and Bad Winter Rating are all statistically significant
#         and combined have a prediction confidence accuracy of 95%
paste("Adjusted R-squared:",sum.f.a.p.w$adj.r.squared)
paste("P-Value:")
sum.f.a.p.w$coefficients[,4]


# Question 5.4.3: If you wanted to create the most parsimonious model (i.e.,
#                 the one that did the best job with the fewest predictors), what would it contain?
# Answer:
#   Analysis of Deviance Table
#   Initial Model:
#     Fawn_Cnt ~ Adult_Antelope_Pop + Annual_Percipitation + Bad_Winter_Scale
#   Final Model:
#     Fawn_Cnt ~ Adult_Antelope_Pop + Annual_Percipitation + Bad_Winter_Scale

# Apply Stepwise regression analysis, find best combination of variables for best prediction output
lm.step <- stepAIC(fawn.AntelopePop.Percipitation.Winter.lm,direction="both")
lm.step$anova
summary(lm.step)
plot(lm.step)


## Other individual attribute tests

# Output: Multiple R-squared = 0.8813 | p-value = 0.0005471
fawn.apop <- lm(Fawn_Cnt ~ Adult_Antelope_Pop, data=antelopeDf)
sum.fawn.apop <- summary(fawn.apop)
sum.fawn.apop
# Output: Multiple R-squared = 0.8536 | p-value = 0.001039
fawn.aperc <- lm(Fawn_Cnt ~ Annual_Percipitation, data=antelopeDf)
sum.fawn.aperc <- summary(fawn.aperc)
sum.fawn.aperc





