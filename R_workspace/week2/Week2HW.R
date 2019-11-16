# Clear objects from Memory
rm(list=ls())
# Clear Console:
cat("\014")


# Homework Week 2 Objective: Explore the mtcars dataset
## Copy the mtcars dataset into a new variable called myCars
myCars <- mtcars
str(myCars)
summary(myCars)
row.names(myCars)

#Step 1: What is the hp
## Q1: What is the highest hp?
maxHp <- max(myCars$hp)
maxHp

## Q2: Which car has the highest hp?
carMaxHp <- myCars[myCars$hp == max(myCars$hp),]
carNameMaxHp <- row.names(carMaxHp)
carNameMaxHp

#---------------------------------------
#Step 2: Explore mpg

## Q3: What is the highest mpg?
maxMPG <- max(myCars$mpg)
maxMPG

## Q4: Which car has the highest mpg?
carMaxMPG <- myCars[myCars$mpg == max(myCars$mpg),]
carMaxMPG
carNameMaxMPG <- row.names(carMaxMPG)
carNameMaxMPG

## Q5: Create a sorted dataframe, based on mpg
sortedMyCars <- myCars[order(-myCars$mpg),]
sortedMyCars

#---------------------------------------
#Step 3; Which car has the "best" combination of mpg and hp?
## Q6: What logic did you use?
carsByMPGAndHP <- data.frame(sortedMyCars$mpg,sortedMyCars$hp,row.names = row.names(sortedMyCars))
colnames(carsByMPGAndHP) <- c('mpg','hp')
carsByMPGAndHP$eff <- carsByMPGAndHP$mpg/carsByMPGAndHP$hp
carBestEff <- carsByMPGAndHP[carsByMPGAndHP$eff == max(carsByMPGAndHP$eff),]
carBestEff

## Q7: Which car?
carNameBestEff <- row.names(carBestEff)
carNameBestEff

#---------------------------------------
#Step 4: Which car has "best" car combination of mpg and hp, where mpg and hp must be given equal weight?
hist(carsByMPGAndHP$mpg)
mpg.z <- scale(carsByMPGAndHP$mpg)
hist(mpg.z)

hist(carsByMPGAndHP$hp)
hp.z <- scale(carsByMPGAndHP$hp)
#hist(hp.z)

eff.z <- mpg.z/hp.z
#hist(eff.z)

scaledBestEff <- data.frame(mpg.z,hp.z,eff.z, row.names = row.names(carsByMPGAndHP))
#scaledBestEff

carScaledBestEff <- scaledBestEff[scaledBestEff$eff.z == max(scaledBestEff$eff.z),]
carScaledBestEff

carNameScaledBestEff <- row.names(carScaledBestEff)
carNameScaledBestEff

carsByMPGAndHP[carNameScaledBestEff,]
