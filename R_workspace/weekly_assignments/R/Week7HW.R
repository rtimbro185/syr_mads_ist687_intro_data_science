## Homework Week 7: Viz Map HW: Median Income

#--- Preprocess Steps:----------------------------------------------------------------------

### Clear objects from Memory
rm(list=ls())
### Clear Console:
cat("\014")
### Set Working Directory
setwd("C:\\workspaces\\ms_datascience_su\\IST687-IntroDataScience\\R_workspace\\hw")

#---- Global Variable Assignments --------------------------------------------
incomeDataSetFileName <- 'MedianZIP_2_2.xlsx'

#---- Load Required Packages -------------------------------------------------
if(!require("devtools")) {install.packages("devtools")}
devtools::install_github("dkahle/ggmap")

if(!require("readxl")) {install.packages("readxl")}
if(!require("gdata")) {install.packages("gdata")}
if(!require("ggplot2")){install.packages("ggplot2")}
if(!require("ggmap")){install.packages("ggmap")}
if(!require("mapproj")){install.packages("mapproj")}
if(!require("dplyr")) {install.packages("dplyr")}
if(!require("sqldf")) {install.packages("sqldf")}
if(!require("zipcode")) {install.packages("zipcode")}
if(!require("reshape2")) {install.packages("reshape2")}

# Register Google API
register_google(key="AIzaSyDdjiKuumlpQunYJxtMYEdEq5o32QJgJ28")

#---- Step 1: Load the data ---------------------------------------------------

## 1.1: Read the data:
readDataSetasXLSX <- function(fName){
  
  ds <- read_xlsx(fName)
  
  return(data.frame(ds))
}

income.df <- readDataSetasXLSX(incomeDataSetFileName)
str(income.df)
head(income.df)

## 1.2: Clean the dataframe:
 cleanIncomeDf <- function(ds){
  ## Remove Columns
  colnames(ds) <- NULL
  ds <- ds[-1,]
  
  ## Rename Columns
  newColnames <- c('zip', 'median', 'mean', 'population')
  colnames(ds) <- newColnames
  
  ## Remove commas and make numeric
  ds$median <- as.numeric(gsub(",","",ds$median))
  ds$mean <- as.numeric(gsub(",","",ds$mean))
  ds$population <- as.numeric(gsub(",","",ds$population))

  return(ds)
}

income.df <- cleanIncomeDf(income.df)
str(income.df)
head(income.df)

## 1.3: Load the 'zipcode' package:
data(zipcode)
head(zipcode)

## Reformat zip codes
income.df$zip <- clean.zipcodes(income.df$zip)
head(income.df$zip)

## 1.4: Merge the zip code information from the two data frames (merge into one dataframe)
income.by.zipcode.df <- merge(income.df,zipcode, by='zip')
head(income.by.zipcode.df)

## 1.5: Remove the Hawaii and Alaska (just focus on the 'lower 48' states)
income.by.zipcode.df <- income.by.zipcode.df[income.by.zipcode.df$state != 'HI',]
income.by.zipcode.df <- income.by.zipcode.df[income.by.zipcode.df$state != 'AK',]
income.by.zipcode.df <- income.by.zipcode.df[income.by.zipcode.df$state != 'DC',]


#---- Step 2: Show the income & population per state -------------------------

## 2.1: Create a simpler dataframe, with just the average median income and the population for each state.
# Average Median Income
income <- tapply(income.by.zipcode.df$median, income.by.zipcode.df$state, mean)
state <- rownames(income)
median.income <- data.frame(state,income)

# Population for each state
pop <- tapply(income.by.zipcode.df$population, income.by.zipcode.df$state, sum)
state <- rownames(pop)
state.pop <- data.frame(state,pop)

# Merge the above two data frames by 'state'
income.by.state.simp.df <- merge(median.income,state.pop,by="state")
head(income.by.state.simp.df)

# Alternative method of simplifying data frame
incomeByZipDf <- income.by.zipcode.df
incomeByStateSimpAltDf <- sqldf("select state, avg(median) as income, sum(population) as pop from incomeByZipDf group by state")
#incomeByStateSimpAltDf <- sqldf("select state, (income/pop) as income, pop from incomeByStateSimpAltDf")

## 2.2: Add the state abbreviations and the state names as new columns (make sure the state names are all lower case)
income.by.state.simp.df$state_name <- tolower(state.name[match(income.by.state.simp.df$state,state.abb)])

## 2.3: Show the U.S. map, representing the color with the average median income of that state
removeThemeAxis <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank()
)

us.map <- map_data("state")
g.map.income <- ggplot(data=income.by.state.simp.df, mapping=aes(map_id=state_name))
g.map.income <- g.map.income + geom_map(map=us.map, mapping = aes(fill=income))
g.map.income <- g.map.income + expand_limits(x=us.map$long, y=us.map$lat)

g.map.income <- g.map.income + coord_map()
g.map.income <- g.map.income + ggtitle("Average Median Income by State") + theme(plot.title=element_text(hjust=0.5))
g.map.income <- g.map.income + guides(fill=guide_legend(title="Income")) + removeThemeAxis
g.map.income
ggsave("U.S._Map_of_Average_Median_Income_by_State.jpg", width = 6, height = 6)


## 2.4: Create a second map with color representing the population of the state
g.map.pop <- ggplot(data=income.by.state.simp.df, mapping=aes(map_id=state_name))
g.map.pop <- g.map.pop + geom_map(map=us.map, mapping = aes(fill=pop))
g.map.pop <- g.map.pop + expand_limits(x=us.map$long, y=us.map$lat)

g.map.pop <- g.map.pop + coord_map()
g.map.pop <- g.map.pop + ggtitle("State Population") + theme(plot.title=element_text(hjust=0.5))
g.map.pop <- g.map.pop + guides(fill=guide_legend(title="Population")) + removeThemeAxis
g.map.pop
ggsave("U.S._Map_of_Population_by_State.jpg", width = 6, height = 6)


#---- Step 3: Show the income per zip code ----------------------------------

## 3.1: Draw each zipcode on the map, where the color of the 'dot' is based on the median income.
#       To make the map look appealing, have the background of the map be black.
income.by.zipcode.df$state_name <- tolower(state.name[match(income.by.zipcode.df$state,state.abb)])
head(income.by.zipcode.df)

g.map.zip <- ggplot(data=income.by.zipcode.df, mapping=aes(map_id=state_name))
g.map.zip <- g.map.zip + geom_map(map=us.map, fill="black", color="white")
g.map.zip <- g.map.zip + expand_limits(x=us.map$long, y=us.map$lat)

g.map.zip <- g.map.zip + geom_point(data=income.by.zipcode.df, mapping=aes(x=income.by.zipcode.df$longitude, y=income.by.zipcode.df$latitude, color=income.by.zipcode.df$median))
g.map.zip <- g.map.zip + coord_map()
g.map.zip <- g.map.zip + ggtitle("Income per Zip Code") + theme(plot.title=element_text(hjust=0.5))
g.map.zip <- g.map.zip + guides(color=guide_legend(title="Median Income"))
g.map.zip <- g.map.zip + removeThemeAxis
g.map.zip
ggsave("U.S._Map_of_Median_Income_by_ZipCode.jpg", width = 6, height = 6)

#---- Step 4: Show Zip Code Density -----------------------------------------

## 4.1: Now generate a different map, one where we can easily see where there are lots of zip codes, 
#       and where there are few (using the 'stat_density2d' function)
g.map.zip.density <- ggplot(data=income.by.zipcode.df, mapping=aes(map_id=state_name))
g.map.zip.density <- g.map.zip.density + geom_map(map=us.map, fill="black", color="white")
g.map.zip.density <- g.map.zip.density + expand_limits(x=us.map$long, y=us.map$lat)

g.map.zip.density <- g.map.zip.density + stat_density_2d(data=income.by.zipcode.df, mapping=aes(x=income.by.zipcode.df$longitude, y=income.by.zipcode.df$latitude))
g.map.zip.density <- g.map.zip.density + coord_map()
g.map.zip.density <- g.map.zip.density + ggtitle("Zip Code Density") + theme(plot.title=element_text(hjust=0.5))
g.map.zip.density <- g.map.zip.density + removeThemeAxis
g.map.zip.density
ggsave("U.S._Map_of_ZipCode_Density.jpg", width = 6, height = 6)

#---- Step 5: Zoom in to the region around NYC ------------------------------

## 5.1: Repeat stes 3 & 4, but have the image / map be of the northeast U.S. (Centered around New York)
nyc <- geocode("New York, NY", source = "dsk")
zoom <- 2
#ggmap(get_map(nyc, zoom=4))

center_x <- nyc$lon
center_y <- nyc$lat

y_limit <- c(center_y-zoom, center_y+zoom)
x_limit <- c(center_x-zoom, center_x+zoom)

## 5.1.1: Draw each zipcode on the map, where the color of the 'dot' is based on the median income.
#       To make the map look appealing, have the background of the map be black.
g.map.nyc.zip.income <- g.map.zip + xlim(x_limit) + ylim(y_limit) + coord_map()
g.map.nyc.zip.income <- g.map.nyc.zip.income + geom_point(aes(x=center_x, y=center_y), color="darkred", size=3)
g.map.nyc.zip.income <- g.map.nyc.zip.income + ggtitle("Income by Zip around NYC") + theme(plot.title=element_text(hjust=0.5))
g.map.nyc.zip.income
ggsave("U.S._Map_of_ZipCode_NYC.jpg", width = 6, height = 6)


## 5.1.2: Now generate a different map, one where we can easily see where there are lots of zip codes, 
#       and where there are few (using the 'stat_density2d' function)
g.map.nyc.zip.density <- g.map.zip.density + xlim(x_limit) + ylim(y_limit) + coord_map()
g.map.nyc.zip.density <- g.map.nyc.zip.density + stat_density_2d(aes(x=center_x, y=center_y), color="darkred", size=3)
g.map.nyc.zip.density <- g.map.nyc.zip.density + ggtitle("Zip Code Density around NYC") + theme(plot.title=element_text(hjust=0.5))
g.map.nyc.zip.density

ggsave("U.S._Map_of_NYC_ZipCode_Density.jpg", width = 6, height = 6)

