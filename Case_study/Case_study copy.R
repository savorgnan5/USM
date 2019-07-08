##Dependencies
## This are the libraries that we will use in this projec
library(dplyr)
library(ggplot2)
library(tidyr)
library(reshape2)
library(tidyverse)


## We are loading the dataset from the beers and the dataset from breweries in the chunk code below.
##The beers dataset is bears, the breweries dataset is brew.

#Beers.csv:

#Name: Name of the beer.
#Beer_ID: Unique identifier of the beer.
#ABV: Alcohol by volume of the beer.
#IBU: International Bitterness Units of the beer.
#Brewery_ID: Brewery id associated with the beer.
#Style: Style of the beer.
#Ounces: Ounces of beer.

#Breweries.csv:

#Brew_ID: Unique identifier of the brewery.
#Name: Name of the brewery.
#City: City where the brewery is located.
#State: U.S. State where the brewery is located.

url<-"https://raw.githubusercontent.com/BivinSadler/MSDS-6306-Doing-Data-Science/master/Unit%207/Beers.csv"
beers <- read.csv(url(url), sep = ",", header = TRUE)
head(beers) 

url<-"https://raw.githubusercontent.com/BivinSadler/MSDS-6306-Doing-Data-Science/master/Unit%207/Breweries.csv"
brew <- read.csv(url(url), sep = ",", header = TRUE)
head(brew) 

## QUESTION 1: BREWERIES PER STATE

# In the following code we count how many breweries are pers state in the US territory. NumBreweries describe the 
# amount of breweries per state in the US territory.Then we ordered in descending order the nunber of breweries.
# Colorado and California are the states with the highest amount of breweries in their state. 
#The states with only one brewere are DC, ND, SD, WD. We used the pipe function from the dplyr library to generate the code

# changing from factor to char
brew$State <- as.character(brew$State)
# changing to int
brew$Brew_ID <- as.integer(brew$Brew_ID)
# changing to char
brew$Name <-  as.character(brew$Name)

# removing trailing whitespace with str_trim
str_trim(brew$State, side = "left") -> brew$State
brew %>% group_by(State) %>% summarize(numBreweries = n_distinct(Name)) -> numBreweries
numBreweries
numBreweries <- arrange(numBreweries, desc(numBreweries))
numBreweries
tail(numBreweries)

# Creating chart for breweries in the US territory by States using ggplot.
# Version 1
p1= numBreweries %>% ggplot(aes(x = reorder(State,-numBreweries),  y = numBreweries, fill = State)) 
p2= p1+ geom_col() + labs(title = "Breweries Per State", x= "State", y= "Number of Breweries") 
p3= p2+ theme( axis.text = element_text(size = rel(0.4),angle =45, hjust = 1, vjust = 1) ) 
p3

# Version 2
g1= numBreweries %>% ggplot(aes(x = reorder(State,-numBreweries),  y = numBreweries, fill = State)) 
g2= g1+ geom_col() 
g3= g2+coord_flip()
g4= g3+theme(legend.position='none')
g5= g4+theme_classic()+labs(x="State", y="Nunber of Breweries", legend="Nunber of Breweries")+ ggtitle("Breweries Per State")
g6= g5+theme(plot.title = element_text(hjust = 0.5),axis.text=element_text(size=5)) 
g6
## QUESTION 2: MERGED DATA SET

# In the following code chunk we merge beer data with the breweries data. We print the first 6 observations and 
# the last six observations to check the merged file in order to explore the dataset. Combined is the combined dataset.

beers$Brew_ID<-beers$Brewery_id
beers$Brewery_id<-NULL
head(beers)
head(brew)
combined<- merge(brew, beers,by = "Brew_ID")

# renaming Name.x to Brewery_Name
colnames(combined)[2] <- "Brewery_Name"
# renaming Name.y to Beer_Name
colnames(combined)[5] <- "Beer_Name"

head(combined, 6)
tail(combined, 6)


## QUESTION 3: MISSING DATA

# In the following chunk code we report the number of NA's in each column of the combined dataset. NA_count is the amount 
# of NA in each column in the combined dataset. There are 62 missing value in ABV(Alcohol content) and 1005 in IBU(bitterness)

na_count <-sapply(combined, function(y) sum(length(which(is.na(y)))))
na_count


## QUESTION 4: MEDIAN ALCOHOL CONTENT

# We compute the median alcohol content and international bitterness unit in a beer for each state from the combined dataset.
# in order to compute the alcohol content and international bitterness unit in a beer for each state from the combined dataset
# We grouped the combined dataset by State, then we sumarized by alcohol content and international bitterness unit per beer using
# the median function. We decided to get rid of the NA values, we used the pipe function from the dplyr library to design this code.
# The final product of the function is plot1. 
## We Plot a bar chart to compare the median alcohol content and international bitterness unit per beer for each state 
# from the combined dataset. We used the ggplot library to perform the plot.

combined %>% group_by(State) %>% summarize(Alcohol_content = median(ABV, na.rm = TRUE), Bitterness= median(IBU, na.rm = TRUE)) ->plot1
plot1

# Version 1
ggplot(data=plot1, aes(x= reorder(State,-Alcohol_content), y=Alcohol_content, fill= Bitterness)) + geom_bar(stat="identity") + ggtitle("Bitterness per Alcohol Content") + labs(x= "States",  y= "Alcohol Content") + theme(axis.text.x = element_text(size =4 , angle =45, hjust = 1, vjust = 1))

# Version 2
g1= ggplot(plot1, aes(x=reorder(State,-Alcohol_content),y=Alcohol_content, fill=Bitterness))   
g2=  g1 + geom_bar(stat="identity") 
g3= g2+coord_flip(ylim=c(.04,.0650))
g4= g3+theme(legend.position='none')
g5= g4+ scale_fill_gradient2(midpoint=median(plot1$Alcohol_content),low='red', mid='snow3', high='black', space='Lab')
g6= g5+theme_classic()+labs(x="State", y="ABV", legend="ABV")+ ggtitle("Median ABV by state")
g7= g6+theme(plot.title = element_text(hjust = 0.5),axis.text=element_text(size=5)) 
g7
# The top 5 state with more alcohol content, which is HAC_S. The top 5 state with the most bitter beer, which is HB_S
# Furtheremore, we proceed to plot HAC_S and HB_S. Previously we clean the data in order no to have repeated the same state
# with the function distint.
df_unique <-distinct(combined[order(combined$ABV, decreasing = T, na.last = T),], State, .keep_all = TRUE)
HAC_S <-head(df_unique,5)
ggplot(data=HAC_S, aes(x= reorder(State,-ABV), y=ABV, fill= IBU)) + geom_bar(stat="identity") + ggtitle("Top State per Alcohol Content") + labs(x= "States",  y= "Alcohol Content") + theme(axis.text.x = element_text(size =8, angle =45, hjust = 1, vjust = 1))

HB_S<-head(combined[order(combined$IBU, decreasing = T, na.last = T),],5)
HB_S
ggplot(data=HB_S, aes(x= reorder(State,-IBU), y=IBU, fill= ABV)) + geom_bar(stat="identity") + ggtitle("Top State per Beer Bitterness") + labs(x= "States",  y= "Beer Bitterness") + theme(axis.text.x = element_text(size =8, angle =45, hjust = 1, vjust = 1)) 

# The bottom 5 lowest state with alcohol content, which is LAC_S. The bottom 5 lowest state with bitterness per beer, which is LB_S
# Furtheremore, we proceed to plot HAC_S and HB_S. Previously we clean the data in order no to have repeated the same state
# with the function distint.
LAC_S<-tail(combined[order(combined$ABV, decreasing = T, na.last = F),],5)
LAC_S
ggplot(data=LAC_S, aes(x= reorder(State,-ABV), y=ABV, fill= IBU)) + geom_bar(stat="identity") + ggtitle("Lowest Alcohol Content per State") + labs(x= "States",  y= "Alcohol Content") + theme(axis.text.x = element_text(size =8, angle =45, hjust = 1, vjust = 1)) 


df_unique <-distinct(combined[order(combined$IBU, decreasing = T, na.last = F),], State, .keep_all = TRUE)
LB_S <-tail(na.omit(df_unique),5)
LB_S
ggplot(data=LB_S, aes(x= reorder(State,-IBU), y=IBU, fill= ABV)) + geom_bar(stat="identity") + ggtitle("Lowest Beer Bitterness per State") + labs(x= "States",  y= "Beer Bitterness") + theme(axis.text.x = element_text(size =8, angle =45, hjust = 1, vjust = 1)) 

                  
## QUESTION 5: MAX IBU & ABV STATES

# The following code show the state with the maximum alcoholic (ABV) beer, and the state with 
# the most bitter (IBU) beer.The maximum alcoholic (ABV) beer, and bitter (IBU) beer is represented by maxal.


##Max ABV and IBU and the state
 maxal<- sapply(plot1, max, na.rm = TRUE)
 maxal
 
# Highest Alcohol Content in a beer is HAC, the highest alcohol content in a beer in its state is HAC_S
HAC<- head(beers[order(beers$ABV, decreasing = T, na.last = T),],1)
HAC
HAC_S<-head(combined[order(combined$ABV, decreasing = T, na.last = T),],1)
HAC_S
# Highest Bitterness in a beer is HB, the highest bitterness in a beer in its state is HB_S
HB<-head(beers[order(beers$IBU, decreasing = T, na.last = T),],1)
HB
HB_S<-head(combined[order(combined$IBU, decreasing = T, na.last = T),],1)
HB_S

## QUESTION 6: SUMMARY STATS FOR ABV

# Summary statistics for the ABV variable from the beers dataset. Median 0.056, mean 0.067. We also show the boxplot of 
# this summary
# Summary statistics for the IBU variable from the beers dataset. Median 35, mean 42.71. We also show the boxplot of
# this summary
summary(beers$ABV)
boxplot(beers$ABV, main=toupper("Alcohol Content in Beers"), font.main=3, cex.main=1.2, xlab="Beers", ylab="Alcohol Content", font.lab=3, col="darkgreen")
summary(beers$IBU)
boxplot(beers$IBU, main=toupper("Bitterness in Beers"), font.main=3, cex.main=1.2, xlab="Beers", ylab="Bitterness", font.lab=3, col="darkgreen")

## QUESTION 7: ABV & IBU RELATIONSHIP


# Scatterplot between Alcohol & Bitterness. It look like bitterness of 42 and alcohol content of 0.06 would be a good 
# combination in a particular beer type of production.
grid(plot(combined$ABV, combined$IBU, main="Scatterplot Between Alcohol Content & Bitterness", xlab = "Alcohol Content", ylab = "Alcohol Bitterness"))

# Tests R correlation between ABV & IBU ignoring NA entries. Cor 0.67, 
#this is a weak correlation between the ABV and IBU
cor.test(combined$ABV, combined$IBU, na.action(na.omit("NA")))

# Predicting bitterness based on alcohol content.The adjusted R-squared:0.4493. There is poor correlation in this case
# between the bitterness and alcohol content.
beer.lm <- lm(combined$IBU ~ combined$ABV, na.action(na.omit("NA")))
summary(beer.lm)

                  
# In the following code using ggplot we draw a scatter plot between the bitterness and alcohol content.
# It look like there is a trend toward a llinear relatioship between the 2 mentioned variable, but it is poor.
ggplot(dat= plot1, aes(x= Alcohol_content, y= Bitterness)) + geom_point(shape=1) + geom_smooth(method = "lm") + ggtitle("Bitterness per Alcohol Content")  

# In the following code we found the most frequent beers style and beer name in the US territory.BeersStyle is the most
# frequent beer style in the US territory. BeersName is the most frequent beers name in the US territory.
beers %>% count(Style) %>% arrange(desc(n)) -> BeersStyle
BeersStyle
beers %>% count(Name) %>% arrange(desc(n)) -> BeersName
BeersName


## ADDITIONAL QUESTION USING CENSUS DATA
# 2018 Census Data on https://www.census.gov 

read.csv(file = "data/population.csv", header = T) -> state_population
state_population$State <- as.character(state_population$State)

# Merging census data into brewery by state variable, the merged data is combined_pop. We merged this data in order
# to have the population per state and perform the calculation below.
# We compared the number of breweries with the state population. In order to find if the two are related.
# We found poor or no relation betwenn the 2.
merge(numBreweries, state_population, by = "State") -> combined_pop

# Tests R correlation between Breweries & Total Population By State. Corr 0.6210315, poor correlation.
cor(combined_pop$Population, combined_pop$numBreweries)


# Predicting number of breweries based on population. The population number poorly predict the number of breweries
# Adjusted R-squared: 0.3731 
brew_pop.lm <- lm(combined_pop$numBreweries ~ combined_pop$Population)
summary(brew_pop.lm)

# Scatterplot Between Breweries & Total Population By State
grid(plot(combined_pop$Population, combined_pop$numBreweries,main="Relationship of Breweries & Total Population", xlab = "Population by State", ylab = "Number of Breweries"))
abline(brew_pop.lm, col = "red")

#Examining correlation between Number of Breweries and Median Alcohol Content.The dataset albrew have the variable 
# number of breweries from the dataset numBreweries created previously and the mediam alcohol content per bear from 
# the dataset plot1 created before. So we combined the plot1 dataset and the numBreweries dataset. Furthermore, we 
# proceeded to plot the relationship between the number of breweries and the median alcohol content. There is a poor 
# correlation between median alcohol content and the population.
numBreweries<-as.data.frame(numBreweries)
plot1<-as.data.frame(plot1)
albrew<- merge(plot1,numBreweries, by= "State")
head(albrew)
grid(plot(albrew$numBreweries, albrew$Alcohol_content, main="Relationship Median Alcohol and Breweries", xlab = "Number of Breweries", ylab = "Median Alcohol Content"))
abline(brew_pop.lm, col = "red")

# Examining correlation between population and median alcohol content.The dataset albrew have the variable 
# numbber of breweries from the dataset numBreweries created previously and the mediam alcohol content per bear from 
# the dataset plot1 created before. So we combined the albrew with the state_population datatset from the census bureau
#in order to have the population variable added to the albrew dataset. The combined dataset of the albrew and satate_population
# is the alpop dataset. Furthermore, we proceeded to plot the relationship between the population per state and the median
# alcohol content. There is a poor correlation between median alcohol content and the population.
merge(albrew, state_population, by = "State") ->alpop 
head(alpop)
grid(plot(alpop$Population, alpop$Alcohol_content, main="Relationship Median Alcohol and Population", xlab = "Population", ylab = "Median Alcohol Content"))
abline(brew_pop.lm, col = "red")

# outputting file
# write.csv(combined_pop, file = "casestudy/combined_pop.csv", row.names = F)

