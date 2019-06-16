##Dependencies
## This are the libraries that we will use in this projec
library(dplyr)
library(ggplot2)
library(tidyr)


## We are loading the dataset from the beers and the dataset from breweries in the chunk code below.
##The beers dataset is bears, the breweries dataset is brew.

url<-"https://raw.githubusercontent.com/BivinSadler/MSDS-6306-Doing-Data-Science/master/Unit%207/Beers.csv"
beers <- read.csv(url(url), sep = ",", header = TRUE)
head(beers) 

url<-"https://raw.githubusercontent.com/BivinSadler/MSDS-6306-Doing-Data-Science/master/Unit%207/Breweries.csv"
brew <- read.csv(url(url), sep = ",", header = TRUE)
head(brew) 
               
## In the following code we count how many breweries are pers state in the US territory.NumBreweries describe the 
## amount of breweries per state in the US territory.Then we ordered in descending order. Colorado and California
## are the states with the highest amount of breweries in their state.

brew$State<- as.character(brew$State)
brew$Brew_ID<- as.integer(brew$Brew_ID)
brew$Name<-  as.character(brew$Name)
brew %>% group_by(State) %>% summarize(numBreweries = n_distinct(Name)) -> numBreweries
numBreweries
numBreweries$numBreweries<- as.integer(numBreweries$numBreweries) 
numBreweries <- arrange(numBreweries, desc(numBreweries))
numBreweries

##. In the following code chunk we merge beer data with the breweries data. We print the first 6 observations and 
## the last six observations to check the merged file in order to explore the dataset. Combined is the combined dataset.

beers$Brew_ID<-beers$Brewery_id
beers$Brewery_id<-NULL
head(beers)
head(brew)
combined<- merge(brew, beers,by = "Brew_ID")
head(combined, 6)
tail(combined, 6)

## In the following chunk code we report the number of NA's in each column of the combined dataset. NA_count is the amount 
## of NA in each column in the combined dataset.

na_count <-sapply(combined, function(y) sum(length(which(is.na(y)))))
na_count

## We compute the median alcohol content and international bitterness unit in a beer for each state from the combined dataset.
## in order to compute the alcohol content and international bitterness unit in a beer for each state from the combined dataset
## We grouped the combined dataset by State, then we sumarized by alcohol content and international bitterness unit per beer using
## the median function. We decided to get rid of the NA values, we used the pipe function from the dplyr library to design this code.
## The final product of the function is plot1. 
## We Plot a bar chart to compare the median alcohol content and international bitterness unit per beer for each state 
## from the combined dataset. We used the ggplot library to perform the plot.

combined %>% group_by(State) %>% summarize(Alcohol_content = median(ABV, na.rm = TRUE),
                                          Bitterness= median(IBU, na.rm = TRUE)) ->plot1
plot1
p1= ggplot(data=plot1, aes(x= State, y=Alcohol_content, fill= Bitterness)) 
p2= p1 + geom_bar(stat="identity")
p3= p2 + ggtitle("Bitterness per Alcohol Content") + labs(x= "States",  y= "Alcohol Content")
p4= p3 + theme(axis.text.x = element_text(size =4 , angle =45, hjust = 1, vjust = 1)) 
p4 


## The following code show the state with the maximum alcoholic (ABV) beer, and the state with 
## the most bitter (IBU) beer.The maximum alcoholic (ABV) beer, and bitter (IBU) beer is represented by maxal.

maxal<- sapply(plot1, max, na.rm = TRUE)
maxal

##Summary statistics for the ABV variable from the beers dataset.

summary(beers$ABV)

## In the following code using ggplot we draw a scatter plot between the bitterness and alcohol content.
## It look like there is a trend toward a llinear relatioship between the 2 mentioned variable, p3 is the plot.
p1= ggplot(dat= plot1, aes(x= Alcohol_content, y= Bitterness))
p2= p1 +geom_point(shape=1) + geom_smooth(method = "lm") 
p3= p2 + ggtitle("Bitterness per Alcohol Content")  
p3