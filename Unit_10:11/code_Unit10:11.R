install.packages("caret")
knn(train, test, cl, k = 1, l = 0, prob = FALSE, use.all = TRUE)
library(dplyr)
library(ggplot2)
library(tidyr)
library(reshape2)
library(tidyverse)
library(stringr)
library(caret)

url<-"https://raw.githubusercontent.com/BivinSadler/MSDS-6306-Doing-Data-Science/master/Unit%207/Beers.csv"
beers <- read.csv(url(url), sep = ",", header = TRUE)
head(beers) 
url<-"https://raw.githubusercontent.com/BivinSadler/MSDS-6306-Doing-Data-Science/master/Unit%207/Breweries.csv"
brew <- read.csv(url(url), sep = ",", header = TRUE)
head(brew) 

# changing from factor to char
brew$State <- as.character(brew$State)
# changing to int
brew$Brew_ID <- as.integer(brew$Brew_ID)
# changing to char
brew$Name <-  as.character(brew$Name)
# removing trailing whitespace with str_trim
str_trim(brew$State, side = "left") -> brew$State
# Cleaning to combine bataset
beers$Brew_ID<-beers$Brewery_id
beers$Brewery_id<-NULL

combined<- merge(brew, beers, by = "Brew_ID")
# renaming Name.x to Brewery_Name
colnames(combined)[2] <- "Brewery_Name"
# renaming Name.y to Beer_Name
colnames(combined)[5] <- "Beer_Name"
head(combined)

na.omit(combined) %>% filter(State %in% c("TX","CO"))%>% arrange(IBU) ->beerCOTX
head(beerCOTX)


na.omit(combined) %>% filter(State %in% c("TX"))%>% arrange(IBU) ->beerTX
head(beerTX)
## 60% and 40% of the sample size
smp_size <- floor(0.60 * nrow(beerTX))
## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(beerTX)), size = smp_size)
trainTX <- beerTX[train_ind, ]
head(trainTX)
testTX <- beerTX[-train_ind, ]
head(testTX)
trainTX$ABV<- as.matrix(trainTX$ABV)
fit<- knnreg(x = trainTX$ABV, y = trainTX$IBU, K = 3)
fit

texas <- lm(trainTX$ABV ~trainTX$IBU) 
summary(texas)
# prediction 1
predictc <- predict.lm(texas, testTX$ABV)
head(predictc)
ASAc <- mean((predictc-dftet$ABV)^2)
ASAc


test<-c(150, 170, 190)
fit<- knnreg(x = trainTX$ABV, y = trainTX$IBU, K = 3)
fit
predictTXn <- predict(fit, test)
predictTXn
predictn <- predict(fit, test)
predictn

na.omit(combined) %>% filter(State %in% "TX" & Style %in% c("American IPA", "American Pale Ale")) %>% arrange(IBU) -> beerSTX
na.omit(combined) %>% filter(State %in% c("TX"))%>% arrange(IBU) ->beerSTX
beerSTX  %>% filter(Style%in% c("American IPA", "American Pale Ale"))%>% arrange(IBU)-> beerSTX 
beerSTX
smp_size <- floor(0.60 * nrow(beerSTX))
## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(beerSTX)), size = smp_size)
trainSTX <- beerSTX[train_ind, ]
head(trainSTX)
testSTX <- beerSTX[-train_ind, ]
head(testSTX)


trainSTX[,9]<- as.numeric(trainSTX[,9])
results = class::knn(trainSTX[,c(7,8)], testSTX[,c(7,8)], trainSTX$Style, k = 3)
testSTX$Stylepred = results
conf<- table(testSTX$Style, testSTX$Stylepred)
confusionMatrix(conf)