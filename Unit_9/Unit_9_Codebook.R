##Dependencies
## This are the libraries that we will use in this projec
library(dplyr)
library(ggplot2)
library(tidyr)
library(reshape2)
library(tidyverse)

# Question A 1,2,3,4
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
# Cleaning the data
# changing from factor to char
brew$State <- as.character(brew$State)
# changing to int
brew$Brew_ID <- as.integer(brew$Brew_ID)
# changing to char
brew$Name <-  as.character(brew$Name)
# removing trailing whitespace with str_trim
str_trim(brew$State, side = "left") -> brew$State
head(brew$State)
#Combine the two dataset and more cleaning
beers$Brew_ID<-beers$Brewery_id
beers$Brewery_id<-NULL
head(beers)
head(brew)
combined<- merge(brew, beers,by = "Brew_ID")
# renaming Name.x to Brewery_Name
colnames(combined)[2] <- "Brewery_Name"
# renaming Name.y to Beer_Name
colnames(combined)[5] <- "Beer_Name"
head(combined)
# Create one new dataset for Colorado and Texas
na.omit(combined) %>% filter(State %in% c("TX","CO"))%>% arrange(IBU) ->beerCOTX
head(beerCOTX)

# Question B
# B1

# Plot 1
p1= ggplot(beerCOTX, aes(x=IBU, y=ABV) )
p2= p1+ geom_point() 
p3= p2 + facet_wrap(State ~ .)
p3
# Plot 2
g1= ggplot(beerCOTX, aes(x=ABV, y=IBU) )
g2= g1+ geom_point() 
g3= g2 + facet_wrap(State ~ .)
g3

# Question C
# C1 TX
na.omit(combined) %>% filter(State=="TX") ->beerTX
head(beerTX)
plot(beerTX$ABV ~ beerTX$IBU, main= "ABV Vs IBU in Texas", xlab = "IBU", ylab = "ABV")
tx_lmt <- lm(beerTX$ABV ~ beerTX$IBU)
summary(tx_lmt)
abline(tx_lmt)
hist(tx_lmt)
#C2 TX Addressing the assumption
hist(tx_lmt$resid)
plot(tx_lmt, which=1)
plot(tx_lmt, which=2)
# 1-Subpopulation of ABV for a fixed IBU are not normally distributed in the state of Texas. Could be corrected with the removal of outlier in the higher end though
# please refer to the histogram. The variation in the distribution of ABV is not the same, please refer to Residual Vs Fitted,
# different variation of the ABV across the plot.
# 2-The means of the distributions of ABV are linearly related to IBU.P value =  <2e-16 
# 3-We can not assume that the mean and variation of the mean from the ABV are normaly distributed.Please refer to residual vs Fitted
# 4-ABV and IBU are independent from one another.(No multicollinearity). Observations are independent of one another
# C1 CO
na.omit(combined) %>% filter(State=="CO") ->beerCO
head(beerCO)
plot(beerCO$ABV ~ beerCO$IBU, main= "ABV Vs IBU in Colorado", xlab = "IBU", ylab = "ABV")
co_lmc <- lm(beerCO$ABV ~ beerCO$IBU)
summary(co_lmc)
abline(co_lmc)
#C2 CO Addressing the assumption
hist(co_lmc$resid)
plot(co_lmc, which=1)
plot(co_lmc, which=2)
# 1-Subpopulation of ABV for a fixed IBU are not normally distributed in the state of Colorado, please refer to the histogram. 
# The variation in the distribution of ABV is not the same, please refer to Residual Vs Fitted, different variation of the ABV across the plot, and 
# the data is concentrated in the lower part of the plot Residual VS Fitted.
# 2-The means of the distributions of ABV are linearly related to IBU.P value =  <2e-16 
# 3-We can not assume that the mean and variation of the mean from the ABV are normaly distributed.Please refer to residual vs Fitted
# 4-ABV and IBU are independent from one another.(No multicollinearity). Observations are independent of one another
# D
# D1
summary(tx_lmt)
# Interpretation of the slope = For IBU in the state of Texas, for every increment in IBU, there is increasement in ABV by
# 4.172e-04*IBU.
# Interpretation of the intercept = When IBU is 0 in the state of Texas the ABV is 4.3473e-02
summary(co_lmc)
# nterpretation of the slope = For IBU in the state of Colorado, for every increment in IBU, 
# there is increasement in ABV by 3.676e-04*IBU.
# Interpretation of the intercept = When IBU is 0 in the state of Colorado the ABV is 4.740e-02
# There is evidence that the relationship of ABV and IBU is stronger in the state of Texas 4.172e-04 versus 3.676e-04  
# D2
confint(tx_lmt)
# For IBU in the state of Texas, for every increment in IBU, we will see increasement in the ABV by
# 4.172e-04*IBU. We are 95% confident that this increment in ABV will be between (0.000344007 - 0.0004903987)*UBU  
confint(co_lmc)
# For IBU in the state of Colorado, for every increment in IBU, we will see increasement in the ABV by 3.676e-04*UBU
# We are 95% confident that this increment in ABV will be between (0.000299726 - 0.0004354124)*UBU 
# In base of the plausible values of the intercept and the slope from Colorado and Texas, we can not say that Texas 
# and colorado have a different relationship.

# E 1,2,3
df1<- mutate(beerCOTX, IBU2= IBU^2)
head(df1)
## 60% and 40% of the sample size
smp_size <- floor(0.60 * nrow(df1))
## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(df1)), size = smp_size)
train <- df1[train_ind, ]
head(train)
test <- df1[-train_ind, ]
head(test)
dim(test)
dim(train)
# Training data
na.omit(train) %>% filter(State=="TX") -> dftt
head(dftt)
na.omit(train) %>% filter(State=="CO") -> dftc
head(dftc)
# Test data
na.omit(test) %>% filter(State=="TX") -> dftet
head(dftet)
na.omit(test) %>% filter(State=="CO") -> dftec
head(dftec)

# Model 1 Texas
texas <- lm(dftt$ABV ~dftt$IBU) # We predict ABV in base of number given by IBU
summary(texas)
# prediction 1
predict1 <- predict.lm(texas, dftet)
predict1
# calculate ASE HERE
ASE1 <- mean((predict1-dftet$ABV)^2)
ASE1
# Model 2 Texas
texas2 <- lm( dftt$ABV ~ dftt$IBU + dftt$IBU2 , data  = dftt)
summary(texas2)
predict2 <- predict.lm(texas2, dftet)
predict2
# Calculate ASE here
ASE2 <-mean((predict2-dftet$ABV)^2)
ASE2
# Model 1 Colorado
colorado <- lm(dftc$ABV ~dftc$IBU) # We predict ABV in base of number given by IBU
summary(colorado)
# prediction 1
predict3 <- predict.lm(colorado, dftec)
predict3
# calculate ASE HERE
ASE3<- mean((predict3-dftec$ABV)^2)
ASE3
# Model 2 Colorado
colorado2 <- lm( dftc$ABV ~ dftc$IBU + dftc$IBU2 , data  = dftc)
summary(colorado2)
predict4 <- predict.lm(colorado2, dftec)
predict4
# Calculate ASE here
ASE4<- mean((predict4-dftec$ABV)^2)
ASE4
# Given all the values of ASE, I conclude that none of the model are much better than each other. The model 1 for colorado
# seen to be the best by 0.00000088
