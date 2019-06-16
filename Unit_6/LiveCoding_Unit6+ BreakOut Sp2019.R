#requires packages "dplyr" and "ggplot2"
#install.package("dplyr")
#install.package("ggplot2"")

library(dplyr)
library(ggplot2)
library(tidyr)

#UNIT 3 Live Session Code!

#load first day dataset

#fileLocation <- "https://raw.githubusercontent.com/BivinSadler/MSDS-6306-Doing-Data-Science/master/Unit%206/nyt1.csv"
fileLocation <- "http://stat.columbia.edu/~rachel/datasets/nyt2.csv"

nytData <- read.csv(url(fileLocation))
head(nytData)

#stratified Age groups
str(nytData)
nytData$ageGroup <- cut(nytData$Age, c(-Inf, 18, 24, 34, 44, 54, 64, Inf))
levels(nytData$ageGroup) <- c("<18", "18-24", "25-34", "35-44", "45-54", "55-64", "65+")
head(nytData)
str(nytData)

#Transform gender to a factor variable
nytData$Gender = factor(nytData$Gender, levels=c(1,0), labels = c("male", "female"))
head(nytData)

#plot ageGroup and impressions
ggplot(data=nytData, aes(x=ageGroup, y=Impressions, fill=ageGroup)) + geom_bar(stat="identity") + theme_bw()
# see what geom_bar is doing!
nytData %>% group_by(ageGroup) %>% dplyr::summarize(Impressions = sum(Impressions))

#plot ageGroup and impressions for those signed in with at least 1 impression
nytData %>% filter(Impressions>0 & Signed_In == 1) %>% ggplot(aes(x=ageGroup, y=Impressions, fill=ageGroup)) + geom_bar(stat="identity") + theme_bw()
# see what geom_bar is doing!
nytData %>% filter(Impressions>0 & Signed_In == 1) %>% group_by(ageGroup) %>% dplyr::summarize(Impressions = sum(Impressions))


#(CTR = clicks/impressions) ; no CTR if there are no impressions
#conclusion <18 and 65+ have highest CTR

nytData %>% filter(Impressions>0 & Signed_In == 1) %>% 
  group_by(ageGroup) %>% 
  summarise(Impressions = sum(Impressions), Clicks = sum(Clicks)) %>% 
  ggplot(aes(x=ageGroup, y=Clicks/Impressions, fill=ageGroup)) + geom_bar(stat="identity") + theme_bw()

#Break Out 2 Starting Over

#load first day dataset
fileLocation <- "https://raw.githubusercontent.com/BivinSadler/MSDS-6306-Doing-Data-Science/master/Unit%206/nyt1.csv"
nytData <- read.csv(url(fileLocation))
head(nytData)

#make Click Through Rate (CTR)
#base R
nytData$CTR = nytData$Clicks/nytData$Impressions
#dplyr
nytData = nytData %>% mutate(CTR = Clicks / Impressions)
head(nytData)


#stratified Age groups
str(nytData)
nytData$ageGroup <- cut(nytData$Age, c(-Inf, 18, 24, 34, 44, 54, 64, Inf))
levels(nytData$ageGroup) <- c("<18", "18-24", "25-34", "35-44", "45-54", "55-64", "65+")
head(nytData)
str(nytData)

#Transform gender to a factor variable
nytData$Gender = factor(nytData$Gender, levels=c(1,0), labels = c("male", "female"))
head(nytData)

#histograms of Gender and CTR
nytData %>% filter(CTR > 0) %>% filter(Signed_In == 1) %>% ggplot(aes(CTR)) + facet_wrap(~Gender) + geom_histogram()

#boxplots for CTR
nytData %>% filter(CTR > 0) %>% filter(Signed_In == 1) %>% ggplot(aes(CTR)) + geom_boxplot(aes(Gender, CTR))

#Both distibutions of CTR looked right skewed although the sample size should be sufficient to invoke the CLT.
#There is not strong visual evidence against equal standard deviation 
# We will assume independence

# T-Test for CTR v. Gender
t.test(CTR~Gender, data = filter(nytData, CTR > 0 & Signed_In == 1), var.equal= TRUE)



#Break Out 3

#histograms of ageGroup and CTR
nytData %>% filter(CTR > 0) %>% filter(Signed_In==1) %>% ggplot(aes(CTR)) + facet_wrap(~ageGroup) + geom_histogram(bins = 30)

#boxplots for CTR over ageGroups
nytData %>% filter(CTR > 0) %>% filter(Signed_In==1) %>% ggplot(aes(CTR)) + geom_boxplot(aes(ageGroup, CTR))


#All distibutions over all agegroups CTR looked right skewed although the sample size should be sufficient to invoke the CLT.
#There is not strong visual evidence against equal standard deviation 
# We will assume independence

#Conduct the ANOVA
fit = aov(CTR~ageGroup, data = filter(nytData,Signed_In == 1 & CTR > 0))
summary(fit)

# Conclusion: 

#Break out 4
install.packages("mvtnorm")
install.packages("multcomp")
library(multcomp)
gfit = glht(fit,linfct = mcp(ageGroup = "Tukey"))
summary(gfit, test = adjusted(type = "bonferroni"))
confint(gfit)
