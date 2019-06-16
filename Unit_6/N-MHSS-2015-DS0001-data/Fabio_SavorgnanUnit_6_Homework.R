library(dplyr)
library(tidyverse)
library(ggplot2)

##Question 1

# 1A Load the file and inspect it
class(mh2015_puf)
df1<-mh2015_puf
head(df1)
dim(df1)
str(df1)

#1B
sa<- as.vector(state.abb)
sa

#1C Pull the VA hospital by State in a dtaframe
df1 %>% filter(FACILITYTYPE == "Veterans Administration medical center (VAMC) or other VA health care facility") -> dfva
head(dfva)
dfva %>% count(LST) ->dfva1
dfva1<-as.data.frame(dfva1)
head(dfva1)
names(dfva1)<- c( "States", "VA Hospital")
head(dfva1)
dfva1 <- lapply(dfva1, function(x) gsub('\\s+', '', x))
dfva1 <- as.data.frame(dfva1)
head(dfva1)
dfva2 <- dfva1 %>% filter(!States %in%  c('AK','HI','VI','GU','PR','AS'))
dfva2
str(dfva2)

#1D Plot the VA Hospitals in the US terrritory by States
dfva2$States <- as.character(dfva2$States)
class(dfva2$States) 
p1= ggplot(data= dfva2, aes(x=States, y= VA.Hospital, fill= States))
p2 = p1 + geom_bar(stat="identity")
p3 = p2 + theme(axis.text.x = element_text(size = 4, angle =45, hjust = 1, vjust = 1)) 
p4 = p3 + ggtitle("VA Hospital per States in the US") + labs(x= "States",  y= "Number of VA Hospitals")
p5 = p4 + guides(fill=FALSE)
p5

##Question2

# 2A, 2B Load the statesize dataset, evaluate the LST original column with the paste funtion in the dfva1 dataframe.
# we need to change the name of the column where the abreviation of the states is present in order to merge the dataset
# with the statesize dataset. There was a problem about spaces which we solved already in question 1 in the LST colums
# which is the States colums in the dfva2 column
url<-url<-"https://raw.githubusercontent.com/BivinSadler/MSDS-6306-Doing-Data-Science/master/Unit%206/statesize.csv"
dfss <- read.csv(url(url), sep = ",", header = TRUE)
head(dfss)
x<- paste(dfva2$States)
x

# 2C Merge the data dfss with the statesize dataset, in order to get the square mile per state and VA per square mile.
names(dfss)<- c( "StateName ", "SqMiles", "States",  "Region") 
head(dfss)
dfc<-merge(dfva2,dfss, by = "States")
dfc$VA.Hospital<-as.numeric(dfc$VA.Hospital)
dfc$SqMiles<-as.numeric(dfc$SqMiles)
dfc1<- mutate(dfc, VAperThousandSqMile= VA.Hospital/SqMiles*1000)
dfc1
#2D
install.packages("ggthemes")
library(ggthemes)
library(forcats)
p1= ggplot(data= dfc1, aes(x= reorder(States,-VAperThousandSqMile), y= VAperThousandSqMile, fill= Region))
p2 = p1 + geom_bar(stat="identity")
p3 = p2 + theme(axis.text.x = element_text(size = 4, angle =45, hjust = 1, vjust = 1)) 
p4 = p3 + ggtitle("VA Hospitalin the US by State and Region") + labs(x= "States",  y= "VA Hospitals per 1000 Square Mile")
p4

##Question2E
# The northeast has the highest amount of VA hospital per 1000 Square Mile. The west has the lowest amount of 
# VA hospitals. I will recomend in base of this dataset to build VA hopitals in the area that has the lowest 
# amount of VA hospital per 1000 Square Mile, which is in the  west region of the US. But before the final decision, 
# I would like have a data which show the relatioship between population per VA hospital in each region.


