#Dependencies
library(dplyr)
library(tidyverse)

####Youb2016

url<-"https://raw.githubusercontent.com/BivinSadler/MSDS-6306-Doing-Data-Science/master/Unit%205/yob2016.txt"
data <- read.csv(url(url), sep = ";", header = F)
head(data)

###Youb2015

url1<-"https://raw.githubusercontent.com/BivinSadler/MSDS-6306-Doing-Data-Science/master/Unit%205/yob2015.txt"
data1 <- read.csv(url(url1), sep = ",", header = F)
head(data1)

###Question 1

#1A,1B
# Load the data above. Change the column name. Display the summary and the structure of the data
class(data)
colnames(data)<- c("Name", "Gender", "Number of Children")
head(data)
summary(data)
str(data)

#1C,1D
#Find the misspelled name, and erase it.The final data is saved as an object under finalData1
ms<-grep(".*yyy.*", data$Name)
ms
data<-data[-ms,]
data
finalData<- data
head(finalData)

###Question2

#2A,2B The file 2015 is loaded above.It is put in a dataframe. 
class(data1)
colnames(data1)<- c("Name", "Gender"," Number of Children") 
#2B The last 10 rows are all males, and small number, very unusual name to me.
tail(data1, 10)
#2C Mege the 2 datas by the name, change the callumn name and erase the Na
final<- merge(data, data1, by ="Name" )
head(final)
final[, "Gender.x"]<-NULL
names(final)<- c("Name", "Number of Children 2016", "Gender", "Number of Children 2015")
head(final)
is.na(final)
final<- na.omit(final)
head(final)
###Question 3

#3A
#Get the total number of name per year
final %>% mutate(Total = `Number of Children 2016` + `Number of Children 2015`) -> FinalFinal
head(FinalFinal)
class(FinalFinal)
#3B
#Sort it by top 10 names
FinalFinal %>% arrange(desc(Total))-> FinalFinal
head(FinalFinal)
#3C
#Filter the data only by female, eliminate male.
FinalFinal %>% filter(Gender == 'F') %>% arrange(desc(Total)) -> FinalFinal
head(FinalFinal)
#3D
#Getting the top ten females names, clean it and write in a csv file.
top_names<- top_n(FinalFinal, 10, Total)
top_names[,"Number of Children 2016" ]<-NULL
top_names[,"Number of Children 2015" ]<-NULL
top_names[,"Gender"]<-NULL
top_names
write.csv(top_names, file = "Top 10 names")
