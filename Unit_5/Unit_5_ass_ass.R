##Assignment Unit 5

data <- read.table("schools_complete.csv", sep = "," ,header = TRUE)

dim(data)

colnames(data)

str(data)

install.packages("downloader")

library(downloader)

download("https://raw.githubusercontent.com/thoughtfulbloke/faoexample/master/appleorange.csv", destfile = "appleorange.csv")
download("https://raw.githubusercontent.com/thoughtfulbloke/faoexample/master/stability.csv", destfile = "stability.csv")
ao<-read.csv("appleorange.csv",  header = FALSE)
head(ao)
str(ao)
tail(ao)
names(ao)<- c("country", "countrynumber", "products", "productsnumber", "tonnes", "year")
head(ao, 10)
ao$countrynumber<- as.integer(ao$countrynumber)
str(ao)
fslines<-which(ao$country == "Food supply quantity (tonnes) (tonnes)")
ao<-ao[(-1 * fslines),]
str(ao)
ao$year<-2009
ao$tonnes<-gsub("\xca", "", ao$tonnes)
ao$tonnes<- gsub(", tonnes\\(\\)", "", ao$tonnes)
ao$tonnes<- as.numeric(ao$tonnes)
str(ao)
apple<- ao[ao$productsnumber == 2617, c(1,2,5)]
str(apple)
names(apple)[3]<-"apple"
orange<- ao[ao$productsnumber == 2611, c(2,5)]
str(orange)
names(orange)[2]<- "oranges"
str(orange)
orange
completeData<- merge(apple, orange, by = "countrynumber", all = TRUE)
head(completeData)
str(completeData)
library(reshape2)
cleanData<- dcast(ao[,c(1:3,5)], formula = country + countrynumber ~ products, value.var = "tonnes")

library(dplyr)
library(tidyverse)
####Practice
##1
df = data.frame(Person = c("MaleBivin", "MaleFred", "MaleEd", "FemaleSally", "FemaleNance","FemaleMary"), Age = c(21,24,45,27,48,65) )
F = df[grep("^Female",df$Person),]
M = df[grep("^Male",df$Person),]
F$Person = gsub("Female","Female.",F$Person)
M$Person = gsub("Male","Male.",M$Person)
F = separate(F,"Person",into = c("Gender","Name"),sep = "\\.")
M = separate(M,"Person",into = c("Gender","Name"),sep = "\\.")
df3 = rbind(M,F)
df3 = arrange(df3,Age)
df2 = data.frame(Name = c("Bivin", "Fred", "Ed", "Sally", "Nance","Mary"), Pet = c("Dog","Dog","Fish","Dog","None","Cat") )
merge(df3,df2, by = "Name")
##2
strings <- c("a", "ab", "acb", "accb", "acccb", "accccb")
grep("ac{2,}b", strings, value = TRUE)   #what will this return ?
grep("ac{2,3}b", strings, value = TRUE)  #what will this return ?

strings <- c("abcd", "cdab", "cabd", "c abd", "cabdd")
grep("(^c)d?", strings, value = TRUE)  # what will this return?
grep("(^c)d+", strings, value = TRUE)  # what will this return?
strings <- c("^ab", "ab", "abc", "abd", "abe", "ab 12")
grep("\\^ab", strings, value = TRUE)          ## [1] "^ab"
grep("abc|abd", strings, value = TRUE)  # what will this return?

##3
NamesDF = data.frame(Name = c("Flash_Gordon", "Babe_Ruth", "John_Smith", "Susan_Anthony", "Michael_Jordan"))
NamesDF = separate(NamesDF,"Name", into = c("First","Last"), sep = "_")
NamesDF

NamesDF$GenderAge <- c("M19", "M40", "M32", "F140", "M55")
#or
NamesDF = mutate(NamesDF,GenderAge=  c("M19", "M40", "M32", "F140", "M55"))

NamesDF$GenderAge = gsub("M","M_",NamesDF$GenderAge)
NamesDF$GenderAge = gsub("F","F_",NamesDF$GenderAge)

NamesDF = separate(NamesDF,"GenderAge",into = c("Gender","Age"), sep = "_")
NamesDF
##4
library(nycflights13)
df1 = data.frame(Student_ID = c("1234", "2345", "8910", "9101", "3456", "5678","8888"), Course = c("Time Series", "NLP", "Stats1", "DDS", "DDS", "ML2","Data Mining"))
df2 = data.frame(Student_ID = c("1234", "2345", "8910", "9101", "3456", "5678","99999", "11111"), Gender = c("M", "F", "M", "F", "F", "F", "M", "M"), State = c("TX", "TX", "CA", "ID", "NY", "FL","NM", "AZ") )

#Different Column Names: by.x and by.y
df3 = data.frame(Student_ID_Number = c("1234", "2345", "8910", "9101", "3456", "5678", "8888"), Course = c("Time Series", "NLP", "Stats1", "DDS", "DDS", "ML2", "Data Mining"))
df4 = data.frame(Student_ID = c("1234", "2345", "8910", "9101", "3456", "5678","99999", "11111"), Gender = c("M", "F", "M", "F", "F", "F", "M", "M"), State = c("TX", "TX", "CA", "ID", "NY", "FL","NM", "AZ") )
merge(df1,df2, by = "Student_ID")
inner_join(df1,df2,by = "Student_ID")
df1 %>% inner_join(df2,by = "Student_ID")
merge(df3,df4, by.x = "Student_ID_Number", by.y = "Student_ID")


merge(df1,df2, by = "Student_ID",all = TRUE)
full_join(df1,df2,by = "Student_ID")
df1 %>% full_join(df2,by = "Student_ID")
merge(df3,df4, by.x = "Student_ID_Number", by.y = "Student_ID", all = TRUE)


merge(df1,df2, by = "Student_ID",all.x = TRUE)
left_join(df1,df2, by = "Student_ID")
merge(df3,df4, by.x = "Student_ID_Number", by.y = "Student_ID", all.x = TRUE)


WholeDF = merge(df1,df2, by = "Student_ID",all.y = TRUE)
right_join(df1,df2, by = "Student_ID")
merge(df3,df4, by.x = "Student_ID_Number", by.y = "Student_ID", all.y = TRUE)
