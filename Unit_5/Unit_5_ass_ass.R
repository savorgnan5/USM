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
