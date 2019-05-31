install.packages("XML", repos = "https://cran.r-project.org/")
library(XML)
library(dplyr)
library(tidyr)
library(stringi)
library(rvest)
install.packages("xml2")
library(ggplot2)
install.packages("RCurl")
library(RCurl)


#2A/2B/C/D

stars<-read_html("http://www.espn.com/nhl/team/roster/_/name/dal/dallas-stars")
stars_table<-html_nodes(stars, "table")
stars_dfs<-html_table(stars_table, fill = TRUE)
Rost1 = stars_dfs[[3]]
Rost2 = stars_dfs[[6]]
Rost3 = stars_dfs[[9]]
Rost4 = stars_dfs[[12]]
Rost5 = stars_dfs[[15]]
Roster = rbind(Rost1,Rost2)
Roster = rbind(Roster,Rost3)
Roster = rbind(Roster, Rost4)
Roster = rbind(Roster, Rost5)
head(Roster)
class(Roster)
df1<-Roster[-1]
df1
df1<- separate(df1, Name, into = c("Name","Position"), sep = "(?<=[a-zA-Z])\\s*(?=[0-9])")
head(df1)
df1[,2]<-sapply(df1[,2], as.numeric)
head(df1)
df1$Age<-NULL
df1$HT<-NULL
df1$WT<-NULL
df1$Shot<-NULL
df1$Birth.Place<-NULL
df1$Birthdate<-NULL
head(df1)

#Live Session 4


install.packages("WDI")
## Install and load package
library(WDI)

## Search for fertilizer consumption data
WDIsearch("Data")

## Use indicator number to gather data
FertConsumpData <- WDI(indicator="AG.CON.FERT.ZS")
FertConsumpData 

MaleOFSD <- WDI(country = "US", indicator="UIS.ROFST.H.2.Q3.M", start = 2017, end = 2018)
MaleOFSD 

#Basic

data <-getURL("https://www.w3schools.com/xml/simple.xml")
doc <- xmlParse(data)
names <- xpathSApply(doc,"//name",xmlValue)
price <- xpathSApply(doc,"//price",xmlValue)
description <- xpathSApply(doc,"//description",xmlValue)
bfasts = data.frame(names,price,description)
bfasts
bfasts$description
which(grepl("toast",bfasts$description))
grepl("covered",bfasts$description)


hp<-read_html("https://www.w3schools.com/xml/simple.xml")
hp_priceR <- html_nodes(hp,"price")
hp_descR <- html_nodes(hp,"description")
hp_nameR
hp_name = stri_sub(hp_nameR,7,-8)
hp_name
hp_price = stri_sub(hp_priceR,8,-9)
hp_price
hp_desc = stri_sub(hp_descR,14,-15)
hp_desc
grep("toast", hp_desc)
grepl("toast",hp_desc)

#Breakout 1
data <-getURL("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml")
doc <- xmlParse(data)
names <- xpathSApply(doc,"//name",xmlValue)
zipcodes <- xpathSApply(doc,"//zipcode",xmlValue)
councildistrict <- xpathSApply(doc,"//councildistrict",xmlValue)
rests = data.frame(names,zipcodes,councildistrict)
dim(rests)
restsDTown = rests[which(rests$councildistrict == "11"),]
grep("Sushi",rests$names,ignore.case = T)

hp<-read_html("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml")
hp_name2 <- html_nodes(hp,"name")
hp_zipcode2 <- html_nodes(hp,"zipcode")
hp_councildistrict2 <- html_nodes(hp,"councildistrict")

#How many restaurants total 
restByDist = hist(as.numeric(councildistrict))
barplot(height = restByDist$counts, names = (as.character(seq(1,13,1))),xlab = "Council District",ylab = "Number of Restaurants")
barplot(height = restByDist$counts, names = (as.character(seq(1,13,1))),xlab = "Council District",ylab = "Number of Restaurants", horiz = TRUE)
