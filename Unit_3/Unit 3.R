###Unit_3
##Question 1 It is cloned in local drive,"Desktop/USM/Unit_3/questio_1/awesome-public-datasets/Datasets/titanic.csv"
##Question2
#A
tdf = read.csv("Desktop/USM/Unit_3/questio_1/awesome-public-datasets/Datasets/titanic.csv", header = TRUE, sep = ",")
str(tdf)
colnames(tdf)
#B
plot(tdf$Sex, xlab= "Female", ylab= "Male", main="Sex Distribution in the Titanic" )
#C
#Without the apply function
meanAge<-mean(tdf$Age, na.rm=TRUE)
meanAge
meanFare<-mean(tdf$Fare, na.rm=TRUE)
meanFare
# With the apply function
asf <- tdf[c("Age","Survived","Fare")]
asf <- na.omit(asf)
head(asf)
meanVar<-lapply(asf, mean)
meanVar
##Question 3
sdf = read.csv("Desktop/USM/Unit_3/sleep_data_01.csv", header=TRUE, sep=",")
head(sdf)
SleepDf  <- function(df,x,y,z){
  median <- median(na.omit(df[,x]))
  min  <- min(na.omit(df[,y]))
  max  <- max(na.omit(df[,y]))
  ranged<-range(na.omit(df[,y]))
  mean <-mean(na.omit(df[,z]))
  std  <-sd(na.omit(df[,z] ))
  report  <- data.frame(median, min, max, ranged, mean, std)
  colnames(report)<-c("MedianAge","MinDuration","MaxDuration","DurationRange", "SelfEsteem","SE_SD")
  return(round(report, digits = 2))
}
SleepDf(sdf,"Age", "Duration", "RSES")
##Question 4
#A
install.packages("fivethirtyeight")
#B
library(fivethirtyeight)
data(package = "fivethirtyeight")
vignette("fivethirtyeight", package = "fivethirtyeight")
data(college_recent_grads)
head(college_recent_grads)
dfc<-data.frame(college_recent_grads)
head(dfc)
#B
View(college_recent_grads)
#C
dim(dfc)
##Question 5
#A
colnames(dfc)
length(colnames(dfc))
#B
unique(dfc$major_category)
MajorCount<- aggregate(data.frame(count = dfc$major_category), list(value = dfc$major_category), length)
MajorCount
#C
par(las=2) 
barplot(height = MajorCount$count, width = 1, space = NULL, names.arg = MajorCount$value,
        main = "Major Category for Recent Colleges Graduates", xlab = "Value", legend.text = "Categoty", 
        col = "darkmagenta", axes = TRUE, horiz = TRUE, srt=60)
#D Write CSV in R
Datafive<-write.csv(dfc, file = "Datafive.csv",row.names=FALSE)
Datafive

