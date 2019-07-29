#### Unit 12
library(dplyr)
library(ggplot2)
library(tidyr)
library(reshape2)
library(tidyverse)
library(stringr)
library(caret)
library(SDMTools)

### Question 1

tsData <- EuStockMarkets
daxind <- tsData[,1]
daxind
plot(daxind)
tsData <- EuStockMarkets[, 1] # ts data
decomposedRes <- decompose(tsData, type="mult") # use type = "additive" for additive components
plot (decomposedRes) # see plot below
stlRes <- stl(tsData, s.window = "periodic")
plot(daxind, main="German Daily Closing Prices",xlab="Year", ylab="Closing Prices") +
  abline(v = 1997, col = "col", lty = 1, lwd = 2)

### Question 2
install.packages("fpp")
library(fpp)
install.packages('fpp2', dependencies = TRUE)
library(fpp2)
maxtemp
dim(maxtemp)
class(maxtemp)
data<- (maxtemp)

maxtemp1990 <- ts(data, start = c(1990,1))

plot(maxtemp1990, ylab = "Max Temperature", xlab = "Year")
maxtemp1990<- ts(maxtemp1990)
fit1 = ses(maxtemp1990,initial = "simple",alpha = .2, h = 5)
ASE <- accuracy(fitted(fit1), maxtemp1990)
plot(maxtemp1990, ylab = "Max Temperature", xlab = "Year", type = "o", xlim = c(1990, 2021))
lines(fitted(fit1), col = "blue", type = "o")
lines(fit1$mean, col = "blue", type = "o")
fit1$model

ASEx<- ASE[,1]^2
ASEx
plot(maxtemp1990, ylab = "Max Temperature", xlab = "Year", type = "o", xlim = c(1990, 2021))
fit1h = holt(maxtemp1990, alpha = .8, beta = .2, damped = TRUE, initial = "optimal", h = 5)
lines(fitted(fit1h), col = "blue", type= "o")
lines(fit1h$mean,col = "darkgreen", type= "o")
fit1h$model$aicc

mean(fit1$residuals)
mean(fit1h$residuals)

ASEh<-accuracy(fitted(fit1h), maxtemp1990 )
ASEhx <- ASEh[,1]^2
ASEhx
##### Question 3
install.packages("dygraphs")
library(dygraphs)
Olli <- read.csv("Unit12TimeSeries_Ollivander.csv", header=FALSE)
head(Olli)
Grego <- read.csv("Unit12TimeSeries_Gregorovitch.csv", header=FALSE)
head(Grego)
library(xts)
library(pander)
library(readr)

Olli$V1 <- as.Date(Olli$V1,"%m/%d/%Y")
Grego$V1 <- as.Date(Grego$V1,"%m/%d/%Y")


Gregorovitch <- read_csv("Unit12TimeSeries_Gregorovitch.csv", 
                         col_names = FALSE, col_types = cols(X1 = col_date(format = "%m/%d/%Y")))
Ollivander <- read_csv("Unit12TimeSeries_Ollivander.csv", 
                       col_names = FALSE, col_types = cols(X1 = col_date(format = "%m/%d/%Y")))
sessionInfo()
install.packages('pander')
OlliTS <- xts(Olli$V2,order.by=Olli$V1)
GregoTS <- xts(Grego$V2,order.by=Grego$V1)
head(OlliTS)
head(GregoTS)
combined<- cbind(OlliTS, GregoTS)

###### Plot

dygraph(combined,main="Wand Sales by Maker",xlab="Date",ylab="Sales") %>%
  dySeries("OlliTS", label="Ollivander", col="blue") %>%
  dySeries("GregoTS", label="Gregorovitch",col="orange") %>%
  dyOptions(stackedGraph = TRUE)%>%
  dyRangeSelector() %>%
  dyShading(from="1995-01-01",to="1999-07-31") %>%
  dyHighlight(highlightSeriesOpts = list(strokeWidth =3))%>% 
  dyLegend(width=500)

dygraph(lungDeaths) %>%
  dySeries("mdeaths", label = "Male") %>%
  dySeries("fdeaths", label = "Female") %>%
  dyOptions(stackedGraph = TRUE) %>%
  dyRangeSelector(height = 20)

maxtemp1990 = ts(data$Temp,start = c(1990,1), frequency = 1)
plot(maxtemp1990, ylab = "Max Temperature", xlab = "Year")
fit1 = ses(maxtemp1990,initial = "simple",alpha = .2, h = 5)
accuracy(fitted(fit1), maxtemp1990)
plot(maxtemp1990, ylab = "Max Temperature", xlab = "Year", type = "o", xlim = c(1990, 2021))
lines(fitted(fit1), col = "blue", type = "o")
lines(fit1$mean, col = "blue", type = "o")


recent <- window(maxtemp,start=c(1990,1))
forecast <- ses(recent, h=5)
plot(forecast)
lines(forecast$fitted,col="blue")
aicc<-forecast$model$aicc
accuracy(forecast, recent)