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
data<- maxtemp
data
maxtemp1990 <- window(data, start = 1990, end = 2016)

plot(maxtemp1990, ylab = "Max Temperature", xlab = "Year")
fit1 = ses(maxtemp1990,initial = "simple",alpha = .2, h = 5)
accuracy(fit1, maxtemp1990)
plot(maxtemp1990, ylab = "Max Temperature", xlab = "Year", type = "o", xlim = c(1990, 2021))
lines(fitted(fit1), col = "blue", type = "o")
lines(fit1$mean, col = "blue", type = "o")
fit1$model

plot(maxtemp1990, ylab = "Max Temperature", xlab = "Year", type = "o", xlim = c(1990, 2021))
fit1h = holt(maxtemp1990, alpha = .8, beta = .2, damped = TRUE, initial = "optimal", h = 5)
lines(fitted(fit1h), col = "blue", type= "o")
lines(fit1h$mean,col = "darkgreen", type= "o")
fit1h$model

mean(fit1$residuals)
mean(fit1h$residuals)
