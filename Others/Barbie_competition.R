
# Dependencies
library(dplyr)
library(ggplot2)
library(tidyr)
library(reshape2)
library(tidyverse)

band<- c(2:10)
dist<-c(17.5, 23, 27.5, 32.5, 37.7, 41.5, 46.5, 51, 56.5)

df<- as.data.frame(cbind(band, dist))
names(df)<- c("band", "dist")
df
# Plot
g1= ggplot(df, aes(x=band, y=dist) )
g2= g1+ geom_point() 
g2

# Hypothesis
fit<- lm(df$dist ~df$band)
summary(fit)
qt(0.05/2, 7)
confint(fit)

# Added band 20
band<- c(2:10, 20)
dist<-c(17.5, 23, 27.5, 32.5, 37.7, 41.5, 46.5, 51, 56.5, 104)

df<- as.data.frame(cbind(band, dist))
names(df)<- c("band", "dist")
# Plot
g1= ggplot(df, aes(x=band, y=dist) )
g2= g1+ geom_point() 
g2

# Hypothesis
fit2<- lm(df$dist ~df$band)
summary(fit2)
qt(0.05/2, 7)
confint(fit2)
