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

# Question 1

#1A / 1B
hp<-read_html("http://www.imdb.com/title/tt1201607/fullcredits?ref_=tt_ql_1")
hp_table<-html_nodes(hp,"table")
derp<-html_table(hp_table)
derp[1]
class(derp)
a <- data.frame(derp[3])
head(a)

#1C/1D/1E
names(a) <- c("Blank", "Actor", "Blank2","Character")
df<-a[2:length(a$Actor),c("Actor", "Character")]
df$Character[10] <- "Griphook / Professor Filius Flitwick"
head(df)
df %>%
  slice(-92) %>%
  separate(Actor, into=c("FirstNames", "Surname"), sep="[ ](?=[^ ]+$)") -> b
head(b,10)

#Question 2

#2A/2B

sports <- read_html('http://www.espn.com/nba/team/stats/_/name/sa/san-antonio-spurs')
sports<-sports %>% html_nodes("table") %>% .[[2]] %>% html_table()
head(sports)
df2<-data.frame(sports)
df2 %>% slice(-14)->df2
df2

#2C

df2<- separate(df2, Name, into = c("Name","Position"), sep = -2)
df2

#2D
#Prepare

players <- read_html('http://www.espn.com/nba/team/stats/_/name/sa/san-antonio-spurs')
players<-players %>% html_nodes("table") %>% .[[2]] %>% html_table()
stats <- read_html('http://www.espn.com/nba/team/stats/_/name/sa/san-antonio-spurs')
stats<-stats %>% html_nodes("table") %>% .[[8]] %>% html_table()
spurs  <- cbind(players,stats)
head(spurs)
dfp<- separate(spurs, Name, into = c("Name","Position"), sep = -2)
head(dfp)
names(dfp)[5] <- "FGP"
dfp<- dfp[-14,]
dfp

#Plot

p1= ggplot(data=dfp, aes(x=Name, y=FGP, fill=Position)) 
p2 = p1 +geom_bar(stat="identity")
p3 = p2 + theme_minimal() 
p4 = p3 + coord_flip()
p5 = p4 + ggtitle("Spurs Players Performance by Positions") + labs(x= "Spurs Players",  y= "Field Goal Percentage")
p5
