install.packages("httr")
install.packages("jsonlite")
########## New York Time##########

#Install NY times API
install.packages("rtimes")

#NY times API key
Sys.setenv(NYTIMES_API_KEY = "ULAGIigdFMCFsmJgB5cxcwKmCjnISU6W")

#Install other packages
install.packages("devtools")
devtools::install_github("ropengov/rtimes")

#Call library
library("rtimes")

#Article search API
res <- as_search(q="bailout", begin_date = "20091001", end_date = '20091201')
res
#NYT JSON Example

install.packages("RTextTools")
library(dplyr)
library(tidyr)
library(plyr)
library(rjson)
library(jsonlite)


NYTIMES_KEY = "ULAGIigdFMCFsmJgB5cxcwKmCjnISU6W";

# Let's set some parameters
term <- "Trump" # Need to use + to string together separate words
begin_date <- "20190101"
end_date <- "20190106"

baseurl <- paste0("http://api.nytimes.com/svc/search/v2/articlesearch.json?q=",term,
                  "&begin_date=",begin_date,"&end_date=",end_date,
                  "&facet_filter=true&api-key=",NYTIMES_KEY, sep="")

initialQuery <- jsonlite::fromJSON(baseurl)
maxPages <- round((initialQuery$response$meta$hits[1] / 10)-1)

#for(i in 1:100000000)
#{  
#  j = (i + 1 -1 )/i 
#}

pages <- list()
for(i in 0:maxPages){
  nytSearch <- jsonlite::fromJSON(paste0(baseurl, "&page=", i), flatten = TRUE) %>% data.frame() 
  message("Retrieving page ", i)
  pages[[i+1]] <- nytSearch 
  Sys.sleep(1) 
}


allNYTSearch <- rbind_pages(pages)

grep("Putin", allNYTSearch$response.docs.headline.main, value = TRUE)
grep("Russia", allNYTSearch$response.docs.headline.main, value = TRUE)
grep("(Putin|Russia)", allNYTSearch$response.docs.headline.main, value = TRUE)

########## Twitter API ##########

api_key = "O1wKPp3sOYMJH3NzWIqd6jAdf"
api_secret = "hedARa6too4P31ulm1xUFr5K0UIrJeT7Jx3IpAY54KOvqw3SAb"
access_token = "1040035561812185093-D6g8ojpgchwsaQwnFERHff8R6HpZcy"
access_token_secret = "aZ1jlQksBd95vlgFfc8eI0qImjvQbvQHPLTJ9cQEfPvsE"

#Load twitteR
install.packages("twitteR")
library("twitteR")
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)


