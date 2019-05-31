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


########## Twitter API ##########

api_key = "O1wKPp3sOYMJH3NzWIqd6jAdf"
api_secret = "hedARa6too4P31ulm1xUFr5K0UIrJeT7Jx3IpAY54KOvqw3SAb"
access_token = "1040035561812185093-D6g8ojpgchwsaQwnFERHff8R6HpZcy"
access_token_secret = "aZ1jlQksBd95vlgFfc8eI0qImjvQbvQHPLTJ9cQEfPvsE"

#Load twitteR
install.packages("twitteR")
library("twitteR")
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)


