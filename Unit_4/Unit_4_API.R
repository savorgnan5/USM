install.packages("httr")
install.packages("jsonlite")

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
