b<- list(1:4,4:7,c("cat","dog","mouse"))
is_empty <- function(l){
  length(l)==0
}

hd <- function(l){
  if (is_empty(l)) stop("List is empty")
  else l[[1]]
}

tl <- function(l){
  if(is_empty(l)) stop("List is empty")
  else l[-1]
}


seconds <- function(l) {
  if (is_empty(l)) list()
  else c(hd(l)[2], seconds(tl(l)))
}

hd(b)
seconds(b)
tl(b)

countdown <- function(n){
  if (n == 0) integer()
  else c(n,countdown(n-1))
}
countdown(10)