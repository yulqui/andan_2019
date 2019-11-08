#HW

is.odd <- function(x) {
  ifelse (nchar(x)!=0, yes=!(nchar(x)%%2==0), no=FALSE) 
   
}
is.odd(c("odd", "even", ""))
