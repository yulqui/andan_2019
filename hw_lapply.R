#1
feet_to_m <- function(feet) {
  meters <- feet*0.3048
  return(meters)
}
feet_to_m(900)

#2
standard_deviation <- function(vector_xi) {
  xmean = mean(vector_xi)
  n = length(vector_xi)
  summ = sum((vector_xi - xmean)^2)
  st_d <- summ / (n - 1)
  return(sqrt(st_d))
}
standard_deviation(c(1, 2, 3, 3, 2, 1))



#3
#typeof(length(unique(dataV$ATTEND)))
#result<-c(rep(NA, times=length(unique(dataV$ATTEND))))
#relig_vec <- sapply(dataV$ATTEND, )
#unique_attend = unique(dataV$ATTEND)
#unique_dataV[ATTEND == 'Never', .N]
relig <- dataV[, .N, by=ATTEND]
#View(relig)


#4 & 5
which.max(relig$N)
relig[which.max(relig$N)]

#6
attend_types = unique(dataV$ATTEND)

#7
result <- sapply(attend_types, perc, v=dataV$ATTEND)

#8
set.seed(89)
dlina_kotikov<-rnorm(20, 1.6, 0.3)
cats_meters <- feet_to_m(dlina_kotikov)
max(cats_meters)
counter <- 0
find_biggest <- function(cats){
  counter <- cats[1]
  for (i in cats) {
    if (i>counter) {counter <- i}
  }
  return(counter)
}
find_biggest(cats_meters)
cats_meters_s <- sapply(dlina_kotikov, feet_to_m)
cats_meters_s
