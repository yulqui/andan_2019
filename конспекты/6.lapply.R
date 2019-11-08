setwd("F:/undone")
library(data.table)
library(foreign)
dataV<-data.table(read.spss("06647-0001-Data.sav",use.value.labels=T,
                            to.data.frame=T))

View(dataV)
set.seed(87)
d1<-data.table(id=c(1:20),b=rnorm(20))
View(d1)

apply(d1,1,mean) #rows
apply(d1,2,mean) #columns

# result2<-apply(d1[,.(id,x)],1,mean)

d1<-data.table(id=c(1:2000),b=rnorm(2000))

library(microbenchmark)

#????????? ??? ??????? ?????????????? ??? ???? ??????. ??? ??? ?? ???? ??????: ?????????? ???????? ?? ??????, ?????? ??? ?? ??? ??????? ????? ????? ?????
result<-numeric()
for (i in c(1:nrow(d1)))
{
  result<-c(result,mean(t(as.data.frame(d1)[i,])))
}

#??? ?????? ?????????? ??????? ? ?????? R ???????????? ???? ?????? - ??????????? ?? ????? ???, ????? ??????? ????????????? ?????? ?????? ?????, ? ????? ??? ????????????. 

#??? ??? ??????? ?  ?????

result<-c(rep(NA,nrow(d1)))
for (i in c(1:nrow(d1)))
{
result[i]<-mean(t(as.data.frame(d1)[i,]))
}

x<-d1$id

#ЦИКЛ FOR I (name of element, enumerator) IN VECTOR OR SET OF ITEMS

for (i in c(1:3)) {
print(i^2)  
}

for (i in names(dataV)[1:10])
{
print(paste0(i, " у тебя в штанах"))  
}


v <- c(1:4)
f <- function(x, deg=2) x^deg
sapply(v, f, deg=4)

#sapply максимально упрощает (например вектор проще списка) и предполагает одномерный вход и ходит по колонкам, apply берет двуменый вход и ходит по колонкам или строчкам, lapply все элементы будет рассматривать как список и на выходе дает список
#function(i) - неименнованная функция, команда рядом
sapply(names(dataV)[1:10], function(i) paste0(i, " у тебя в штанах"))

sapply(names(dataV)[1:10],  paste, " у тебя в штанах", sep=" не ")

sapply(names(dataV)[1:10], function(i) paste("у тебя в штанах", i, sep=" не "))

list1<-lapply(d1,function(x) 
  {mean(x-1)}  )

num_of_unique_items <- sapply(dataV, uniqueN)
head(num_of_unique_items)

#percentage <- function(num, total) paste(round((num/total)*100, 2), '%')
res <- sapply(dataV, perc, value='DK')
#num_of_DK_answers <-  sapply(dataV, length(dataV[dataV==value]))
#part_of_DK_answers <- sapply(dataV, )

#names(dataV)
#dataV[,2][1]

#for (i in ncol(dataV)) {
 # for (j in nrow(dataV)) {
  #  if (dataV[,i][j] == 'DK') {dataV[,i][j] <- TRUE}
   # else {dataV[,i][j] <- FALSE}
#  }
#}

dataV2 <- dataV[, lapply(.SD, function(x)
{
  x[x == "Missing"] <- NA
  x[x == "Refusal"] <- NA
  x[x == "DK"] <- NA
  x[x == "Other"] <- NA
  x
})]


remove_missings<-function(x)
{
  x[x == "Missing"] <- NA
  x[x == "Refusal"] <- NA
  x[x == "DK"] <- NA
  x[x == "Other"] <- NA
  x
}

dataV2 <- dataV[, lapply(.SD, remove_missings)]

dataV3 <- dataV[, lapply(.SD, remove_missings),.SDcol=grep("APPEAL",names(dataV))]


sapply(names(dataV)[20:40], function(i) {
  i
})



remove_missings(c("A","M","DK","Missing","U"))

dataV[, c("HISPANIC","DEGREE"):=lapply(.SD, remove_missings),.SDcol=c("HISPANIC","DEGREE")]

dataO<-dataV

dataV2<-dataV[, .SD,.SDcol=names(dataV)[order(names(dataV))]]


dataO[,c(names(dataO)):=lapply(.SD,remove_missings),.SDcol=names(dataO)]



sapply(d1,mean)

grep('APPEAL', names(dataV), value=T)
ddd <- dataV[grepl('deg',DEGREE)]
ddd
ddd2 <- dataV[DEGREE=='Fin 4-5 yr deg']
dataV$DEGREE=='Fin 4-5 yr deg'
head(ddd2)

grepl('deg', dataV$DEGREE)

#grepl = logical grep

appeals <-   grep('APPEAL', names(dataV), value=T)
appeals

ggplot(data=dataV, aes(x=APPEAL1, y=APPEAL2)) +geom_point(alpha=0.01)

remove_trash <-  function(x) {
  if(length(x)==0) {
    warning('empty vector')
    return(x)
  }
  x[x=='DK'] <- NA
  x[x=='Refusal'] <- NA
  x[x=='Missing'] <- NA
  x[x=='Missing'] <- NA
  #print('the end')
  return(x)
}
remove_trash(c())
remove_trash(dataV$APPEAL1)
remove_trash(c('G', 'DK'))

ggplot(data=dataV, aes(x=remove_trash(APPEAL1), y=remove_trash(APPEAL2))) +geom_point(alpha=0.01)


if (2==3) {print('one')} else {print('two')} #else is optional

selected_col <- dataV[,.SD,.SDcol=1634:1]
View(selected_col)

prepared_col <- dataV[,lapply(.SD, remove_trash),.SDcol=31:37]
View(prepared_col)

dataV[,.N, by=get(names(dataV)[31])]

prepared_col[,.N,by='WORKNITE']

mean(as.numeric(dataV$APPEAL10), na.rm=T)
mean_factor <- function(var) mean(as.numeric(remove_trash(var)), na.rm=T)
dataV[,sapply(.SD,mean_factor), .SDcol=appeals]

levels(dataV$APPEAL1)


results <- data.table(appeal_name=appeals, mean=dataV[,sapply(.SD,mean_factor), .SDcol=appeals])
View(results)
