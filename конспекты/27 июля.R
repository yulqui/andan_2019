setwd("F:/undone")
library(data.table)
library(foreign)
dataV<-data.table(read.spss("2016/dataset/06647-0001-Data.sav",use.value.labels=T,
                            to.data.frame=T))

set.seed(87)
d1<-data.table(id=c(1:20),b=rnorm(20))

sapply(d1,mean)

appeals<-grep(pattern = "APPEAL",  x = names(dataV),value = T)

ggplot(data=dataV,aes(x=APPEAL1,y=APPEAL2)) + geom_point(alpha=0.01)

remove_trash<-function(x)
{
if (length(x)==0) {
  warning ("empty vector")
  return(x)
}
x[x=="DK"] <- NA
x[x=="Refusal"] <- NA
x[x=="Missing"] <- NA
x[x == "Other"] <- NA
# print ("конец")
x
}
ggplot(data=dataV,aes(x=remove_trash(APPEAL1),y=remove_trash(APPEAL2))) + geom_point(alpha=0.01)

if (2==3) 
{ print("первое")} else 
{print( "Второе")}

#.SD и .SDcol

selected_columns<-dataV[,.SD,.SDcol=1634:1]

prepared_columns<-dataV[,lapply(.SD,remove_trash),.SDcol=31:37]

dataV[,.N, by=get(names(dataV)[31])]
prepared_columns[,.N, by="WORKNITE"]
# a=5
# a
# get("a")

mean(as.numeric(dataV$APPEAL8),na.rm = T)

var<-c("1","2","DK")

levels(dataV$APPEAL1)

mean_factor<-function(var) mean(as.numeric(remove_trash(var)),na.rm = T)


dataV[,sapply(.SD,mean_factor),.SDcol=appeals]

results<-data.table(appeal_name=appeals,mean=dataV[,sapply(.SD,mean_factor),.SDcol=appeals])


