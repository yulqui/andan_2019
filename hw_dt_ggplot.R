# 1) Разделить всех респондентов на пьющих и непьющих на основании переменной DRINK
# 2) Построить график доли ответов Yes на вопросы
# /LEWHYSX1  "DID R HAVE LE SEX TO MAKE UP AFTER FIGHT"
# /LEWHYSX2  "DID R HAVE LE SEX TO RELIEVE TENSION"
# /LEWHYSX3  "DID R HAVE LE SEX BECAUSE P WANTED IT"
# /LEWHYSX4  "DID R HAVE LE SEX FOR SAKE OF PREGNANCY"
# /LEWHYSX5  "DID R HAVE LE SEX TO EXPRESS LOVE"
# /LEWHYSX6  "DID R HAVE LE SEX FOR SOME OTHER REASON"
# в зависимости от употребления алкоголя.

library(foreign)


variable <- grep('DRINK', names(dataV), fixed=T, value = T)
View(dataV$DRINK)
drink_table <- dataV[, .SD, .SDcol=c('CASEID', 'DRINK')]
drink_table_cleaned <-  remove_trash(drink_table)
drink_table_cleaned[, .N, by='DRINK']

# check_drinking_status <- function (x) {
#   ifelse((x=='Daily' | x=='Daily' | x=='Several times mo'), drinking_status:='drinking',  drinking_status:='not drinking')
# }
# check_drinking_status(drink_table_cleaned)
# is.data.table(drink_table_cleaned)

#drink_table_cleaned_2<-drink_table_cleaned[, .N,by=DRINK]
#drink_melt<-melt(data=drink_table_cleaned,id.vars=c("CASEID"),measure.vars=c('DRINK')) #measure.vars=количество столбцов


appeal_vars2<-grep("APPEAL",names(dataV),fixed=T,value=T)
drink_melt<-melt(data=dataV,id.vars=c("CASEID"),measure.vars=variable) 
setnames(drink_melt, old='drink frequency', new='drink_frequency')
drink_melt <- drink_melt[variable == 'DRINK'] 
setnames(drink_melt, old='variable', new='drink var')
drink_melt <- drink_melt[, drinking_status:='drinking']
#dat[JAIL == 1 & !(ATTEND %in% c(0, 1, 9)), status := 'religious prisoner']
drink_melt <- drink_melt[	drink_frequency=='Not at all' ,drinking_status:='not drinking']

le_why <- '/LEWHYSX1  "DID R HAVE LE SEX TO MAKE UP AFTER FIGHT"
  /LEWHYSX2  "DID R HAVE LE SEX TO RELIEVE TENSION"
  /LEWHYSX3  "DID R HAVE LE SEX BECAUSE P WANTED IT"
  /LEWHYSX4  "DID R HAVE LE SEX FOR SAKE OF PREGNANCY"
  /LEWHYSX5  "DID R HAVE LE SEX TO EXPRESS LOVE"
  /LEWHYSX6  "DID R HAVE LE SEX FOR SOME OTHER REASON"'

LE_WHYYYY <- read.table(text=le_why, stringsAsFactors = F)
LE_WHYYYY$V1 <- gsub('/','', LE_WHYYYY$V1, fixed=T)

sex_and_drink<-merge(drink_melt,dataV[,.(CASEID,GENDER)],all.y=T)

lewhy_vars<-grep("LEWHY",names(dataV),fixed=T,value=T)
LEWHY<-melt(data=dataV,id.vars=c("CASEID"),measure.vars=lewhy_vars)
LEWHY_FREQ<-LEWHY[,.N,by=c("variable","value")]
LEWHY_FREQ<-LEWHY_FREQ[!(value %in% c(NA,"DK","Missing","Refusal"))]

LEWHY_FREQ[,varsum:=sum(N),by=variable]
LEWHY_FREQ[,perc:=N/varsum]
LEWHY_FREQ[,perc_label:=paste0(round(perc*100,1),"%")]
LEWHY_FREQ_LABELLED<-merge(LEWHY_FREQ,LE_WHYYYY,by.x="variable",by.y="V1")

setnames(x=LEWHY_FREQ_LABELLED,old="V2","LABEL")

LE_WHY_YES<-LEWHY_FREQ_LABELLED[value=="Yes"]
LE_WHY_YES<-LE_WHY_YES[order(perc)]
LE_WHY_YES[,LABEL:=factor(LABEL,levels=LABEL)]

sex_and_drink<-merge(LEWHY,dataV[,.(CASEID,DRINK)],all.y=T)

sex_and_drink <- sex_and_drink[	DRINK=='Not at all' ,drinking_status:='not drinking']
sex_and_drink <- sex_and_drink[	!DRINK=='Not at all' ,drinking_status:='drinking']

sex_by_drink<-sex_and_drink[,.N,by=c("drinking_status","variable","value")]
sex_by_drink<-sex_by_drink[!(value %in% c(NA,"DK","Missing","Refusal"))]
sex_by_drink[,varsum:=sum(N),by=c("variable","drinking_status")]
sex_by_drink[,perc:=N/varsum]
sex_by_drink[,perc_label:=paste0(round(perc*100,1),"%")]

yes_sex_by_drink<-sex_by_drink[value=="Yes"]

yes_sex_by_drink<-merge(yes_sex_by_drink,LE_WHYYYY,by.x="variable",by.y="V1")   
yes_sex_by_drink <- merge(yes_sex_by_drink, LE_WHY_YES$LABEL, by='variable')
setnames(yes_sex_by_drink,"V2","LABEL")


yes_sex_by_drink[,LABEL:=factor(LABEL,levels=LE_WHY_YES$LABEL)]


library(ggplot2)

ggplot(data=yes_sex_by_drink, aes(x=LABEL.y, y=perc.x, fill=drinking_status)) +
  geom_bar(stat="identity", position=position_dodge()) +coord_flip()+theme_bw() + geom_text(aes(label = perc_label.x, y = perc.x, hjust=-0.3), fontface="bold", colour="black",size = 2.5, position=position_dodge(width = 1))+theme(axis.title.x = element_blank(),axis.text.x = element_blank(),axis.ticks = element_blank(), panel.grid = element_blank(),panel.border = element_blank(),panel.spacing = unit(10,"cm")) + scale_y_continuous(limits=c(0,max(LE_WHY_YES$perc)+0.05)) + ggtitle("Percent of yes answers per sex situation") + labs(x="situations") 




