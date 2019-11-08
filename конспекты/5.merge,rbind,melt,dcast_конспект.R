
library(data.table)
library(ggplot2)
# load("dataSex.Rdata")
library(foreign)
dataV<-data.table(read.spss("06647-0001-Data.sav",use.value.labels=T,
                            to.data.frame=T))
# library(rio)
# dataRio<-import("2016/dataset/06647-0001-Data.sav")


#ещё немного про работу с текстами
#в R есть множество функций для них. :-)

#помимо собственно исследовательских пакетов, есть много задач прикладного характера при написании кода вашего скрипта, так как имена переменных - это тоже текст. 

#чаще всего в моей практике нужны такие функции, как grep() и gsub()

#данные функции используют в своей работе регулярные выражения (POSIX-style и Perl-style, если perl=TRUE)

#к сожалению, если ты не работаешь с регулярными выражениями регулярно, то запомнить ЭТО невозможно. Поэтому мы с вами ограничимся примерами, в которых можно использовать параметр fixed=TRUE, то есть прямым поиском целой подстроки, как в обычном текстовом редакторе, например. 
appeal_nums<-grep("APPEAL",names(dataV),fixed=T)
appeal_vars<-grep("APPEAL",names(dataV),fixed=T,value=T)
appeal_vars2<-gsub(pattern = "APPEAL",replacement = "appeal_",x = appeal_vars,fixed = T)
setnames(x = dataV,old = appeal_vars,new = appeal_vars2)

table(dataV$appeal_1)

a <- c('111', '221', '212', '233')
gsub(pattern = '1', replacement = '999', x = a) #по умолчанию поиск с помощью регулярных выражений осуществляется

#узкий формат данных


#сначала немного про melt

# мелт преобразует данные в "узкий" формат "идентификаторы, имена переменных, значения переменных". Это может быть полезно, когда мы хотим, например, построить графики по нескольким однотипным переменным сразу. 


appeal_vars2<-grep("APPEAL",names(dataV),fixed=T,value=T)

DT<-dataV[,.SD,.SDcol=appeal_vars2] #новая табличка только с аппилами, широкий вариант - для каждой переменной своя колонка

APPEALS<-melt(data=dataV,id.vars=c("CASEID","RELIG"),measure.vars=appeal_vars2) #measure.vars=количество столбцов

APPEALS_3<-melt(data=dataV,id.vars=c("CASEID"),measure.vars=appeal_vars2) 

APPEALS_2<-APPEALS[,.N,by=c("variable","value")]

APPEALS_2<-APPEALS_2[!(value %in% c(NA,"DK","Missing","Refusal"))]

c(1,8) %in% c(1,2,3,4,5,6,7,8)

APPEALS_2[,varsum:=sum(N),by=variable]
APPEALS_2[,perc:=N/varsum]
APPEALS_2[,perc_label:=paste0(round(perc*100,1),"%")]
library(ggplot2)
g<-ggplot(data=APPEALS_2[value=="very appealing"], aes(x=variable, y=perc, fill=variable)) +
  geom_bar(stat="identity", position=position_dodge())+coord_flip()+theme_bw()+
  geom_text(aes(label = perc_label, y = perc, hjust=-0.3), fontface="bold", colour="black",size = 3, position=position_dodge(width = 1)) +theme(legend.position = 'none') +scale_y_continuous(limits=c(0,max(APPEALS_2$perc)+0.05))
#лучше все данные подготовить отдельно, а не передавать в ggplot таблицу (тогда он считает сам)
#stat=identity - указание на способ взаимодействия ggplot с данными, значит, что он ничего не должен преобразовывать
#position_dodge

ggplot(data=APPEALS_2[value=="very appealing"], aes(x=value, y=perc, fill=variable)) +
  geom_bar(stat="identity") #no position dodge - использовать только тогда, когда всё складывается в 100%, но сейчас - нет

install.packages('rio')
library(rio)
dataNV<-import("06647-0001-Data.sav")
str(dataNV$DEGREE)
attr(x = dataNV$DEGREE,which = "label")
dataNV<-data.table(dataNV)

a <- 1
attr(a, 'name') <- stroki #в атрибуты переменной можно засунуть любые данные
a


stroka <- '/APPEAL1   "APPEAL OF SEX W MORE THAN ONE P"
  /APPEAL2   "APPEAL OF SEX W SAME GENDER P"
  /APPEAL3   "APPEAL OF FORCING SOMEONE TO HAVE SEX"
  /APPEAL4   "APPEAL OF BEING FORCED TO HAVE SEX"
  /APPEAL5   "APPEAL OF WATCHING OTHERS HAVE SEX"
  /APPEAL6   "APPEAL OF HAVING SEX W STRANGER"
  /APPEAL7   "APPEAL OF WATCHING P UNDRESS"
  /APPEAL8   "APPEAL OF VAGINAL INTERCOURSE"
  /APPEAL9   "APPEAL OF USING A DILDO OR VIBRATOR"
  /APPEAL10  "APPEAL OF A P PERFORMING ORAL SEX ON R"
  /APPEAL11  "APPEAL OF PERFORMING ORAL SEX ON A P"
  /APPEAL12  "APPEAL OF ANUS STIMULATED BY PS FINGERS"
  /APPEAL13  "APPEAL OF STIMULATING PS ANUS W FINGERS"
  /APPEAL14  "APPEAL OF PASSIVE ANAL INTERCOURSE"
  /APPEAL15  "APPEAL OF ACTIVE ANAL INTERCOURSE"'

stroki <- read.table(text=stroka, stringsAsFactors = F)
stroki$V1 <- gsub('/','', stroki$V1, fixed=T)
stroki$V2 <- gsub('APPEAL OF ','', stroki$V2, fixed=T)

str1 <- c('/\\\\\\', 'V', "''", '*')
print(str1)
cat(str1)
grep('*', str1)
grep('\*', str1)
grep('*', str1, fixed=T) #fixed отключает распознавание regexp

appeal_names<-sapply(X = appeal_vars,function(x) attr(x = dataNV[,get(x)],which = "label"))

names(appeal_names)<-gsub(pattern = "APPEAL",replacement = "appeal_",x = names(appeal_names),fixed = T)

appeal_names<-data.table(variable=names(appeal_names),name=appeal_names)


d1<-data.table(id=c(1:20),b=rnorm(20))
d2<-data.table(id2=seq(1,40,2),b=rnorm(20))
u_d1<-merge(d1,d2,by.x="id",by.y="id2") #только для 2 таблиц за раз
u_d2<-merge(d1,d2,by.x="id",by.y="id2",all.x=T)
u_d3<-merge(d1,d2,by.x="id",by.y="id2",all.x=T,all.y=T) #fulljoin


APPEAL3<-merge(APPEALS_2,stroki,by.x = 'variable', by.y = 'V1') #если имена колонок одинаковые, то можно вместо by.x, by.y указывать только by="variable"    
setnames(x=APPEAL3, old='V2', new = "APPEAL")


g<-ggplot(data=VERY_APPEALING, aes(x=APPEAL, y=perc, fill=APPEAL)) +
  geom_bar(stat="identity", position=position_dodge())+coord_flip()+theme_bw()+
  geom_text(aes(label = perc_label, y = perc, hjust=-0.3), fontface="bold", colour="#FF0000",size = 3, position=position_dodge(width = 1)) +theme(legend.position = 'none', axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank(), panel.border = element_blank()) +scale_y_continuous(limits=c(0,max(APPEALS_2$perc)+0.05)) +ggtitle('Percent of "very appealing" answers per practice type') +labs(x='practice type')

VERY_APPEALING <- APPEAL3[value=='very appealing']
VERY_APPEALING <-VERY_APPEALING[order(VERY_APPEALING$perc)]
VERY_APPEALING[,APPEAL:=factor(APPEAL, levels = APPEAL)] #levels=APPEAL because variable APPEAL is already sorted in order, we turn APPEAL into factor bc ggplot sorts according to factor variable (by default)


#обратное преобразование в "широкий" формат из melt

comeback<-dcast.data.table(APPEALS,CASEID + RELIG ~variable,value.var="value")


#Допустим, мы что-то забыли добавить в нашу таблицу, или есть какие-то дополнительные данные из другого файла. Тут нам пригодится команда merge

a1<-dataV[,.(GENDER)]
a2<-dataV[,GENDER]
str(a2)
# APPEALS_WITH_GENDER<-merge(APPEALS,dataV[,.(CASEID,GENDER)],all.y=T)
# appeals_by_gender <- APPEALS_WITH_GENDER[,.N, by=c('GENDER', 'variable', 'value' )]
# appeals_by_gender<-appeals_by_gender[!(value %in% c(NA,"DK","Missing","Refusal"))]
# appeals_by_gender<-appeals_by_gender[,.N,by=c("variable","GENDER")]
# appeals_by_gender[,perc:=N/varsum]
# appeals_by_gender[,perc_label:=paste0(round(perc*100,1),"%")]

APPEALS_WITH_GENDER<-merge(APPEALS,dataV[,.(CASEID,GENDER)],all.y=T)

appeals_by_gender<-APPEALS_WITH_GENDER[,.N,by=c("GENDER","variable","value")]
appeals_by_gender<-appeals_by_gender[!(value %in% c(NA,"DK","Missing","Refusal"))]
appeals_by_gender[,varsum:=sum(N),by=c("variable","GENDER")]
appeals_by_gender[,perc:=N/varsum]
appeals_by_gender[,perc_label:=paste0(round(perc*100,1),"%")]

# APPEALS_WITH_GENDER2<-merge(APPEALS,dataV[,.(CASEID,GENDER)],by="CASEID",all.y=T)
# 
# d1<-data.table(id=c(1:20),b=rnorm(20),x=c(20:1))
# d2<-data.table(id2=seq(1,40,2),b=rnorm(20),x=c(20:1))
# 
# u_d1<-merge(d1,d2,by.x=c("id","x"),by.y=c("id2","x"))

VERY_APPEALING_GENDER <- appeals_by_gender[value=='very appealing']


VERY_APPEALING_GENDER<-merge(VERY_APPEALING_GENDER,stroki,by.x = 'variable', by.y = 'V1') #если имена колонок одинаковые, то можно вместо by.x, by.y указывать только by="variable"    
setnames(x=VERY_APPEALING_GENDER, old='V2', new = "APPEAL")

VERY_APPEALING_GENDER[, APPEAL:=factor(APPEAL, levels=VERY_APPEALING$APPEAL)]


ggplot(data=VERY_APPEALING_GENDER, aes(x=APPEAL, y=perc, fill=GENDER)) +
  geom_bar(stat="identity", position=position_dodge())+coord_flip()+theme_bw()+
  geom_text(aes(label = perc_label, y = perc, hjust=-0.3), fontface="bold", colour="#FF0000",size = 3, position=position_dodge(width = 1)) +theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank(), panel.border = element_blank()) +scale_y_continuous(limits=c(0,max(APPEALS_2$perc)+0.05)) +ggtitle('Percent of "very appealing" answers per practice type') +labs(x='practice type')
