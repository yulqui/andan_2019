setwd("F:/undone")
library(data.table)
library(ggplot2)
# load("dataSex.Rdata")
library(foreign)
dataV<-data.table(read.spss("2016/dataset/06647-0001-Data.sav",use.value.labels=T,
                            to.data.frame=T))

#выбираем нужные нам переменные
appeal_vars<-grep("APPEAL",names(dataV),fixed=T,value=T)

# мелт преобразует данные в "узкий" формат "идентификаторы, имена переменных, значения переменных". Это может быть полезно, когда мы хотим, например, построить графики по нескольким однотипным переменным сразу. 


APPEALS<-melt(data=dataV,id.vars=c("CASEID"),measure.vars=appeal_vars)

#считаем частоту

APPEALS_2<-APPEALS[,.N,by=c("variable","value")]

#удаляем миссинги разных типов
APPEALS_2<-APPEALS_2[!(value %in% c(NA,"DK","Missing","Refusal"))]

# cчитаем проценты от общего числа ответов на каждый вопрос
APPEALS_2[,varsum:=sum(N),by=variable]
APPEALS_2[,perc:=N/varsum]
APPEALS_2[,perc_label:=paste0(round(perc*100,1),"%")]

# рисуем!
library(ggplot2)

ggplot(data=APPEALS_2[value=="very appealing"], aes(x=variable, y=perc, fill=variable)) +
  geom_bar(stat="identity", position=position_dodge()) +coord_flip()+theme_bw() + geom_text(aes(label = perc_label, y = perc, hjust=-0.3), fontface="bold", colour="black",size = 2.5, position=position_dodge(width = 1))+theme(legend.position = "none") + scale_y_continuous(limits=c(0,max(APPEALS_2$perc)+0.05))

# улучшаем, считываем переменные из файла синтаксиса SPSS

stroka<-'  /APPEAL1   "APPEAL OF SEX W MORE THAN ONE P"
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

stroki<-read.table(text=stroka,stringsAsFactors = F)
stroki$V1<-gsub("/","",stroki$V1,fixed = T)
stroki$V2<-gsub("APPEAL OF ","",stroki$V2,fixed = T)

# соединяем лейблы переменных с данными через merge
APPEAL3<-merge(APPEALS_2,stroki,by.x="variable",by.y="V1")   

setnames(x=APPEAL3,old="V2","APPEAL")

ggplot(data=APPEAL3[value=="very appealing"], aes(x=APPEAL, y=perc, fill=APPEAL)) +
  geom_bar(stat="identity", position=position_dodge()) +coord_flip()+theme_bw() + geom_text(aes(label = perc_label, y = perc, hjust=-0.3), fontface="bold", colour="black",size = 2.5, position=position_dodge(width = 1))+theme(legend.position = "none") + scale_y_continuous(limits=c(0,max(APPEALS_2$perc)+0.05))

# улучшаем дальше, меняем порядок 

VERY_APPEALING<-APPEAL3[value=="very appealing"]
VERY_APPEALING<-VERY_APPEALING[order(perc)]
VERY_APPEALING[,APPEAL:=factor(APPEAL,levels=APPEAL)]

# функции для генерации случайных цветов заданного количества, максимально удалённых друг от друга

generate_hue_rainbow <- function(n, start = sample(1:360, 1))
{
  h <- sapply (c(0:(n - 1)), function(x)
  {
    hue <- round(start + x * (360 / (n)))
    if (hue >= 360)
      hue <- hue - 360
    hue
  })
  h
}

hsl_to_rgb <- function(h, s, l) {
  h <- h / 360
  r <- g <- b <- 0.0
  if (s == 0) {
    r <- g <- b <- l
  } else {
    hue_to_rgb <- function(p, q, t) {
      if (t < 0) {
        t <- t + 1.0
      }
      if (t > 1) {
        t <- t - 1.0
      }
      if (t < 1 / 6) {
        return(p + (q - p) * 6.0 * t)
      }
      if (t < 1 / 2) {
        return(q)
      }
      if (t < 2 / 3) {
        return(p + ((q - p) * ((2 / 3) - t) * 6))
      }
      return(p)
    }
    q <- ifelse(l < 0.5, l * (1.0 + s), l + s - (l * s))
    p <- 2.0 * l - q
    r <- hue_to_rgb(p, q, h + 1 / 3)
    g <- hue_to_rgb(p, q, h)
    b <- hue_to_rgb(p, q, h - 1 / 3)
  }
  return(rgb(r, g, b))
}

generate_rgb_rainbow <-
  function(n,
           s = 0.6,
           l = 0.5,
           random_order = T,
           start = F)
  {
    if (start != F)
      colors <-
        generate_hue_rainbow(n, start)
    else
      colors <- generate_hue_rainbow(n)
    colors <- sapply(colors, function(x)
      hsl_to_rgb(h = x, s = s, l = l))
    if (random_order)
      colors <- sample(colors, n)
    colors
  }

# пример
generate_rgb_rainbow(5,random_order = F,start = 1)

# теперь - в графике
ggplot(data=VERY_APPEALING, aes(x=APPEAL, y=perc, fill=generate_rgb_rainbow(nrow(VERY_APPEALING),random_order = F,start = 1,s = 0.8))) +
  geom_bar(stat="identity", position=position_dodge()) +coord_flip()+theme_bw() + geom_text(aes(label = perc_label, y = perc, hjust=-0.3), fontface="bold", colour="#FF0000",size = 2.5, position=position_dodge(width = 1))+theme(legend.position = "none",axis.title.x = element_blank(),axis.text.x = element_blank(),axis.ticks = element_blank(), panel.grid = element_blank(),panel.border = element_blank()) + scale_y_continuous(limits=c(0,max(VERY_APPEALING$perc)+0.05)) + ggtitle("Percent of 'very appealing' answers per practice type") + labs(x="practice type") 


# этот кусок - про домашку... 
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

result=data.table(appeal_vars,

means=dataV[,sapply(.SD,  function(x) mean(as.numeric(remove_trash(x)),na.rm = T)), .SDcol=appeal_vars  ],

sd=dataV[,sapply(.SD,  function(x) sd(as.numeric(remove_trash(x)),na.rm = T)), .SDcol=appeal_vars]
)

kubkv<-function(x) {
  if(!is.numeric(x)) stop ("numeric!")
  x[x>0]<-x[x>0]^2
  x[x<0]<-x[x<0]^3
  x
}

kubkv2<-function(x) {
  if(!is.numeric(x)) stop ("numeric!")
  if(x<0) return (x^3) else return (x^2)
}

kubkv3<-function(x) 
  {
  if(!is.numeric(x)) stop ("numeric!")
  ifelse(test = x>0,yes = x^2, no = x^3)
}
kubkv(10)
kubkv2(10)

kubkv(-10)
kubkv2(-10)

kubkv(c(10,-10))
kubkv3(c(10,-10))



#обратное преобразование в "широкий" формат

comeback<-dcast(APPEALS,CASEID ~variable,value.var="value")

# Допустим, мы что-то забыли добавить в нашу таблицу, или есть какие-то дополнительные данные из другого файла. Тут нам пригодится команда merge

d1<-data.table(id=c(1:20),b=rnorm(20))
d2<-data.table(id2=seq(1,40,2),b=rnorm(20))

u_d1<-merge(x=d1,y=d2,by.x="id",by.y="id2")
u_d2<-merge(x=d1,y=d2,by.x="id",by.y="id2",all.x=T)
u_d3<-merge(x=d1,y=d2,by.x="id",by.y="id2",all.x=T,all.y=T)


APPEALS_WITH_GENDER<-merge(APPEALS,dataV[,.(CASEID,GENDER)],all.y=T)

# снова готовим данные для графика, но теперь - с разбиением по гендеру


appeals_by_gender<-APPEALS_WITH_GENDER[,.N,by=c("GENDER","variable","value")]
appeals_by_gender<-appeals_by_gender[!(value %in% c(NA,"DK","Missing","Refusal"))]
appeals_by_gender[,varsum:=sum(N),by=c("variable","GENDER")]
appeals_by_gender[,perc:=N/varsum]
appeals_by_gender[,perc_label:=paste0(round(perc*100,1),"%")]

VERY_APPEALING_GENDER<-appeals_by_gender[value=="very appealing"]

VERY_APPEALING_GENDER<-merge(VERY_APPEALING_GENDER,stroki,by.x="variable",by.y="V1")   

setnames(VERY_APPEALING_GENDER,"V2","APPEAL")


VERY_APPEALING_GENDER[,APPEAL:=factor(APPEAL,levels=VERY_APPEALING$APPEAL)]

ggplot(data=VERY_APPEALING_GENDER, aes(x=APPEAL, y=perc, fill=GENDER)) +
  geom_bar(stat="identity", position=position_dodge()) +coord_flip()+theme_bw() + geom_text(aes(label = perc_label, y = perc, hjust=-0.3), fontface="bold", colour="#FF0000",size = 2.5, position=position_dodge(width = 1))+theme(axis.title.x = element_blank(),axis.text.x = element_blank(),axis.ticks = element_blank(), panel.grid = element_blank(),panel.border = element_blank(),panel.spacing = unit(10,"cm")) + scale_y_continuous(limits=c(0,max(VERY_APPEALING$perc)+0.05)) + ggtitle("Percent of 'very appealing' answers per practice type") + labs(x="practice type") 

