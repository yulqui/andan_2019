#dplyr vs data.table hw
setwd("~/LSH_projects/data_wrangling")
library(data.table)
data_fics <- fread('fics_simple.csv', encoding = 'UTF-8')
dt <- data_fics
df <- read_csv('fics_simple.csv')

## Сколько всего фанфиков в нашем наборе?
nrow(data_fics)
data_fics[,.N]

## Сколько фанфиков имеют размер больше 100Кб
nrow(data_fics[size_cat=='small'])
data_fics[size_cat == 'small', .N]

nrow(data_fics[size_kb>100])
dt[,sum(size_kb>100)] #количество TRUE
dt[,mean(size_kb>100)] #Доля TRUE
data_fics[size_kb>100, .N]

## Когда опубликован самый первый фанфик? И что это был за фанфик?
head(data_fics[published==min(published), .(id, title, published)])

dt[,min(published)]
dt[published==min(published),.(id, title, published)]



library(microbenchmark)
library(ggplot2)
library(dplyr)
library(readr)
theme_set(theme_light(base_size = 24))
microbenchmark(
  'filter' = {df %>% filter(published == max(published))},
  'top_n' = {df %>% top_n(1, published)}
) %>% autoplot()



#df %>% filter(id %in% c(119387, 46832, 119034, 109013)) %>% select(id, title, translated) %>% mutate(
#  orig_lang = ifelse(translated, 'Unknown', 'Russian')
  
#data_fics[id %in% c(119387, 46832, 119034, 109013)]
data_fics[translated==FALSE, orig_lang:='Russian']
data_fics[translated==T, orig_lang:='Unknown']

## Какой фанфик писался дольше всего? Сколько времени заняло написание?
data_fics[max(as.Data(last_update)-as.Data(published))]

dt[dt[,as.Date(last_update)-as.Date(published)]==max(dt[,as.Date(last_update)-as.Date(published)]),title]
max(dt[,lubridate::ymd(last_update)-lubridate::ymd(published)])

data_fics[,long := (lubridate::ymd(last_update) - lubridate::ymd(published))]
data_fics[long == max(long)]


## Из тех фанфиков, которые писались больше года, какой был раньше всех начат и когда это случилось?
data_fics[long>365.25][published==min(published)]
data_fics[long>lubridate::years(1)][published==min(published),.(title, published)]

## Когда опубликовали первый переводной фанфик?
data_fics[translated == "TRUE"][published == min(published), .(title, published, last_update)]
dt[(translated)][published==min(published)]
dt[(translated),min(published)]

microbenchmark(
  'piped' = {dt[(translated)][published==min(published)]},
  'and' = {dt[(translated) & published ==min(published)]},
  'piped2' = {dt[published==min(published)][(translated)]}
) %>% autoplot()



## Сколько времени прошло от открытия сайта до завершения  пятого опубликованного перевода?
date_of_the_opening = data_fics[, min(as.Date(published))]
data1 = data_fics[translated == T, by = published]
data1
date_of_the_fifth_published_translation = sort(data1$published, decreasing = FALSE)[5]
date_of_the_fifth_published_translation
time = as.Date(date_of_the_fifth_published_translation) - as.Date(date_of_the_opening)
time #wrong


tr <- dt[translated==T]
pub5 <- tr[,sort(published, decreasing = F)][5]
lu5 <- tr[published==pub5,last_update]
as.Date(lu5)-as.Date(dt[,min(published)])

tr[,sort(published)]
fifth_tr <- tr[,sort(published)][5]
fifth_tr_labd <- tr[published==fifth_tr, last_update]
z <- as.Date(fifth_tr_labd) - as.Date(data_fics[,min(published)])
z

tr[order(published)][5, lubridate::ymd(last_update)-min(lubridate::ymd(data_fics$published))]
data_fics[order(published)][, first:=min(lubridate::ymd(published))][translated==T][5, lubridate::ymd(last_update)-first]

microbenchmark(
  'two_tables' = {tr <- dt[translated==T]
  tr[order(published)][5, lubridate::ymd(last_update)-min(lubridate::ymd(data_fics$published))] },
  'long_pipe' = {data_fics[order(published)][, first:=min(lubridate::ymd(published))][translated==T][5, lubridate::ymd(last_update)-first]}
) %>% autoplot()

## Здесь есть есть авторские произведения и переводы. Сколько авторских, сколько переводов?
dt[ ,.N,by=translated]


bdf <- rbindlist(list(df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df, df))

bdt <- as.data.frame(bdf)
  
  microbenchmark(
    'group_by' = {bdf %>% group_by(translated) %>% count()},
    'summarise' = {bdt %>% summarise(    authored = sum(!translated),     translated = sum(translated))},
    'data.table' = {bdt[ ,.N,by=translated]}
  ) %>% autoplot()

  
  ## Как часто появляются новые фанфики? Посчитайте, сколько в среднем фанфиков появляется в месяц.
  
  n <- length(dt[,unique(id)])
  n <- dt[,.N]
  
  maxpub <- as.numeric(as.Date(dt[,max(published)])) 
  minpub <- as.numeric(as.Date(dt[,min(published)]))
  maxpub-minpub
  n/(5235/31)

  library(lubridate) 
  
  data_fics[, published_month:=floor_date(ymd(published), 'month')]
  monthly_published <- data_fics[,.N, by=published_month]
  monthly_published[,mean(N)]

  data_fics[, published_month:=floor_date(ymd(published), 'month')][,.N, by=published_month][,mean(N)]

  ## Отличается ли частота публикаций в месяц, рассчитанная за всё время существования сайта, от рассчитанной за последние пять лет?
  
  monthly_published[published_month>today()-years(5), mean(N)]

  ggplot(monthly_published,aes(x=published_month,y=N)) + geom_bar(stat="identity")  

  
  ## Отличается ли частота публикаций для фанфиков разных рейтингов?
  
  data_fics[,unique(rating)]
data_fics[rating=='G', rating:='General']  

monthly_published <- data_fics[, .N, by=c('published_month', 'rating')]
monthly_published[,mean(N),by="rating"]


library(jsonlite)
df <- stream_in(file('fics.jsl'), simplifyMatrix=FALSE)


genres <- unique(unlist(df$genre))
dt <- data.table(df)
dt[,c(genres):=lapply(genres, function(x) sapply(genre, function(y) x %in% y))]


i <- c(1,2,3,4,5,9)
s <- sort(i)
o <- order(i)

identical(i[o], s)



View(data_HT10[translated==TRUE,sort(published, decreasing = F)])
View(data_HT10[translated==TRUE,order(published, decreasing = F)])
View(data_HT10[translated==TRUE][order(published, decreasing = F)])


data_fics[translated==T, .(mean(size_kb), .N)]
data_fics[, .(mean(size_kb[translated==T]), .N)]


