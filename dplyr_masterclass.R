library(data.table)
data_fics <- fread('fics_simple.csv')
library('dplyr')
library('lubridate')
data_fics %>% nrow()
data_fics %>% filter(size_cat=='small') %>% nrow()
df %>% filter(size_kb > 100) %>% nrow()
df %>% top_n(1, desc(published)) #desc - descending
#фильтр быстрее сортировки

#чистка данных mutate & arrange
#mutate - добавляется новая колонка, transmute - выдает только новую созданную колонку

#arrange - сортировка

ymd(df$published) %>% class()
df2 <- df %>% mutate(Time_of_writing = (ymd(df$last_update) - ymd(df$published)))

df2 %>% filter(Time_of_writing == max(Time_of_writing)) %>% select(id, title, published, last_update, Time_of_writing)
df2 %>% filter(Time_of_writing > 365) %>% top_n(1, desc(published))

df2 <- df2 %>% mutate(orig_lang = ifelse(translated, 'Unknown', 'Russian'))
df2 %>% filter(orig_lang=='Unknown') %>% top_n(1, desc(published))
df %>% '[['(1) %>% '['(1:10)
#=pull

fifth_trans <- df2 %>% arrange(published)
df %>% slice(1:10, 15)




library(data.table)
df <- stream_in(    file('fics.jsl'),  simplifyMatrix=FALSE) 

genres<-unique(unlist(df$genre))

dt<-data.table(df)    

dt[,c(genres):=lapply(genres,function(x) sapply(genre, function(y)  x %in% y))]



