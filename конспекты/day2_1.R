 #ВСЯКИЕ ТАМ РАЗНЫЕ ТИПЫ ВЕКТОРОВ И ДАННЫХ 

#Character -- это вектор для хранения текстовых данных, состоящий из объектов-строк (любых символов в кавычках)

ch <- c('apple', 'pear', 'banana', 'orange')
ch
typeof(ch)





#сколько уникальных элементов содержится в векторе?
ch2 <- c('apple', 'pear', 'banana', 'orange', 'apple', 'apple')
unique(ch2)
sort(ch2)

ch[ch>"ba"]




#сколько символов в элементах вектора?
length(ch)
nchar(ch)

#поиск элементов
grep('b', ch, value=TRUE)
grep('b', ch, value=F)
ch[grep('b', ch, value=F)]

#вывод элемента по номеру
ch2[1]
#исключение элемента по номеру
ch2[-4]
ch[-1]
ch2[-c(1, 2)]


#разбиение
text <- 'Ну-ка фрукты встаньте в ряд'
strsplit(text, ' ')
a <-  strsplit(text, ' ')
typeof(a)
typeof(a[1])




#строка с пропусками вида character и digit
sprintf("%s отправляется в %d часов", "Электричка", 12)
sprintf("%s отправляется в %d часов", rep("Электричка", 2), c(12, 13))

#вытащить кусок строки
substr("Я маленькая лошадка", start=3, stop=12) 

#заменить кусок внутри строки
sub("маленькая", "большая", "Я маленькая лошадка") 
sub("маленькая", "большая", "Я маленькая маленькая лошадка") 
gsub("маленькая", "большая", "Я маленькая маленькая лошадка")



#LOGIC


#логический вектор состоит из TRUE и FALSE

lo <- c(T,T,F,T)
lo


#он нужен чтобы сортировать все остальные данные
ch[lo]


#есть НЮАНС
#он связан с NAs
lo2<-c(T,F,F,F,T,NA)
lo2==T

ch2
ch2[lo2] # выводит сNA

which(lo2) #находит только TRUE
which(!lo2) #находит только FALSE

ch2[which(lo2)]


#числовые векторы
d <- c(1,2,6,4)
d>2
d[d>2] #сами элементы
which(d>2) #номера их позиций





#Numeric (integer, double)

#целые числа
int_num<-c(1,2,3,4000)
is.integer(int_num)
typeof(int_num)
int_num<-as.integer(c(1,2,3,4000))
int_num<-c(1L,2L,3L,4000L) #сохраняет как целые
as.integer(214748364.7)
as.integer(2147483648) #слишком большое число, не может хранить
as.integer(-2147483647)
as.integer(-2147483648)
as.double("10000998843483274893274892374238947273")

#включение scientific notation
options(scipen=999) 
options(scipen=0)
2^64
90071992547409923


#числа с десятичной долей
numeric_num<-c(1,2,2.5)
is.integer(numeric_num)
is.integer(int_num)
is.numeric(int_num)
is.numeric(numeric_num)
str(numeric_num)
is.double(numeric_num)
typeof(numeric_num)
?double
?integer

#объединение векторов с разнымм типами значений
n <- c(1,2,3)
s <- c('a', 'b', 'c')
c(n,s)

#таблица (датафрейм)

n <- c(2, 3, 5) 
s <- c("aa", "bb", "cc") 
b <- c(TRUE, FALSE, TRUE) 
df <- data.frame(n, s, b)
df
View(df)
df[1, 2]
typeof(df)
typeof(df$s)
class(df$s)
class(df)

#оказывается, столбец s был воспринят как факторная переменная
#если нужно этого избежать:

df <- data.frame(n, s, b, stringsAsFactors = F)
df[1,2]

#ну а что же такое фак
#торы

# ://////////++++++++++++++++++++++++++++++++++//////:::::::::::::/:::::
# //////////+++++++++++++++++++++++++++++++ossoo++/////++syyo+//:////:::
# ///////////++++++++++++++++++++++++++++//:/+shhhhhhddddddddhyo//////::
# ////////////++++/++++++++////+++ooo+++syyyysssyyyhdmmmdddddhhy+//////:
# ////////////////+osyhddho:::osshddddhhysyhhyyyyyhysosydmmmmddhy//////:
# //////////////+syhdmmmdh+/oyyhdddhhhhhdddhso+++syhhyo+/+shdmdddo/////:
# ///////+oyhhhhdmmmmmmhs//shhdhssssysooooo++/+ss//+oooso/::/+yddh/////:
# ///osyhmmmmmmmmmmmmho::/osyhyyssoossyyssssyhhy/:/shhhyysso+/:/shs///::
# /+hmddmmmmmmmmmmmds:-::++oys+//sys+//oyooshyyho+oydmdddddddyo+:+hho/::
# +smdmmmmmmmmmmmmh+---::++//+oydmmddsoohsoyhyyhyyyhdmdhhydddddyo/sso/::
# +ohmmmmmmmmmmmds:---::///shdmmdmmmddhhhssyyhhhhyyyyhdy+hdhddddy+/:::--
# ++odmmmmmmmmmd+------::ohddddddhhdmddhhhdddddddddhsssssydhddddh+/:----
# +++odmmmmmmmd+------:/ohhddddddhyyddhhdmdhdhhhdhdddhyssyhdddddho+:---.
# ++++ymmmmmmmh:-.--::+shhddmydhddhhysyhmdhhdmhhhddddddhhyyyddddyoo:....
# ++++odmmmmmdy-----:/shhhhddddddhhssshdmddhhdhdyhhhdddddddhhddhs++/....
# ++++++ydddddy----::+yddhhhhhhyyhsoshmddddhhhdhhhddddddddddddhhyo+/:...
# /++++++oshhhs-----:/shhyyhdddddyosdmddddhhhhhddhyyhhhddddddddhyys/:-..
# ++++++++++/o+--.-----/+sssyyyhyoydmmddddhhhhhhdmdddddddddddddddhyo::-.
# +++++++++/--:-..-+/..-:+osssyyssdmmdddddhhyhdddhyyhdddddddddddddhs//--
# /++++++++/---...-/:.-:+o++/+ssshmdddddddhhhddhhdddddddmdddddddddho//:-
# +++++++++/:::-..-:---/ooo+/+ysydmddddddddddmddddddddddmmmddddddds+/:::
# ++++++++:-...-....--:+yysoshhhdmmmmmddddddmdddddddddddmmmmmmmdddoo/:::
# +++++++/-..`..-...-:/+syyhdddmmddddddddddmmdddmmmmmdddmmmmmmmmdyo/::::
# ++++++/:-.....--.---:/oyhddddddhhdddddmmmmmddmmmmmmdddmmmmmdddyo//:::/
# /+++++/--.--...------:/ohddmmmdddddmmmmmmdmmmmmmmmmmddmmmmmddys+///:/:
# //////:....--...-:::::/+shhdddmmmmmmmmdmmmmmmmmmmmdmmmmmmmdhyo//:/::/:
# /////:.......--..-:::::/+shddmddmmmmdmmmmmmmmmmmmmmmmmmddhhso////:///:
# :///:.``.....---..-::::/+osyhdmmmmmmmmmmmmmmmmddmmmmmdyyyyo+////::/:::
# ::::-.```...--::-.------::/+syhdddddddmdddhyyyyhddmmhso+++/:://:::::::
# ::::.``````..-:::--..----::/+oossssyhddyo+:://oyhhys+:::/::::/::::::--
# :::-.```````....-:--...------:::::/oso/-----::/++//:--::::://::::---..
# :::-`````````....----....--....-------......--::::::/::::://::::--....
# -::-.````.......--:---......-......------:::/+++++//////:::::---......

dogs <- c("мопс", "пудель", "овчарка", "йорк", "мопс", "мопс")

f_dogs <- factor(dogs)
f_dogs

#уровни сортируются по алфавиту

levels(f_dogs)

str(f_dogs)

#допустим, мы хотим отсортировать уровни по-своему

f_dogs2 <- factor(dogs,
                  levels=c("овчарка", "мопс", "йорк", "пудель"))

f_dogs2
str(f_dogs2)
str(f_dogs)

#изменение уровней фактора
levels(f_dogs) <- c("йорк", "шарпей", "овчарка", "пудель") #меняем мопсов на шарпеев
f_dogs

#ordered factor (упорядоченный)
#допустим мы хотим знать, какая собака больше (чуть более реальный пример: размер одежды S-M-L и т.д. как фактор)

f_dogs[1] < f_dogs[2]

o_f_dogs <- factor(f_dogs, ordered = T, levels = c("йорк", "пудель", "шарпей", "овчарка"))
o_f_dogs

o_f_dogs[1] < o_f_dogs[2]


#ДАТЫ И ВРЕМЯ

#как задавать?
  
date1 <- as.Date("2019-07-24")
date1



#можно писать другие форматы, но к ним нужны пояснения
  
date2 <- as.Date("07/24/2019", format = "%m/%d/%Y")
date2



date3 <- as.Date("24.07.2019", format = "%d.%m.%Y")
date3


date4 <- as.Date("July 24, 2019", format = "%B %d, %Y")
date4


## вот тут есть список всех этих сокращений от даты и от времени 
# (например, большая M -- это минуты)
  
`?`(strptime)

#вычисления с датами


#вычитаем друг из друга
date5 <- as.Date("2019-12-31")
date5

date5 - date1


difftime(date5, date1, units = "weeks")

#можно просто прибавлять дни как числа
  
date1 + 12


#а еще можно сравнивать даты между собой

dates<-c("2019-07-20","2019-07-29","2019-08-20","2018-06-30","2019-02-29")

dates[dates<"2019-07-24"]

#системное время
Sys.Date()

dates[dates<Sys.Date()]



#попробуем вытащить время из нашего датасета

data <- read.csv("file_with_NA.csv", sep='}')

#там есть переменная "когда был первый секс"

firstsexdates<-as.data.frame(table(data$SPFSEX1))
firstsexdates

#создадим новую переменную в датасете, чтобы преобразовывать уже её
data$fs1<-data$SPFSEX1

#в начале преобразуем все "отказы отвечать" и т.п. в пропуски (NA)
data$fs1[data$fs1>5000]<-NA
#данные представлены в виде кол-ва месяцев с 1 января 1900 года
# вычисляем целочисленное частное и целочисленный остаток от деления на 12
data$fs1year <- data$fs1%/%12 #year
data$fs1month<-data$fs1 %% 12 #month

NA%/%4


#вносим поправку, касающуюся декабрей - они записаны как нулевые месяцы следующего года
# во-первых, отнимаем единицу от года

120%%12
120%/%12

data$fs1year[which(data$fs1month==0)]<-data$fs1year[which(data$fs1month==0)]-1


#во-вторых, меняем месяц на 12
data$fs1month[which(data$fs1month==0)]<-12

hist(data$fs1month,breaks = c(0:12))




#преобразуем месяц в текст
data$fs1month<-as.character(data$fs1month)

#теперь добавляем 0 в начало тем месяцам, в которых только один символ
data$fs1month[which(nchar(data$fs1month)==1)]<-paste0("0",data$fs1month[which(nchar(data$fs1month)==1)])

nchar("1975-05-01")

x<-c(1,2,3)
y<-c("a","b","c")

paste0(x,y)
paste(x,y,"12",y, sep="}")
paste(x,collapse="}")


#теперь соединяем результат с 19 и чёрточками 
data$fs1[!is.na(data$fs1month)]<-paste0("19",data$fs1year[!is.na(data$fs1month)],"-",data$fs1month[!is.na(data$fs1month)],"-01")  

head(data$fs1,50)


hist(as.Date(data$fs1),breaks=100)


hist(data$fs1year,breaks=c(44:93))

hist(data$AGE[data$AGE<90],breaks=c(15:68))




#вернёмся ненадолго к датафреймам и векторам:
#Есть три базовых способа фильтрации векторов и датафреймов: 
#1. с помощью вектора с адресами (номерами строк)
#2. с помощью вектора с именами
#3. С помощью логического вектора

data32_1<-data[1:32,]

data32_1<-data[c(1:10,15:24),c("BORN","fs1")]


data32_2<-data[c(rep(T,32),rep(F,3400)),]

#команда rep - очень удобная штука... 

rep(c(1:3),times=4)
rep(1:3,each=3)
rep(1:2,times=2,each=2)

#удобная команда seq

seq(0,20,1/5)



#Ну и наконец, команда sample

sample(1:nrow(data),10) #псевдослучайные числа, выбирает по функции


set.seed(100) #установить  начало последовательности для выбора типа случайных чисел
sample(c(1,3,4,5,6,7,899,5),6)


a <- seq(120, 468, 12)
b <- rep(7:9, each = 2)
b
c <- c('a', 'b','c')
paste0(a, b, c)

#а теперь центральная предельная теорема!
data[data$AGE>95,"AGE"]<-NA
means<-replicate(100000,mean(data[sample(1:nrow(data),200),"AGE"],na.rm=T))
hist(means,50)  


