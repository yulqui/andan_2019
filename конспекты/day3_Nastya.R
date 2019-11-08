setwd("C:/Users/serve/Desktop/андан/dataset")
library(data.table)
library(car)
data <- read.csv("file_with_NA.csv", sep='}')
options(scipen = 999)




"1 пара - КОДИМ С НАСТЕЙ КОТЛИКОВОЙ
Описательные статистики. Среднее, минимум, максимум, медиана, квартили. Расчёт процентилей. 
Plot, hist
Базовые статистические критерии
                Критерий Манна-Уитни
                Синтаксис выражения ""formula""
                t-критерий Стьюдента для независимых выборок
t-критерий Стьюдента для зависимых выборок
T-Вилкоксона для зависимых "










mean(data$AGE)
#мы немножко потрогаем ножкой воды семейства ф-ций apply:
sapply(data, mean, na.rm=TRUE)

















data[data$AGE>95,"AGE"]<-NA
data$AGE
max(data$AGE,na.rm=T)
min(data$AGE,na.rm=T)
mean(data$AGE,na.rm=T)
round(mean(data$AGE,na.rm=T),2)
#посмотрите наш датасет и попробуйте посчитать средние двух интересных вам категорий и их разницу












median(data$AGE, na.rm=T)
hist(data$AGE)



















summary(data$AGE)
summary_AGE <- summary(data$AGE)
names(summary_AGE)
summary_AGE$'Min.' #не работает! почему? потому что summary это atomic vector, а оператор $ с ними не работает
summary_AGE[1]













#fivenum() показывает Tukey's summary: минимум, 25th квартиль, медиану, 75th квартиль и максимум
fivenum(data$AGE)



















example_data <- data.frame(data$AGE, data$GENDER, data$FIRSTJOB)
psych::describeBy(example_data, "data.GENDER")
sapply(example_data, summary, na.rm=TRUE)
quantile(data$AGE, probs = seq(0, 1, 0.25))
#на всякий случай:
seq(0, 1, 0.25)

#попробуйте посчитать перцентили для возраста




















quantile(data$AGE, probs = seq(0, 1, 0.01), na.rm=T)













data <- data.table(data)
data[data$FIRSTJOB < 15 | data$FIRSTJOB > 30, 'FIRSTJOB'] <- NA
table(data$FIRSTJOB)












#давайте немножко посмотрим на наши данные. как это можно сделать?
head(data$GENDER)
tail(data$GENDER)
plot(data$GENDER, data$P18)
plot(data$AGE, data$FIRSTJOB)
men <- data[GENDER == 1]
women <- data[GENDER == 2]


















#для начала давайте проверим, верны ли наши предположения о среднем количестве партнёров у мужчин!
#здесь ваша догадка

n = NA
t.test(men$P18, mu = n)
summary(women$P18)
summary(men$P18)













#а теперь давайте проверим, одинаковое ли количество сексуальных партнёров у мужчин и женщин
t.test(men$P18, women$P18)
#но мы не проверили данные на равность дисперсий, давайте это сделаем
var.test(men$P18, women$P18)
#значит, просто уточняем, что дисперсии не равны?
t.test(men$P18, women$P18, var.equal = F)
















#но нет! функция t.test() в R по умолчанию использует поправку Уэлча о том, что дисперсии не равны. на самом деле мы забыли проверить оба вектора на нормальность распределения!
shapiro.test(women$P18)
qqnorm(men$P18)
car::qqPlot(women$P18, distribution = 'norm')
hist(men$P18)
hist(women$P18)
#так что надо использовать непараметрический t-тест Манна-Уитни (для независимых выборок)
wilcox.test(men$P18, women$P18)










#что можно сделать с результатом статистического теста?
wilcox_men_women_p18 <- wilcox.test(men$P18, women$P18)
class(wilcox_men_women_p18)
typeof(wilcox_men_women_p18)
names(wilcox_men_women_p18)
wilcox_men_women_p18$statistic
class(wilcox_men_women_p18$statistic)
wilcox_men_women_p18$method
class(wilcox_men_women_p18$method)
typeof(wilcox_men_women_p18$method)





#а что, если у нас зависимые выборки? в датасете такие данные найти сложно, давайте их сгенерируем. например, давайте сгенерируем ваше настроение вчера и сегодня.

time <- c(rep('Y', times = 8), rep('T', times = 8))
mood <- c(rnorm(mean = 3, sd = 2, n = 8), rnorm(mean = 4, sd = 2, n = 8))

t.test(mood ~ time, paired = T)



#тест Вилкоксона (выборка маленькая, и если бы мы не знали, что взяли ее из норм. распределения, мы не могли бы сказать этого наверняка -- выборка очень маленькая): 

hist(mood[1:8])
hist(mood[9:16])

wilcox.test(mood ~ time, paired=T)

#а что это за формат? mood ~ time? это Ф О Р М У Л А ! ! ! можно было бы сделать вот так:

moodtimedata <- cbind(time, mood)
moodtimedata <- data.table(moodtimedata)

t.test(as.numeric(moodtimedata[time == 'Y']$mood), as.numeric(moodtimedata[time == 'T']$mood), paired = T)









#давайте разберёмся, что мы тут делаем:

moodtimedata[time == 'Y']
moodtimedata[time == 'Y']$mood
class(moodtimedata[time == 'Y']$mood)
as.numeric(moodtimedata[time == 'Y']$mood)









#штуки с тильдой (~) это формулы. у них есть доп.операторы: * и +, про них мы поговорим на паре про линейные регрессии.


a <- mood ~ time
class(a)
#oh wow
t.test(a, paired = T)




#G-критерий знаков
#загрузим данные про давление до и после применения лекарства
before <- c(171.2, 169.8, 154.6, 130.9, 158.5, 145.5, 143.5, 144.7, 147.7, 160.7, 154.7, 181.8, 167.2, 137.4, 180.2, 138.7, 159.9, 141.8, 172.2, 167.0, 
            137.2, 170.9, 168.4, 163.7, 160.1, 163.5, 146.7, 173.9, 180.1, 136.0, 159.0, 145.6, 186.5, 177.7, 167.7, 167.4, 165.9, 147.2, 165.2, 133.3, 
            175.0, 174.7, 163.0, 154.1, 189.4, 166.5, 153.0, 134.3, 177.1, 150.4, 152.4, 176.2, 160.3, 135.3, 131.2, 172.1, 137.0, 156.6, 178.5, 168.1)
after <- c(179.5, 141.9, 124.7, 103.2, 143.1, 146.0, 132.2, 104.9, 145.3, 123.5, 135.2, 176.2, 142.7, 114.1, 171.9, 115.0, 126.4, 108.0, 171.7, 148.8,
           103.5, 178.5, 138.9, 150.0, 131.8, 169.2, 131.4, 138.8, 146.2, 116.1, 148.8, 109.2, 186.3, 164.1, 147.3, 165.3, 140.0, 122.6, 174.4, 104.6,
           156.6, 175.3, 126.8, 122.6, 184.0, 139.6, 149.4, 105.3, 181.9, 134.6, 129.4, 148.0, 170.2, 144.2, 133.3, 171.8, 118.4, 131.2, 150.0, 131.0)
boxplot(before, after, col = c(6,5), 
        main = "The effect of treatment")
axis(1, at=1:2, labels=c("before","after"))


difference <- before - after
difference
length(difference)
length(difference[difference>0])

binom.test(50, 60)

# install.packages('DescTools')
# library(DescTools)
# 
# SignTest(x = before,
#          y = after)
