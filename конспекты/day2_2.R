#ЛЕКЦИЯ

# номинативные данные
# Частотные таблицы. Гистограмма частот.
# Таблицы сопряжённости. Команда prop.table. 
# Критерий Хи-квадрат. Точный критерий Фишера.

#чтение файла csv, разделенного символом '}'

df <- read.csv("file_with_NA.csv", sep='}')
View(df)

#выберем несколько номинативных переменных из датасета
#сделаем их факторами и присвоим осмысленные значения
#levels & labels

#переменная BORN: родился ли R в США
#посмотрим на все возможные значения

levels(factor(df$BORN))
table(df$BORN)

#оставим только 1 и 2 (7 - отказ, 9 - пропущенное значение)

df <- subset(df, BORN %in% 1:2) #выбираем только тех, кто ответил 1 или 2

df <- subset(df, BORN %in% c(1,2)) #делаем то же самое

df$BORN <- factor(df$BORN)

levels(df$BORN)

sum(df$BORN)

levels(df$BORN) <- c('Born_USA', 'Born_smwr_else')

summary(df$BORN)

#особенности факторных переменных
max(df$BORN)
mean(df$BORN)
median(df$BORN)

levels(df$BORN)

#переменная MAWORK - работала ли мать Respondent за деньги, когда ему было 14

table(df$MAWORK)
df <- subset(df, MAWORK %in% 1:2)
df$MAWORK <- factor(df$MAWORK)
levels(df$MAWORK) <- c('M_worked', 'M_didnt_work')

#ABANY: аборт должен быть разрешен, вне зависимости от причины

table(df$ABANY)
df <- subset(df, ABANY %in% 1:2)
df$ABANY <- factor(df$ABANY)
levels(df$ABANY) <- c('Abort_OK', 'Abort_N_OK')

#RELIG: религия R в настоящий момент (None, Protestant,
#Catholic, Jewish, Orthodox etc.)
table(df$RELIG)
#оставим значения 0:4, соответствующие этим пяти вариантам
df <- subset(df, RELIG %in% 0:4)
df$RELIG <- factor(df$RELIG)
levels(df$RELIG) <- c('None', 'Protestant', 'Catholic', 'Jewish', 'Orthodox')

#JAIL: проводил ли хотя бы ночь в заключении

table(df$JAIL)
df <- subset(df, JAIL %in% 1:2)
df$JAIL <- factor(df$JAIL)
levels(df$JAIL) <- c('jailed', 'n_jailed')

# HOMOSEX

table(df$HOMOSEX) #отношение к гомосексуальности и сексу с тем же полом
df <- subset(df, HOMOSEX %in% 1:4)
df$HOMOSEX <- factor(df$HOMOSEX)
levels(df$HOMOSEX) <- c('never_OK', 'alm_never_OK', 'smtimes_OK', 'alw_OK')

#частотная таблица
t1 <- table(df$RELIG)
t1

dim(t1)


#таблица сопряженности
t2 <- table(df$RELIG, df$JAIL)
t2

dim(t2)

t3 <- table(df$RELIG, df$JAIL, df$ABANY)
t3

dim(t3)


## более удобное представление 3-мерных таблиц
ftable(t3)
prop.table(ftable(t3)) #показывает проценты, 100% = вся выборка
prop.table(ftable(t3),1) #100% в кажной строке
prop.table(ftable(t3),2) #100% в каждом столбце


#назовем элементы таблицы

t_rel_j <- table('Религия' = df$RELIG, 
                 'Тюрьма' = df$JAIL)
t_rel_j


# таблица с долями
prop.table(t2)
round(prop.table(t2), 2)*100

# не по всей таблице
#100% по столбцам
prop.table(t2,2)
#100% по строкам
prop.table(t2,1)

#округлим и переведем в проценты
pt2 <- prop.table(t2,2)
rpt2 <- round(pt2, 2)
RPC <- rpt2 * 100 # проценты
RPC

# сабсеттинг таблиц 
t1
t1[2]
t2
t2[3,2]
t3
t3[,2,]
t3[2,1,1]


t_j_ab <- table('prison' = df$JAIL, 
                'abortion' = df$ABANY)
t_j_ab

#по всей таблице
prop.table(t_j_ab)
#по строкам
prop.table(t_j_ab,1)
#по столбцам
prop.table(t_j_ab,2)


#BARPLOT
#(столбиковая диаграмма)
#порядок действий:
#делаем из категориальной переменной частотную таблицу =>
#строим barplot
barplot(table(df$RELIG))
barplot(t1)

#у нас уже есть частотные таблицы некоторых пар переменных

barplot(t_j_ab)
barplot(t_rel_j)

t_rel_j
#не очень осмысленно, давайте транспонируем
barplot(t(t_rel_j))

#добавим легенду
barplot(t(t_rel_j), legend.text = TRUE, 
        args.legend = list(x = 'topright'))
#разделим каждый столбик на два для более удобного сравнения
barplot(t(t_rel_j), legend.text = T, 
        args.legend = list(x = 'topright'), 
        beside = T)

#можем посмотреть на процентное соотношение
t(t_rel_j)
barplot(prop.table(t(t_rel_j),2)*100,legend.text = T, 
        args.legend = list(x = 'topright'), 
        beside = T)






#мозаичный график
mosaicplot(t_rel_j)


mosaicplot(t3)







#критерий Хи-квадрат

#для одномерной величины представляет собой 
#биномиальный тест 
#на отличие выборочного распределения от 
#заданного теоретического
table(df$JAIL)
options(scipen=999)
chisq.test(table(df$JAIL))





table(df$MAWORK)
chisq.test(table(df$MAWORK))









#запишем результаты теста в переменную
chi <- chisq.test(table(df$JAIL))
chi$exp #expected values
chi$obs #observed values










#хи-квадрат можно использовать для таблиц бОльших 
#размерностей
#сделаем таблицу отношение к гомосексуальности --
# -- отношение к абортам
t_homo_ab <- table('homosexuality' = df$HOMOSEX, 
                   'abortions' = df$ABANY)
t_homo_ab









chisq.test(t_homo_ab)





chisq.test(t_homo_ab)$exp
chisq.test(t_homo_ab)$obs









#Точный тест Фишера
#гипергеометрическое распределение, более сложная формула
#используется когда в какой-то из ячеек мало или совсем нет наблюдений 
#и тогда chi квадрат ломается

#p-value - вероятность получения данной таблицы 
#сопряженности на имеющихся данных 

#посмотрим еще раз на таблицу сопряженности 
#"тюрьма -- аборты"
t_j_ab

fisher.test(t_j_ab)











#теперь сделаем таблицу сопряженности 
#"работа матери -- аборты"

t_m_ab <- table("mother worked" = df$MAWORK, 
                "abortion" = df$ABANY)
t_m_ab



fisher.test(t_m_ab)






#пример где ломается хи-квадрат 
#(и дефолтный фишер тоже ломается)
t_rel_ab <- table("Религия" = df$RELIG, 
                  "Аборты" = df$ABANY)



t_rel_ab


chisq.test(t_rel_ab)








fisher.test(t_rel_ab) #памяти не хватает



fisher.test(t_rel_ab, workspace=2e6)


