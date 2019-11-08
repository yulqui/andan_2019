#итак, дата.тейбл!
library(data.table)
#производительность, лаконичность, код легко читать (но это не точно)

dat <- fread("06647-0001-Data.tsv")

#и никаких сепараторов! (фрид умный)

#dat[i, j, by], или dat[фильтр строк, выражение, параметр]. Take dat, subset/reorder rows using i, then calculate j, grouped by by. начнём с фильтра строк.
View(dat[JAIL == 1])
View(dat[1:10])
nrow(dat[JAIL == 1 & !(ATTEND %in% c(0, 1, 9))])

#выражение
dat[JAIL == 1 & !(ATTEND %in% c(0, 1, 9)), .N] #number of rows
dat[JAIL == 1 & !(ATTEND %in% c(0, 1, 9)), mean(AGE)]
dat[JAIL == 1 & !(ATTEND %in% c(0, 1, 9)), summary(AGE)]
#припис(ь)ка! := creates new column and a name
dat[JAIL == 1 & !(ATTEND %in% c(0, 1, 9)), status := 'religious prisoner']
View(dat[, status]) #выдаёт, заметим, вектор, а не маленький дата.тейбл, как дата.фрейм
#list() == .() сюрприз!
View(dat[, .(JAIL, status, AGE)])
View(dat[JAIL == 1, .(JAIL, status, AGE)])
View(dat[, CASEID:BORN])
dat[JAIL == 1 & (ATTEND %in% c(0, 1, 9)), status := 'non-religious prisoner']
dat[JAIL == 2 & !(ATTEND %in% c(0, 1, 9)), status := 'religious non-prisoner']
dat[JAIL == 2 & (ATTEND %in% c(0, 1)), status := 'non-religious non-prisoner']
dat[JAIL %in% 6:9 | ATTEND == 9, status:= 'unknown' ]

#добавьте в колонку status тэги "non-religious prisoner", 'religious non-prisoner', 'non-religious non-prisoner' и 'unknown'

table(dat[, status])
dat[, .N, by=status]

#а теперь параметр
dat[,mean(AGE), by = status]
dat[, .(mean(AGE), max(AGE)), by = status]

dat[REDLGHT4 %in% 6:9, .(mean_age = mean(AGE), .N), by = .(status, GENDER)]


#давайте почистим переменные MJNUMB, JAIL и ATTEND
dat[MJNUMB >= 996, MJNUMB := NA]
dat[ATTEND %in% c(0, 1, 9) , ATTEND := NA]
dat[JAIL %in% 6:9, JAIL := NA]

#и теперь посмотрим, как они всяко меняются
dat_no_NA <- dat[!is.na(ATTEND)& !is.na(JAIL), .N, by = .(JAIL, ATTEND)]
dat_no_NA[JAIL == 1, JAILlabel := 'prisoner']
dat_no_NA[JAIL == 2, JAILlabel := 'non-prisoner']
dat_no_NA[ATTEND %in% c(1, 2, 3), ATTENDlabel := 'non-religious']
dat_no_NA[!(ATTEND %in% c(1, 2, 3)), ATTENDlabel := 'religious']
dat_no_NA[, label := paste(ATTENDlabel, JAILlabel, sep = '\n')]
jail <- dat_no_NA[, .N, by = label]
barplot(N ~ label, data = jail, cex.names = .7)

#домашка: сравните частоту хождения в церковь среди людей, которые сидели и не сидели в тюрьме.

sex_and_religion <- dat[, mean(MJNUMB, na.rm = T), by = ATTEND]

barplot(V1 ~ ATTEND, data = sex_and_religion)
#у людей, которые почти каждую неделю ходят в церковь, больше всего партнёров! И Н Т Е Р Е С Н О

#а постройте теперь такой же график количества партнёров в зависимости от того, был человек в тюрьме или нет!

#надоели эти колонки, давайте их кикнем из датасета :))
new_data <- dat[, !c(JAIL, ATTEND, MJNUMB)]
#шучу, шучу. но так можно!

#а теперь я предлагаю вам выбрать две бинарных переменных, почистить их и сравнить распределение возраста с нормальным (любой функцией) для каждого сочетания этих переменных.




shap <- dat[, shapiro.test(AGE), by = .(JAIL, GENDER)]
View(shap)
#ПРАВДА КРУТО???

#а ещё by принимает не только переменные, но и ПАРАМЕТРЫ. Смотрите!

dat[!is.na(ATTEND) & !is.na(AGE), .N, by = .(AGE > 40, ATTEND == 6)]
