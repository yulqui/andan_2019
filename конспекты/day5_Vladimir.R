#CHECK HOMEWORK

data <- c('0', '-1', '5', '6', '99')
numeric_data <- as.numeric(data)
factor_data <- factor(data)
sum(as.numeric(factor_data)) #DO NOT TURN STRINGASFACTOR INTO NUMERIC
sum(numeric_data)

#sapply(набор элементов, функция для каждого элемента[, дополнительные аргументы для функции])


sapply(c('Vasya', 'Petya'), paste0, ', you\'re a fool')

sapply(c('Vasya', 'Petya'), paste, ' you\'re a fool', sep=', wow,')

amount <- function(vector, item) length(vector[vector==item])
perc <- function(vector, item) amount(vector, item)/length(vector)
sapply(attend_types, perc, dataV$ATTEND)
sapply(attend_types, perc, v=dataV$ATTEND)

perc(dataV$ATTEND, 'Never')
perc('Never', dataV$ATTEND)

attend_types <- unique(dataV$ATTEND)
attend_types <- as.character(attend_types)
#fread по умолчанию импортирует их как строки, а не как факторы
attend_types <- names(table(data$ATTEND))#это нам сказали, как можно его еще вызвать
#attend_types <- relig[,'ATTEND'] это был наш вариант

#offtop
#не нужно пароли держать в исполняемом файле, который ты потом подгружаешь и отправляешь куда-нибудь (типа в продакшен), лучше их в отдельный файл засунуть и, если нужно, его вызывать

#character лучше чем factor объяснение:
data_ex <- c("0","-1","5","6","99")#создали текстовый вектор
factor_data <- factor(data_ex)#записали его фактором
sum(as.numeric(factor_data))#перевели его в числа b и посчитали сумму, так, что он посчитал сумму уровней, а не значений. Поэтому аккуратней с факторами, лучше работать со строками, они такого эффекта не дают и считают сумму как положено
levels(factor_data)#он посчитал количество уровней

default.stringsAsFactors(0)#делаем так, чтобы он не воспринимал строки как факторы


#NEW INFO
#see 6.lapply
