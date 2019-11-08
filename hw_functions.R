#1
#написать функцию, которая все положительные числа возводит в квадрат, а отрицательные - в куб. При поступлении на вход не числовых данных функция должна выдавать осмысленную ошибку stop ("input must be numeric!")
raise_to_second_third_power <- function(number) {
  if (is.numeric(number)) sapply(number, function(number) {if (number>=0) number^2 else number^3})
  else warning(stop("input must be numeric!"))
}
  
#оператор if -не векторизованная! есть функция ifelse для векторов

kubkv3 <- function(x)  {
  if (!is.numeric(number)) warning(stop("input must be numeric!"))
  ifelse (test=x>0, yes=x^2, no=x^3)
}
kubkv3(c(1, 43, -25.5, 0, -100))

# raise_to_second_third_power(c(1, 43, -25.5, 0, -100))
# typeof(c(1, 43, -25.5, 0, -100))

#2
#написать функцию, которая принимает на вход два числовых вектора (проверяет, что они числовые), создаёт из них data.frame (или data.table), а потом рисует по ним scatterplot (как было на занятии)

remove_trash <-  function(x) {
  if(length(x)==0) {
    warning('empty vector')
    return(x)
  }
  x[x=='DK'] <- NA
  x[x=='Refusal'] <- NA
  x[x=='Missing'] <- NA
  x[x=='Missing'] <- NA
  #print('the end')
  return(x)
}

vector_to_scatter_transformation <- function(vector1, vector2) {
  if (is.numeric(vector1) & is.numeric(vector2)) {
    used_data <- data.table(vector1, vector2)
    ggplot(data=used_data, aes(x=vector1, y=vector2)) + geom_point()
  }
  else warning(stop("input must be numeric!"))
}

library(ggplot2)

vector_to_scatter_transformation(a, b)
a <- c(rnorm(200, mean=2, sd=2))
b <- c(rnorm(100, mean=1.7, sd=1))
grep("APPEAL1", colnames(dataV))
grep("APPEAL2", colnames(dataV))





#HW
# 1) Посчитайте с помощью data.table описательные статистики для женщин по переменным APPEAL (средние, медианы, стандартные отклонения) в зависимости от религиозной принадлежности. Базовый уровень сложности — посчитать всё это для одной из религиозных принадлежности, со звёздочкой — по всем.
# 
# 2) Создайтк переменную x, которая будет содержать данные, составляющие следующий ряд (заполнить пропущенное):
#   
#   "10a","15b","20c","25a","30b", ..., "200c"
# 
# Проверка: 
#   
#   sum(strtoi(x,16))==138141
# 
# 3) Создайте переменную, которая будет содержать данные, составляющие следующий ряд (заполнить пропущенное):
#   
#   "1207a" "1327b" "1448c" "1568a" "1689b" "1809c" "1927a" "2047b" "2168c" "2288a" "2409b" ... "4689c"  
# 
# Проверка: 
#   
#   sum(strtoi(x,16))==5417034
means <- dataV[,sapply(.SD, function(x) mean(as.numeric(remove_trash(x)), na.rm = T)), .SDcol = appeal_vars]
sd <- dataV[,sapply(.SD, function(x) sd(as.numeric(remove_trash(x)), na.rm = T)), .SDcol = appeal_vars]
result <- data.table(appeal_vars,means,sd)
