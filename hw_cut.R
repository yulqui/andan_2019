#домашечка!
# 1) освоить команду quantile. 
# 2) написать функцию, которая принимает на вход числовой вектор, вычисляет 25 и 75 процентиль и делит по ним этот вектор, возвращая 25% "lo", 50% " middle", 25% "hi". 
# Назовите функцию "to_three_levels". 
# to_three_levels(1:8) должно возвращать вектор c("lo","lo","middle","middle","middle","middle", "hi","hi")


a<-c(1:8)
cut_data<-data.table(a)
quantile(a)
quantile(a, probs = c(0.05, 0.95))
quantile(a, probs = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1))
cut_data <- cut_data[, quant:=cut(x=cut_data$a, breaks=c(min(cut_data$a)-1, quantile(cut_data$a, probs = c(0.25, 0.75)), max(cut_data$a)+1), labels = c("lo","middle","hi"))]
View(cut_data)

cut_data[,int7:=cut(x = a,breaks = c(min(a)-1,5,10,max(a)+1),labels = c("низко","средне","высоко"))]


to_three_levels <- function(vec) {
  cut(x=vec, breaks=c(min(vec)-1, quantile(vec, probs = c(0.25, 0.75)), max(vec)+1), labels = c("lo","middle","hi"))
}

to_three_levels(1:15)

#можно использовать для анализа контрастных групп (выкидываем middle)
#грубая нелинейная нормализация (раномеризация), в т.ч. избавляемся от выбросов