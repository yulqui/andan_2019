#дополнительная пара с Машей

#задание на потренироваться: создайте вектор чисел от 5 по 14, умножьте его на два, разделите на три, потом сделайте большой вектор, в котором будет изначальный, умноженный на два, и изначальный, умноженный на три




a <- 5:14
#или
a <- seq(5, 14)
a2 <- a*2
a3 <- a/3
a4 <- c(a2, a3)

#создайте три последовательности чисел: все чётные числа больше одного и меньше сотни, первые тридцать положительных чётных чисел, и первые двадцать положительных чисел, кратных пяти, пусть это будут вектора a, b и c





a <- seq(2, 99, by = 2)
b <- seq(from = 2, by = 2, len = 30)
b2 <- seq(from = 2, to = 60, by = 2)
c <- seq(from = 5, by = 5, len = 20)
any(!(b == b2))

seq(from = 1, by = 2, len = length(a))
#это то же самое, что 
seq(from = 1, by = 2, along.with = a)

#выберите 5 случайных чисел из вектора а, 3 случайных числа из вектора b и 2 случайных числа из вектора c. числа внутри каждой случайной выборки не должны повторяться. создайте из них один большой вектор и перемешайте его. посчитайте среднее, сумму, минимум и максимум этого вектора. посчитайте, сколько в нём чётных и нечётных чисел.

a5 <- sample(a, 5, replace = F)
b3 <- sample(b, 3)
c2 <- sample(c, 2)

one_big_vector <- sample(c(a5, b3, c2))
sum_obv <- summary(one_big_vector)

sum_obv[c('Min.', 'Mean', 'Max.')]
sum(one_big_vector)


odd <- sum(one_big_vector %% 2)
even1 <- sum(!(one_big_vector %% 2))
even2 <- length(one_big_vector) - odd
even3 <- sum(one_big_vector %% 2 == 0)





#создайте вектор, в котором будет 100 случайных значений, взятых из нормального распределения со средним 42 и дисперсией 100. Постройте гистограмму этого распределения.
dist <- rnorm(100, 42, 10)
hist(dist, xlim = range(10:69))

min(dist)

#сделайте дата.фрейм, в котором будут такие столбцы: 1) имя испытуемого (и всех испытуемых зовут Маша, Саша и Даша, вообще всех, и все три имени примерно равновероятны)) 2)  возраст испытуемых (пусть им будет от 25 до 50 лет) 3) пол испытуемых (пусть некоторые Саши будут мальчиками) и 3) айди вида 1MASHA25, то есть порядковый номер, имя и возраст. всего у нас будет 100 испытуемых.

names <- sample(rep(c('Masha', 'Sasha', 'Dasha'), len = 100), replace = T)
age <- sample(seq(25, 50), size = 100, replace = T)
hist(age)
subj_df <- data.frame('name' = names, 'age' = age)
num_of_sashas <- nrow(subj_df[subj_df$name == 'Sasha',])
indices_sasha <- which(subj_df$name == 'Sasha')
random_number_of_male_sashas <- round(runif(1, min = 1, max = num_of_sashas))

final_male_sashas <- sort(sample(indices_sasha, random_number_of_male_sashas))
subj_df$gender <- 'Female'
subj_df$gender[final_male_sashas] <- 'Male'


subj_df$IDnum <- 1:100
vec <- paste0(subj_df$IDnum, subj_df$name, subj_df$age)
subj_df$ID <- vec

subj_df$IDnum <- NULL
