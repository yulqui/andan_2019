a <-  c(seq(5, 14, 1))
a
typeof(a)
b <-  a*2
b
c <-  b/3
c
final_vector <- c(a, b, c)
final_vector
#final_vector2 <- 

a <- seq(2, 99, 2)
a
b2 <- seq(from=2, by=2, length.out=30)
b
c <- seq(5, 100, by=5)
c2 <- seq(from=5, by=5, len=20)
c2
length(c)
c


sample_a <- sample(a, 5)
sample_a

sample_b <- sample(b, 3)
sample_b

sample_c <- sample(c, 2)
sample_c

resuming_vector_3 <- c(sample_a, sample_b, sample_c)
sort(resuming_vector_3)
one_big_vector <- sample(c(sample_a, sample_b, sample_c))
one_big_vector
mean(resuming_vector_3)
sum(resuming_vector_3)
mean(resuming_vector_3)
max(resuming_vector_3)
summary(one_big_vector)[c('Min.', 'Mean', 'Min.')]
sum((resuming_vector_3 %% 2) == 0)
sum(!((resuming_vector_3 %% 2) == 0))


sample_norm <- rnorm(100, 42, 100)
sample_norm
hist(sample_norm)


names <- c('Masha', 'Sasha', 'Dasha')
DFNAMES <- sample(names, 100, replace=T)
genders <- c('M', 'F')
DFAGE <- sample(25:50, length(DFNAMES), replace=T)
generated_df <- data.frame(DFNAMES, DFAGE)
generated_df$sex<-NA
generated_df$sex[(generated_df$DFNAMES == 'Dasha') | (generated_df$DFNAMES == 'Masha')] <- 'F'
View(generated_df)
num_of_Sasha <-  sum(generated_df$DFNAMES == 'Sasha')
generated_df$sex[(generated_df$DFNAMES == 'Sasha')] <- sample(genders, sample(1, num_of_Sasha, 1), replace=T)
generated_df$DF_ID<-NA
vec <- c(1:100)
generated_df$DF_ID <- paste0(as.character(vec), generated_df$DFNAMES, generated_df$DFAGE)
