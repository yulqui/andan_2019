#FUNCTIONS
#name_of_function <- function(arguments)command1;command2 (preferrably only one command)

#name_of_func <- function
#  {command1
#   ...
#   commandN}

hw <- function() 'hello world'
hw()
hw <- 'hello will'
hw2 <-  function(name){
  paste0('Hello, ', name)
}
hw3 <- function(greet='Hello', name='world')
{
  paste0(greet, ", ", name)
}
hw3('HI', 'MIKE')
hw3(name='Mike', 'Hi')
hw4 <- function(get_number) paste((get_number-1)^2)
hw4(3)
fnames <- c('Петя', 'Петя', 'Вася')
search1 <- 'Вася'

amount <- function(v, value) {
  if (search1 %in% fnames) {
  paste(sum(v == value))
  } else {paste('Item not found')}
}

amount2 <- function(v, value) {length(v[v==value])}
amount()
amount2(dataV[,7], 'DK')

perc <- function(v, value) {
  num_of_search_items = amount2(v, value)
  num_of_total_items = length(v)
  return((num_of_search_items/num_of_total_items)*100)
}
perc()

perc2 <- function(v, value, dec) {
    paste0(round((perc(v, value)), dec), '%')
}
perc2(fnames, search1, dec=2)


