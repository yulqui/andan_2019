dat <- data.table(a = 1:10, b = 11:20, c = letters[1:10])
dat2 <- data.table(a = 11:20, b = 21:30, c = letters[1:10])
?merge
merge(dat, dat2) #не роботоет
merge(dat, dat2, by = c('c')) #роботоет
View(merge(dat, dat2, by.x = c('b'), by.y = c('a')))
dat <- data.table(a = c(1:5, 11:15), b = letters[1:10])
dat2 <- data.table(a = c(1:5, 21:25), b = letters[11:20])
View(merge(dat, dat2, by = 'a'))
library(reshape2)
?melt
dat <- data.table(name = c('Masha', 'Dasha', 'Sasha'), age = c(18:20), height = c(167:169), group = c('exp', 'control', 'exp'), RT = c(0.5, 0.3, 0.2))
View(dat)
dat_melt <- melt(dat, measure.vars = c('height', 'age', 'RT'), id.vars = c('name', 'group'))
View(dat_melt)
?dcast
dcast(dat_melt, name+group~variable)

save.image("F:/undone/meow.RData")
load.image("F:/undone/meow.RData")

dat_melt$newvalue <- c(168:170, 19:21, 0.4, 0.3, 0.2)
View(dat_melt)

dat3 <- rbind(dat_melt, dat_melt)

View(dcast(dat3, group~variable, value.var='value', fun.aggregate = c(mean, max)))

vec <- 1:10
#apply(X, MARGIN = 1 строчки 2 столбцы, FUN)
View(lapply(vec, as.character))
View(sapply(vec, as.character))
View(lapply(dat[, .(height, age, RT)], mean))
View(sapply(dat[, .(height, age, RT)], mean))
class(dat[, lapply(.SD, mean), .SDcol = c('height', 'age', 'RT')])
newdat <- dat[, sapply(.SD, as.character)]
class(newdat)

newdat <- dat[, lapply(.SD, as.character)]
class(newdat)
class(newdat$age)
newdat$age*2

newnewdat <- newdat[, lapply(.SD, as.numeric), .SDcol = c('height', 'age', 'RT')]
newnewdat$age+1


