a = read.csv(file.choose(),header = TRUE)
# count missing values in each column
s = sapply(a, function(x) sum(is.na(x)))
# Make all missing values NA
str(a)
a$chars[6] = NA
str(a)
a$chars = as.character(a$chars)
str(a)
library(naniar)
gg_miss_var(a)
#Fifa
gg_miss_var(fifa[,1:40]) + ylim(0,75)
gg_miss_var(fifa[,41:89]) + ylim(0,75)
#mpg
gg_miss_var(mpg) + ylim(0,10)
sapply(mpg, function(x) sum(is.na(x)))
#nycflights13
library(nycflights13)
gg_miss_var(flights)
sapply(flights, function(x) sum(is.na(x)))