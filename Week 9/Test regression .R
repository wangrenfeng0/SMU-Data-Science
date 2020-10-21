test=read.csv(file.choose(),header=T)
fit_test=lm(math~science,data=test)
summary(fit_test)
confint(fit_test, level=.99)