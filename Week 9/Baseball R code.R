baseball=read.csv(file.choose(),header=T)
fit_baseball=lm(Wins~Payroll,data=baseball)
summary(fit_baseball)
confint(fit_baseball)
winpredict=predict(fit_baseball)
sum((winpredict-baseball$Wins)^2)

win_100 = predict(fit_baseball, data.frame(Payroll=100),interval='confidence')
win_100

win_100_predict = predict(fit_baseball, data.frame(Payroll=100),interval='prediction')
win_100_predict