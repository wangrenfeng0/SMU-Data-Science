fired = c(34,37, 37, 38, 41, 42, 43, 44, 44, 45, 45, 45, 46, 48, 49, 53, 53, 54, 54, 55, 56)
notfired = c(27, 33, 36, 37, 38, 38, 39, 42, 42, 43, 43, 44, 44, 44, 45, 45, 45, 45, 46, 46, 47, 47, 48, 48, 49, 49, 51, 51, 52, 54)
#hist(fired)
#hist(notfired)
library(tidyverse)

obersved_diff=mean(fired)-mean(notfired)
obersved_diff
fired=data.frame(Value=fired, Status='Fired')
notfired=data.frame(Value=notfired, Status='Notfired')

employee=rbind(fired,notfired)
employee
xbarDiffHolder = numeric(10000)
for (i in 1:10000)
{
  scramble_employee=sample(employee$Status, 51)
  employeeTemp=employee
  employeeTemp$Status=scramble_employee
  
  xbars=employeeTemp %>% group_by(Status) %>% summarize(mean=mean(Value))
  xbars
  xbarNminusT=xbars[2,2]-xbars[1,2]
  xbarNminusT
  xbarDiffHolder[i]=xbarNminusT$mean
}

ggplot(mapping=aes(x=xbarDiffHolder), color='black')+geom_histogram(bins=25, color='black', fill='blue')

num_more_extreme = sum((xbarDiffHolder) >= 1.92381)
num_more_extreme

pvalue = num_more_extreme / 10000
pvalue

Fired = c(34, 37, 37, 38, 41, 42, 43, 44, 44, 45, 45, 45, 46, 48, 49, 53, 53, 54, 54, 55, 56)
Not_fired = c(27, 33, 36, 37, 38, 38, 39, 42, 42, 43, 43, 44, 44, 44, 45, 45, 45, 45, 46, 46, 47, 47, 48, 48, 49, 49, 51, 51, 52, 54)
t.test(x = Fired, y = Not_fired, conf.int = .95, var.equal = TRUE, alternative = "two.sided")
