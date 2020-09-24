SMU = c(34, 1200, 23, 50, 60, 50, 0, 0, 30, 89, 0, 300, 400, 20, 10, 0)
Seattle = c(20, 10, 5, 0, 30, 50, 0, 100, 110, 0, 40, 10, 3, 0)
hist(SMU)
hist(Seattle)
library(tidyverse)

obersved_diff=mean(SMU)-mean(Seattle)
obersved_diff
cash1=data.frame(Value=SMU, School='SMU')
cash2=data.frame(Value=Seattle, School='Seattle')
cash1
cash2
cash=rbind(cash1,cash2)
cash
xbarDiffHolder = numeric(10000)
for (i in 1:10000)
{
scramble_schools=sample(cash$School, 30)
cashTemp=cash
cashTemp$School=scramble_schools

xbars=cashTemp %>% group_by(School) %>% summarize(mean=mean(Value))
xbars
xbarNminusT=xbars[2,2]-xbars[1,2]
xbarNminusT
xbarDiffHolder[i]=xbarNminusT$mean
}

ggplot(mapping=aes(x=xbarDiffHolder), color='black')+geom_histogram(bins=25, color='black', fill='blue')

num_more_extreme = sum((abs(xbarDiffHolder)) >= 114.625)
num_more_extreme

pvalue = num_more_extreme / 10000
pvalue