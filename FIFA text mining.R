library(ggthemes) 
library(plotly)
library(magrittr)
library(ggplot2)
library(tidyr)
library(maps)
library(dplyr)
library(mapproj)
library(stringr)
library(naniar)
library(GGally)
fifa=read.csv(file.choose(), header=TRUE)
head(fifa)
str(fifa)

fifa$Weight=str_remove_all(fifa$Weight,'lbs')

sum(str_count(fifa$Weight, 'lbs'))

fifa$Heightstr=str_split(fifa$Height,"\\'")
for (i in 1:nrow(fifa)) 
{
  fifa$Heightinch[i]=as.numeric(fifa$Heightstr[[i]][1])*12+as.numeric(fifa$Heightstr[[i]][2])
}

fifa$Heightinch=unlist(fifa$Heightinch)
  
fifa$Weight=as.numeric(fifa$Weight)

fifa %>% ggplot(aes(x=Weight, y=Heightinch, color=Position)) + geom_point()+facet_wrap(~Position)

gg_miss_var(fifa)
s = sapply(fifa$Weight, function(x) sum(is.na(x)))

fifa %>% filter(Position=='LB'| Position=='LM') %>% ggplot(aes(x=Weight, y=Heightinch, color=Position))+geom_point()+facet_wrap(~Position)
fifa %>% filter(Position=='LB'| Position=='LM') %>% ggplot(aes(x=Weight, fill=Position))+geom_histogram()+facet_wrap(~Position)
fifa %>% filter(Position=='LB'| Position=='LM') %>% ggplot(aes(x=Heightinch, fill=Position))+geom_histogram()+facet_wrap(~Position)





df=read.table(file.choose(), sep=';', col.names=c('name', 'gender','numbers'))

head(df)
str(df)
summary(df)

str_count(df$name, 'yyy$')

df %>% filter(str_detect(df$name, 'yyy$'))

y2016=df %>% filter(!str_detect(df$name, "yyy$"))
str(y2016)


y2015=read.table(file.choose(), sep=',', col.names=c('name', 'gender','numbers'))

tail(y2015,10)
str(y2015)
summary(y2015)

final=merge(y2016,y2015,by=c('name', 'gender'),all=FALSE)
final
gg_miss_var(final)

str(final)

total_number=final %>% mutate(total=numbers.x+numbers.y) %>% arrange(desc(total))

girl_name=total_number %>% filter(gender=='F')

boy_name=total_number %>% filter(gender=='M')

girl_name_total=girl_name %>% select('name','total')
boy_name_total_ten=head(boy_name, 10) %>% select('name','total','gender')
girl_name_total_gender=girl_name %>% select('name','total','gender')

write_as_csv(head(girl_name_total, 10), file_name="Top ten girls' names") 

girl_name_total_ten=head(girl_name_total, 10)
girl_name_total_ten %>% ggplot(aes(x=name, y=total))+geom_point()+ggtitle('Top ten girls name')+xlab('Name')+ylab('Total')

girl_name_total_ten_gender=head(girl_name_total_gender,10)
top_ten_names=rbind(boy_name_total_ten, girl_name_total_ten_gender)
top_ten_names %>% ggplot(aes(x=name, y=total, fill=gender),binwidth=100)+ geom_bar(stat='identity')



fifa %>% select(Finishing, BallControl, ShotPower) %>% ggpairs()
fifa_LMLF=fifa %>% filter(Position=='LM' | Position=='LF')

fifa %>% filter(Preferred.Foot == "Right" | Preferred.Foot == "Left") %>% 
  select(Finishing, BallControl, ShotPower, Preferred.Foot) %>% ggpairs(aes(color = Preferred.Foot))

fifa_LMLF %>% select(Acceleration, Agility, Position) %>% ggpairs(aes(color=as.factor(Position)))

fifa %>% select(Finishing, BallControl, ShotPower) %>% ggpairs()
fifa_LF=fifa %>% filter(Position=='LF')
mean(fifa_LF$Agility)
fifa_LM=fifa %>% filter(Position=='LM')
mean(fifa_LM$Agility)
gg_miss_var(fifa_LMLF)
fifa_ttest=fifa_LMLF %>% select(Agility, Position)
fifa_ttest

qt(0.975,1108)  #critical t value

nrow(fifa_LM)
sd(fifa_LM$Agility)
sd(fifa_LF$Agility)
t.test(fifa_ttest %>% filter(Position=='LM') %>% select(Agility), fifa_ttest %>% filter(Position=='LF') %>% select(Agility), alternative='two.sided')

fifa_ttest %>% ggplot(aes(x=Agility, fill=Position))+geom_histogram()+facet_wrap(~Position)

head(fifa)
str(fifa)       
fifa$International.Reputation

fifa %>% group_by(Position) %>% summarize(meanBC = mean(BallControl), count = n()) %>% print(n = 28)

sapply(fifa, function(x) sum(is.na(x)))


fifa_NAN=fifa %>% filter(!is.na(Position) & !is.na(International.Reputation) & !is.na(Finishing) & !is.na(BallControl))
sapply(fifa_NAN, function(x) sum(is.na(x)))
gg_miss_var(fifa_NAN)

player_reputationfactor=cut(fifa_NAN$International.Reputation, breaks=c(1,2,3,4,5), labels=c('Obscurity','Regular','Famous','SuperStar'),include.lowest=TRUE)
fifa_player=fifa_NAN  %>% mutate(player_reputationfactor=player_reputationfactor)
fifa_player %>% select(Name,player_reputationfactor, Position, BallControl, Finishing,International.Reputation) %>%
  ggplot(aes(x=BallControl, y=Finishing, color= player_reputationfactor)) + geom_point() + facet_wrap(~Position)

library(plotly)
p=fifa_player %>% select(Name,player_reputationfactor, Position, BallControl, Finishing,International.Reputation) %>% count(Position,player_reputationfactor) %>%
ggplot(aes(x=Position, y=player_reputationfactor)) + geom_tile(mapping=aes(fill=n))
ggplotly(p)

d=fifa_player %>% select(player_reputationfactor, Position, BallControl, Finishing) %>% 
  ggplot(aes(x=BallControl, y=Finishing, color=player_reputationfactor)) + geom_point()
ggplotly(d)

fifa_player %>% select(Name,player_reputationfactor, Position, BallControl, Finishing,International.Reputation) %>% 
  ggplot(aes(x=BallControl, y=Finishing)) + geom_boxplot(mapping=aes(group=cut_width(BallControl,5)))


fifa_player %>% select(player_reputationfactor, BallControl, Finishing) %>% 
  ggpairs(aes(color=player_reputationfactor))


xBarVec=c()
xbarGenerator = function(sampleSize, number_of_samples)
{
  for (i in 1:number_of_samples)
  {
    theSample = sample(fifa_LM$Agility, sampleSize)
    xbar = mean(theSample)
    xBarVec = c(xBarVec, xbar)
  }
  return(xBarVec)
}

xbars=xbarGenerator(50,10000)


hist(xbars, col='red', main='Distribution of the sample mean for LM: n = 50')




xBarVec=c()
xbarGenerator = function(sampleSize, number_of_samples)
{
  for (i in 1:number_of_samples)
  {
    theSample = sample(fifa_LF$Agility, sampleSize)
    xbar = mean(theSample)
    xBarVec = c(xBarVec, xbar)
  }
  return(xBarVec)
}

xbars=xbarGenerator(5,10000)


hist(xbars, col='red', main='Distribution of the sample mean for LF: n = 5')



sample = c(1.7, 1.6, 1.5, 2.0, 2.3, 1.6, 1.6, 1.8, 1.5, 1.7, 1.2, 1.4, 1.6, 1.6, 1.6)
t.test(x=sample, mu = 1.8, conf.int = "TRUE", alternative = "two.sided")


install.packages('car')
install.packages('mgcv')
install.packages('tidymodels')
install.packages('maps')
install.packages('XML')
install.packages('jsonlite')
       