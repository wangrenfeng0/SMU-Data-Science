mpg
x = NA
is.na(x)
#Dataframe for the Example
age = c(22,21,NA,24,19,20,23)
yrs_math_ed = c(4,5,NA,2,5,3,5)
names = c("Mary","Martha","Rosy","Kim","Kristen","Amy","Sam")
subject = c("English","Math",NA,"Sociology","Math","Music","Dance")
df = data.frame(Age = age, Years = yrs_math_ed,Name = names, Major = subject)
df
is.na(df$Years)
is.na(df$Name)
is.na(df$Major)

library(tidyverse)
df %>% filter(!is.na(Years))

df%>% filter(is.na(Years))
install.packages('GGally')
library(GGally)

install.packages('nycflights13')
library(nycfligthts13)

# chooses rows with year < 2000
mpg %>% filter(year < 2000)

#chooses the columns class, city, hwy
mpg %>% select(class, cty, hwy)


#Dataframe for the Example
age = c(22,21,NA,24,19,20,23)
yrs_math_ed = c(4,5,NA,2,5,3,5)
names = c("Mary","Martha","Rosy","Kim","Kristen","Amy","Sam")
subject = c("English","Math",NA,"Sociology","Math","Music","Dance")
df = data.frame(Age = age, Years = yrs_math_ed,Name = names, Major = subject)
df
#Sort on a column with no NAs
df %>% arrange(df$Name)
#Sort on a column with NAs
df %>% arrange(df$Age)
# NA end up at the end of the list when sorted on column with NAs.




#class, n() function can only be used within the function summarize, filter, mutate
mpg %>% group_by(class) %>% summarize(meanCTY = mean(cty), count = n())
# class and model
mpg %>% group_by(class, model) %>% summarize(meanCTY = mean(cty), count = n())
#Chapter 2 example
mpg %>% group_by(class) %>% summarize(sum(cty))


df = data.frame(Name = c("Jack","Julie","Cali","Sunny","James"), Age = c(3,4,2,1,5), Height = c(23,25,30,29,24), Gender = c("Male","Female","Female","Female","Male"))
df %>% group_by(Gender) %>% summarize(MeanHeight = mean(Height))
install.packages('naniar')
library(naniar)
diamonds
gg_miss_var(diamonds)
sapply(diamonds, function(x) sum(is.na(x)))
str(diamonds)
diamond1=diamonds %>% mutate(zscore_x=(x-mean(x))/sd(x), zscore_y=(y-mean(y))/sd(y), zscore_z=(z-mean(z))/sd(z))




fifa=read.csv('/Users/renfengwang/Documents/SMU\ Data\ Science\ Program/Doing\ Data\ Science/Week\ 3/FIFA\ Players.csv', header=TRUE)
fifa
fifa %>% select(Finishing, BallControl, ShotPower) %>% ggpairs()
fifa_LMLF=fifa %>% filter(Position=='LM' | Position=='LF')
head(fifa_LMLF)
str(fifa_LMLF)
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