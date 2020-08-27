#Basic of R code

x = c(1, 2, 3, 4)
plot(x)

age=c(21,22,24,19,20,23)
age
age[2]
yrs_math_ed = c(4,5,2,5,3,5)
yrs_math_ed

df1=data.frame(Age=age, Years=yrs_math_ed)
df1
a=c("Mary","Martha", "Kim", "Kristen", "Amy", "Sam")
b=c('English', 'Math', 'Socialogy', 'Math', 'Music', 'Dance')
df2= data.frame(Name=a, Major=b)
df2

df3=cbind(df1, df2) #cbind combines columns
df3
class(df3$Name)
str(df3)
summary(df3)
d=c(19,4,'John','Math')
d
df4=rbind(df3,d)
df4

str(d)
str(df4)
df4[,3]
df4[,1]



example = read.csv(file.choose(), header = TRUE)
example
plot(example$ad_tv, example$sales, pch=15, xlab= 'TV Advertising', ylab='Sales', main='Sales VS Adverstising')
abline(h=55, col='orange', lwd=5)

dev.off()
iris
hist(iris$Petal.Length, col='red')
pairs(~mpg+disp+drat+wt,data=mtcars,
      main="Simple Scatterplot Matrix")

par(mfrow=c(1,2)) #dividing the plot space to one row, two columns.
summary(mpg$class)
barplot(summary(mpg$class))
mpg$classfact=as.factor(mpg$class)  #Change characters to factors
head(mpg)
barplot(summary(mpg$classfact))
barplot(mpg$classfact)

catalog = c('Data Viz', 'Machine Learning', 'Mathematics', 'Statistics', 'Computer Science', 'Communication', 'Domain Expertise')
proficient_level= c(2,1,3,3,2,3,1)
DataScienceProfile=data.frame(Catalog=catalog, Proficient_level=proficient_level)
DataScienceProfile
summary(DataScienceProfile)
barplot(names.arg=DataScienceProfile$Catalog, DataScienceProfile$Proficient_level, ylab='Proficient Level', main='My Data Science Profile', col='blue', ylim=c(0,5))

xBarVec=c()
population=rnorm(10000, mean=60, sd=10)
xbarGenerator = function(sampleSize, number_of_samples)
{
  for (i in 1:number_of_samples)
  {
    theSample = sample(population, sampleSize)
    xbar = mean(theSample)
    xBarVec = c(xBarVec, xbar)
  }
  return(xBarVec)
}

xbars=xbarGenerator(30,1000)
length(xbars)
hist(xbars)
summary(xbars)

xbarVec1=c()
xbarGenerator1 = function(sampleSize, number_of_samples, mean, sd)
{
  for (i in 1:number_of_samples)
  {
    theSample=rnorm(sampleSize,mean,sd)
    xbar=mean(theSample)
    xbarVec1=c(xbarVec1, xbar)
  }
  return(xbarVec1)
}
xbars1=xbarGenerator1(30,1000,60,10)
hist(xbars1)
summary(xbars1)

sd(xbars)
sd(xbars1)

quiz= read.csv(file.choose(), header = TRUE)
quiz
boxplot(quiz$Score~quiz$Treatment, data=quiz,main='Creativity', xlab='Treatment', ylab='Score')
abline(h=mean(quiz$Score))
points(mean(quiz$Score))
mean(quiz$Score)

install.packages("ggplot2") # Install it again
library(ggplot2) # Load the librarie (you have to do this one on each new session)
mtcars
install.packages("C50")
install.packages("gmodels")
install.packages("party")
install.packages("car")
install.packages("ggplot2")
library(C50)
library(gmodels)
library(party)
library(car)
library(ggplot2)
mpg

install.packages("magrittr") # package installations are only needed the first time you use it
install.packages("dplyr")    # alternative installation of the %>%
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)    # alternatively, this also loads %>%
data()
mpg %>% ggplot(aes(x = displ, y = hwy, color = drv)) + geom_point()

ggplot(mpg, aes(x=displ,y=hwy, color=drv))+geom_point() #Both two scatter plots are the same
ggplot(data=mpg, mapping=aes(x=displ,y=hwy, color=drv))+geom_point()


ggplot(mpg, aes(x=hwy, y=cty))+geom_point()

ggplot(mpg, aes(x=hwy, y=cty))+geom_smooth()

ggplot(mpg, aes(x=hwy, y=cty, linetype=drv))+geom_smooth()
ggplot(mpg, aes(x=hwy, y=cty, linetype=drv, color=drv))+geom_smooth()+geom_point()
ggplot(mpg)+geom_smooth(aes(x=hwy, y=cty, linetype=drv,color=drv))
+geom_point(aes(x=hwy, y=cty, color=class))

ggplot(mpg)+geom_smooth(aes(x=hwy, y=cty, linetype=class,color=class))+geom_point(aes(x=hwy, y=cty, color=class))+
facet_wrap(~class)


ggplot(mpg)+geom_smooth(aes(x=hwy, y=cty, linetype=class,color=class))+geom_point(aes(x=hwy, y=cty, color=class))+
facet_grid(drv~class)


mpg %>% ggplot(aes(x = cty, y = hwy, color = drv)) + geom_point() + facet_grid(cyl~year)

mpg %>% ggplot(aes(x = class, y = cty)) + geom_boxplot() + ggtitle("Boxplot of City MPG v. Class")


mpg %>% ggplot(aes(x = class)) + geom_bar(stat = "count") #stat = "count" is default
mpg %>% ggplot(aes(x = class, y = cty)) + geom_bar()
#stat = identity adds the values of the y value per class (x level)
mpg %>% ggplot(aes(x = class, y = cty)) + geom_bar(stat = "identity") #sum of all cty mile per gallon by class

mpg %>% ggplot(aes(x = class, fill=drv)) + geom_bar(position='dodge')
#position can be {dodge, stack, fill}

ggplot(mpg)+geom_smooth(aes(x=hwy, y=cty, linetype=class,color=class))+geom_point(aes(x=hwy, y=cty, color=class), position='jitter')+
facet_grid(drv~class)


ggplot(mpg, aes(x=class, fill=class))+geom_bar()+coord_flip()
ggplot(mpg, aes(x=class, fill=class))+geom_bar()+coord_polar()

install.packages('maps')
library(maps)
usa=map_data('usa')
ggplot(usa)+geom_polygon(aes(x=long,y=lat,group=group),fill='red', color='black')+
  coord_quickmap()

Dallas=tibble(long=c(-96.797), lat=c(32.7767), names=c('Dallas'))
p+geom_point(data=Dallas, aes(x=long, y=lat), shape=20,color='black',fill='blue', size=5)+
  geom_text(data=Dallas, aes(x=long, y=lat,label=names),hjust=0,nudge_x=1,color='white')
