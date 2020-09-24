library(class)
library(caret)
library(e1071)
library(magrittr)
library(XML) #xml_Parse
library(dplyr)
library(tidyr)
library(stringi)
library(rvest) #html_table, html_node
library(ggplot2)
library(RCurl) #getURL
library(plotly)
library(gridExtra)
library(httr)
library(jsonlite)

df=getURL('https://public.opendatasoft.com/api/records/1.0/search/?dataset=titanic-passengers&rows=2000&facet=survived&facet=pclass&facet=sex&facet=age&facet=embarked')
ps=jsonlite::fromJSON(df, flatten=T)

passenger <- as.data.frame(ps$records)
ps_infor=passenger[,4:15]

names(ps_infor)=c('Fare', 'Name', 'Embarked', 'Age', 'Parch', 'Pclass','Sex', 'Survived','Ticket','PassengerID', 'Sibsp','Cabin')
str(ps_infor)
ps_infor$Pclass=as.factor(ps_infor$Pclass)
ps_inforage$Sex=as.character(ps_inforage$Sex)
ps_infor$Survived=as.factor(ps_infor$Survived)

s=sapply(test_age$Sex, function(x) is.na(x))

library(naniar)
gg_miss_var(test_age)
gg_miss_var(ps_inforage)
ps_inforage=ps_infor %>% filter(ps_infor$Age!='NA')
classification = knn.cv(ps_inforage[,c(4,6)], ps_inforage$Survived, prob=T,k=5)
confusionMatrix(table(classification, ps_inforage$Survived))

classification_1=knn(ps_inforage[,c(4,6)],c(33,1),ps_inforage$Survived, prob=T,k=5)
classification_2=knn(ps_inforage[,c(4,6)],c(33,2),ps_inforage$Survived, prob=T,k=5)
classification_3=knn(ps_inforage[,c(4,6)],c(33,3),ps_inforage$Survived, prob=T,k=5)

test=read.csv(file.choose(),header=T)
str(test_age)
test$Survived=as.factor(test$Survived)

test$Pclass=as.factor(test$Pclass)
test_survived=right_join(test,ps_infor,by=c('Fare', 'Name', 'Embarked', 'Age', 'Parch', 'Pclass','Sex','Ticket','Cabin'))
test_survived$Survived=as.character(test_survived$Survived)
test_age=test_survived %>% mutate(Survived=ifelse(Survived=='No',0,1)) %>% filter(test_survived$Age!='NA')
test_age$Survived=as.factor(test_age$Survived)
test_age$Sex=as.character(test_age$Sex)
ps_survived=ps_inforage %>% mutate(Survived=ifelse(Survived=='No',0,1))
classification_test=knn(ps_inforage[,c(4,6)],test_age[,c('Age', 'Pclass')], ps_survived$Survived, prob=T,k=5)
confusionMatrix(table(test_age$Survived, classification_test))

test_sex=test_age %>% mutate(Sex=ifelse(Sex=='male',1,0))

ps_sex=ps_survived %>% mutate(Sex=ifelse(Sex=='male',1,0))
ps_sex$Sex=as.factor(ps_sex$Sex)
ps_sex$Survived=as.factor(ps_sex$Survived)

classification_sex=knn(data.frame(ps_sex$Sex),data.frame(test_sex$Sex), ps_sex$Survived, prob=T,k=5)

confusionMatrix(table(test_sex$Survived, classification_sex))

------------------------------------------------------------
library(gghighlight)
splitPerc = .7
iterations = 500
numks = 90
masterAcc = matrix(nrow = iterations, ncol = numks)

for(j in 1:iterations)
{
  accs = data.frame(accuracy = numeric(30), k = numeric(30))
  trainIndices = sample(1:dim(iris)[1],round(splitPerc * dim(iris)[1]))
  train = iris[trainIndices,]
  test = iris[-trainIndices,]
  for(i in 1:numks)
  {
    classifications = knn(train[,1:2],test[,1:2],train$Species, prob = TRUE, k = i)
    table(classifications,test$Species)
    CM = confusionMatrix(table(classifications,test$Species))
    masterAcc[j,i] = CM$overall[1]
  }
}
MeanAcc = colMeans(masterAcc)
plot(seq(1,numks,1),MeanAcc, type = "l")
p=ggplot(mapping=aes(x=seq(1,numks,1), y=MeanAcc))+geom_line()
ggplotly(p)

----------------------------------------
#Leave one our cross validation 
for(j in 1:iterations)
{
  accs = data.frame(accuracy = numeric(30), k = numeric(30))
  trainIndices = sample(1:dim(iris)[1],dim(iris)[1])
  train = iris[trainIndices,]
  for(i in 1:numks)
  {
    classifications = knn.cv(train[,1:2],train$Species, prob = TRUE, k = i)
    table(classifications,train$Species)
    CM = confusionMatrix(table(classifications,train$Species))
    masterAcc[j,i] = CM$overall[1]
  }
}
MeanAcc = colMeans(masterAcc)
#plot(seq(1,numks,1),MeanAcc, type = "l")
p=ggplot(mapping=aes(x=seq(1,numks,1), y=MeanAcc))+geom_line()
ggplotly(p)