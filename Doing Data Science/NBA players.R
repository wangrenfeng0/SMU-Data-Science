library(ggthemes) 
library(plotly)
library(magrittr)
library(ggplot2)

player = read.csv(file.choose(), header= TRUE)
player
p=ggplot(player, aes(x=position))+geom_bar()+theme_economist()+ggtitle('Number of players in each position')
ggplotly(p)


ggplot(player[player$position=='C' | player$position=='F',],aes(x=position, y=weight))+geom_boxplot()+ggtitle('Distribution of the weight of centers (C) and the distribution of the weight of forwards (F).')

summary(player)

player$height_num=strsplit(player$height,'-')

player$height_num[[6]][[2]]
class(as.numeric(player$height_num[[2]][[2]]))



for (i in 1:nrow(player)) 
{
    player$height_inch[[i]]=as.numeric(player$height_num[[i]][[1]])*12+as.numeric(player$height_num[[i]][[2]])
}
player

p=ggplot(player[player$position=='C' | player$position=='F',],aes(x=position, y=height_inch))+
  geom_boxplot()+ggtitle('Distribution of the height of centers (C) and the distribution of the height of forwards (F).')


ggplotly(p)

p=ggplot(player,aes(x=position, y=height_inch))+
  geom_boxplot()
ggplotly(p)

ggplot(player,aes(x=position, fill=height_inch))+geom_bar(position='dodge')


p=ggplot(player[player$position=='C',],aes(x=position, y=height_inch))+
  geom_boxplot()
ggplotly(p)

ggplot(player)+geom_point(aes(x=weight, y=height_inch, color=position))+geom_smooth(aes(x=weight, y=height_inch, color=position, linetype=position))+
  facet_wrap(~position)

ggplot(player,aes(x=weight, y=height_inch))+geom_point(position='jitter')

class(player$birth_data)

player$birthday=as.Date(player$birth_date, format='%b %d, %Y')
head(player)
class(player$birthday)
p=ggplot(player, aes(x=birthday, y=height_inch))+geom_point()+theme_economist()+geom_smooth(aes(x=birthday, y=height_inch))
ggplotly(p)



p=plot_ly(player, x=~weight, y=~height_inch, z=~birthday, color=~position) %>%
  add_markers() %>%
  layout(scene = list(xaxis=list(title='Weight'),
                       yaxis=list(title='Height(inches)'),
                       zaxis=list(title='Birthday')))


p




