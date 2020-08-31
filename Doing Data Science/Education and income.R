income=read.csv(file.choose(), header=TRUE)
income
summary(income)
income$Subjectfact=as.factor(income$Subject)

for (i in 1:nrow(income))
{
  if (income$Educ[[i]]=='13-15'){
    income$Educ[[i]]=14
  } else if (income$Educ[[i]]=='>16') {
    income$Educ[[i]]=17
  } else if (income$Educ[[i]]=='<12') {
    income$Educ[[i]]=11
  } else {
    income$Educ[[i]]=as.numeric(income$Educ[[i]])
  }
}
income$Educfactor=as.factor(income$Educ)
income
class(income$Educfactor)

p=ggplot(income, aes(x=Educfactor, y=Income2005))+geom_boxplot()+ggtitle('Education & Income')+xlab('Education Year')+ylab('Income')+
  stat_summary(fun=mean, geom="point", shape=20, size=1, color="red", fill="red")
library(plotly)
ggplotly(p)

mtcars
library(ggplot2) 
theme_set(theme_bw())
mtcars$`car name` <- rownames(mtcars)
mtcars$mpg_z <- round((mtcars$mpg - mean(mtcars$mpg))/sd(mtcars$mpg), 2)  # compute normalized mpg
mtcars$mpg_type <- ifelse(mtcars$mpg_z < 0, "below", "above")  # above / below avg flag
mtcars <- mtcars[order(mtcars$mpg_z), ]  # sort
mtcars$`car name` <- factor(mtcars$`car name`, levels = mtcars$`car name`)  # convert to factor to retain sorted order in plot.
# Plot
ggplot(mtcars, aes(x=`car name`, y=mpg_z, label=mpg_z)) + 
  geom_point(stat='identity', aes(col=mpg_type), size=6)  +
  scale_color_manual(name="Mileage", 
                     labels = c("Above Average", "Below Average"), 
                     values = c("above"="#00ba38", "below"="#f8766d")) + 
  geom_text(color="white", size=2) +
  labs(title="Diverging Dot Plot", 
       subtitle="Normalized mileage from 'mtcars': Dotplot") + 
  ylim(-2.5, 2.5) +
  coord_flip()
