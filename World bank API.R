library(WDI)
# World Bank Development Indicators
#Useful URL in explainging WDI XML and JSON data formats.
#https://datahelpdesk.worldbank.org/knowledgebase/articles/898599-indicator-api-queries
#Goal 1: Create a bar chart of topics relating to gdp.
#search for reports with "gdp" in the description
results = as.data.frame(WDIsearch("gdp"))
#Many reports have more than 4 parts of the indicator
# This is in contrast to this documentation: 
#https://datahelpdesk.worldbank.org/knowledgebase/articles/201175-how-does-the-world-bank-code-its-indicators
# We use a new function from a new package that we will cover later: str_count
# This function is in the stringr package and simply counts the number of a specific 
# character ("\\.") in a given string (indicator)
# The \\ means to literally look for the '.' which means something else in this context.
#This line will filter the data frame to leave only those with 4 pieces in the indicator.
resultsGoodIndicator = results %>% filter(str_count(indicator,"\\.")==3)
#Check out the new data frame with only 4 piece indicators
resultsGoodIndicator$indicator
# Break the indicator code up into 4 distinct columns. 
resultsGoodIndicator = as.data.frame(resultsGoodIndicator) %>% separate(indicator,c("topic","general","specific","extension"))
#plot the topic column in a bar chart to see the frequency of each topic.
#compare the expenditure (NE) and the income (NY)
resultsGoodIndicator %>% ggplot(aes(x = topic, fill = topic)) + geom_bar()
#Goal 2: Plot GDP (NY and GDP) per capita (PCAP) of Mexico, Canada and the US in constant US dollars (KD)
dat = WDI(indicator='NY.GDP.PCAP.KD', country=c('MX','CA','US'), start=1960, end=2012)
head(dat)
library(ggplot2)
ggplot(dat, aes(x = year, y = NY.GDP.PCAP.KD, color=country)) + geom_line() + 
  xlab('Year') + ylab('GDP per capita')
