library(XML) #xml_Parse
library(dplyr)
library(tidyr)
library(stringi)
library(rvest) #html_table, html_node
library(ggplot2)
library(RCurl) #getURL

#Basics of Scraping XML

# Method 1: XML

data=getURL("https://www.w3schools.com/xml/simple.xml")
doc <- xmlParse(data)
names <- xpathSApply(doc,"//name",xmlValue)
price <- xpathSApply(doc,"//price",xmlValue)
description <- xpathSApply(doc,"//description",xmlValue)
bfasts = data.frame(names,price,description)
bfasts
bfasts$description
length(grep("covered",bfasts$description)) # grep() is used to search any description contains 'covered'
grepl("covered",bfasts$description) # grepl() returns True or false if the value contains 'covered'
sum(grepl("covered",bfasts$description))
grep("covered",bfasts$description)  #The default value is FALSE, so it only returns index of rows
grep("covered",bfasts$description, value=TRUE)
which(grepl("covered",bfasts$description)) # shows which index contains 'covered'

# Method 2: rvest


grep("toast", bfast$hp_desc)
grep('toast', bfast$hp_desc, value= TRUE)
grepl("toast",bfast$hp_desc)

sum(grepl("toast",bfast$hp_desc))
