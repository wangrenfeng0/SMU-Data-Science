library(XML) #xml_Parse
library(dplyr)
library(tidyr)
library(stringi)
library(rvest) #html_table, html_node
library(ggplot2)
library(RCurl) #getURL
library(plotly)
library(gridExtra)
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
hp<-read_html("https://www.w3schools.com/xml/simple.xml")
hp_nameR <- html_nodes(hp,"name")
hp_priceR <- html_nodes(hp,"price")
hp_descR <- html_nodes(hp,"description")
hp_nameR
hp_name = stri_sub(hp_nameR,7,-8)
hp_name
hp_price = stri_sub(hp_priceR,8,-9)
hp_price
hp_desc = stri_sub(hp_descR,14,-15)
hp_desc
bfast = data.frame(hp_name,hp_price,hp_desc)
grep("toast", bfast$hp_desc)
grepl("toast",bfast$hp_desc)

sum(grepl("toast",bfast$hp_desc))

grep("toast", bfast$hp_desc)
grep('toast', bfast$hp_desc, value= TRUE)
grepl("toast",bfast$hp_desc)

sum(grepl("toast",bfast$hp_desc))
-----------------------------------------------------
restaurant=read_html('https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml')
rest_nameR=html_nodes(restaurant, 'name')
rest_zipcodeR=html_nodes(restaurant, 'zipcode')
rest_citycouncilR=html_nodes(restaurant, 'councildistrict')
rest_name=stri_sub(rest_nameR, 7, -8)
rest_name
rest_zipcode=stri_sub(rest_zipcodeR, 10, -11)
rest_zipcode
rest_citycouncil=stri_sub(rest_citycouncilR, 18, -19)
rest_citycouncil
rest_infor=data.frame(name=rest_name, zipcode=rest_zipcode, city_council=rest_citycouncil)
rest_infor

grep('sushi', rest_infor$name, value=TRUE, ignore.case=TRUE)
grep('sushi', rest_infor$name, ignore.case=TRUE)
library(stringr)
rest_infor %>% filter(str_detect(name, 'SUSHI'))
str(rest_infor)
rest_indowntown=rest_infor %>% filter(city_council=='11')
grep('SUSHI', rest_indowntown$name, value=TRUE, ignore.case=TRUE)
rest_indowntown %>% filter(str_detect(name, 'SUSHI'))
rest_infor$city_councilfactor=as.factor(rest_infor$city_council)
rest_infor$zipcodefactor=as.factor(rest_infor$zipcode)
head(rest_infor)
totals=count(rest_infor, city_councilfactor)
totals
rest_merg=merge(rest_infor,totals)
rest_merg
rest_merg %>% group_by(city_councilfactor) %>% arrange(city_councilfactor) %>% ggplot(aes(x=reorder(city_councilfactor, n), y=n)) + geom_bar(stat='identity')+
  geom_text(aes(city_councilfactor, vjust=1,label=n, fill=NULL), data=totals)+ xlab('City Council')+ ylab('Count')

------------------------------------------------------
# API Codes

#Unit 4 API R Code
#Intrinio
# Andrew Carpenter helpful link: http://old.r-fiddle.org/#/fiddle?id=n1shL0IB&version=1
#Show access through URL: https://api-v2.intrinio.com/companies/AAPL?api_key=OjcwYTBlMTU1ODk1YjJiMTI1ZTE1OWFhZTRjMDhkYWM5
library(httr)
library(jsonlite)
username = "OjcwYTBlMTU1ODk1YjJiMTI1ZTE1OWFhZTRjMDhkYWM5"
password = "Ojc0OWNjMmQ3Yzk4MTRhZmNmZGYwY2VmNjVkNTBkNjgy"

base <- "https://api-v2.intrinio.com/securities/"
endpoint <- "prices"
stock <- "AAPL"
call1 <- paste(base,stock,"/", endpoint,"?", 'api_key=', username,sep='')
call1
#get_prices <- fromJSON(call)
#get_prices_text <- content(get_prices, "text")
#get_prices_text
stock_json <- jsonlite::fromJSON(call1, flatten=TRUE)
stock_json
stock_df <- as.data.frame(stock_json)
stock_df

stock_df$stock_prices.date = as.Date(stock_df$stock_prices.date)

par(mar = rep(2, 4)) #Set plot margin
par(mfrow = c(2,1)) # does not work in ggplot

p=stock_df %>% ggplot(aes(x = stock_prices.date, y = stock_prices.close))+ geom_line() + ggtitle("Adjusted Close")
ggplotly(p)

q=stock_df %>% ggplot(aes(x = stock_prices.date, y = stock_prices.volume))+ geom_col() + ggtitle("Adjusted Volume")
ggplotly(q)

grid.arrange(p, q, nrow=2)


--------------------------------------------




username = "OjcwYTBlMTU1ODk1YjJiMTI1ZTE1OWFhZTRjMDhkYWM5"
password = "Ojc0OWNjMmQ3Yzk4MTRhZmNmZGYwY2VmNjVkNTBkNjgy"

base <- "https://api-v2.intrinio.com/securities/"
endpoint <- "prices"
stock <- "GE"
call1 <- paste(base,stock,"/", endpoint,"?", 'api_key=', username,sep='')
call1
#get_prices <- fromJSON(call)
#get_prices_text <- content(get_prices, "text")
#get_prices_text
stock_json <- jsonlite::fromJSON(call1, flatten=TRUE)
stock_json
stock_df <- as.data.frame(stock_json)
stock_df

stock_df$stock_prices.date = as.Date(stock_df$stock_prices.date)

par(mar = rep(2, 4)) #Set plot margin
par(mfrow = c(2,1)) # does not work in ggplot

p=stock_df %>% ggplot(aes(x = stock_prices.date, y = stock_prices.close))+ geom_line() + ggtitle("Adjusted Close")
ggplotly(p)

q=stock_df %>% ggplot(aes(x = stock_prices.date, y = stock_prices.volume))+ geom_col() + ggtitle("Adjusted Volume")
ggplotly(q)

grid.arrange(p, q, nrow=2)



client <- IntrinioSDK::ApiClient$new()

# Configure API key authorization: ApiKeyAuth
client$configuration$apiKey <- "OjcwYTBlMTU1ODk1YjJiMTI1ZTE1OWFhZTRjMDhkYWM5"

# Setup API with client
SecurityApi <- IntrinioSDK::SecurityApi$new(client)

# Required params
identifier <- "GE" # Character | A Security identifier (Ticker, FIGI, ISIN, CUSIP, Intrinio ID)

# Optional params
opts <- list(
  start_date = as.Date("2018-01-01"), # Date | Return prices on or after the date
  end_date = as.Date("2019-01-01"), # Date | Return prices on or before the date
  frequency = "daily", # Character | Return stock prices in the given frequency
  page_size = 100, # Integer | The number of results to return
  next_page = NULL # Character | Gets the next page of data from a previous API call
)

response <- SecurityApi$get_security_stock_prices(identifier, opts)

print(response)
print(response$content)


uscis='https://raw.githubusercontent.com/vicdus/uscis-case-statistics/master/package.json'
uscis_json <- jsonlite::fromJSON(uscis, flatten=TRUE)
uscis_json

uscis_df <- as.data.frame(uscis_json)
uscis_df