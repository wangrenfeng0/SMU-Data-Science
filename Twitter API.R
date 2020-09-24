library(XML) #xml_Parse
library(dplyr)
library(tidyr)
library(stringi)
library(rvest) #html_table, html_node
library(ggplot2)
library(RCurl) #getURL
library(plotly)
library(gridExtra)
library(stringr)
library(rtweet)


#Go to https://docs.ropensci.org/rtweet/ to check most updated commands.
#Get tweets
tweets = search_tweets("$appl", n = 10, lang = "en")
tweets
#Locations 
Dallas=get_trends('Dallas')
Dallas
#Get Trends for Location

DallasTrends = Dallas %>% select(trend) #  Dallas, US
DallasTrends
DallasTrends[1:10,]

stream_tweets(
  "realdonaldtrump,trump",
  timeout = 60 * 60 * 24 * 7,
  file_name = "tweetsabouttrump.json",
  parse = FALSE
)

## read in the data as a tidy tbl data frame
djt <- parse_stream("tweetsabouttrump.json")

## get user IDs of accounts followed by CNN
cnn_fds <- get_friends("cnn")

## lookup data on those accounts
cnn_fds_data <- lookup_users(cnn_fds$user_id)
cnn_fds_data
