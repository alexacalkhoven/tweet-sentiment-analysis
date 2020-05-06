# elon musk tweet sentiment analysis

Setup <- function(){
  # load in libraries
  library(tidyverse)
  library(tidytext)
  library(glue)
  library(stringr)
  library(plyr)
  library(dplyr)
  library(tibble)
  
  # set working directory
  setwd("c:/Users/alexa/Documents/Coding/R-code/sentiment-analysis")
  
  # read in the csv file
  elonTweets <- read.csv(file = "data/data_elonmusk.csv")
  
  # make an object to hold the sentiments 
  sentiments <- tibble()
}

Run <- function(){
  # loop through the csv
  for(i in 1:(nrow(elonTweets))){
    sentiments <- rbind(sentiments, GetSentiment(i))
  }
}

GetSentiment <- function(x){
  
  # load tweet
  tweet <- as.character(elonTweets[x, 2])
  print(tweet)
  
  # load date
  year <- as.character(elonTweets[x, 3])
  year <- as.numeric(strsplit(year, "-")[[1]][1])
  
  # remove "$"
  tweet <- gsub("\\$", "", tweet)
  
  # tokenize 
  tokens <- data_frame(text = tweet) %>% unnest_tokens(word, text)
  
  # get the tweet sentiment
  sentiment <- tokens %>% 
    inner_join(get_sentiments("bing"))
  
  if(empty(sentiment)){
    print("empty")
    return(c(0, year))
  }
  
  sentiment <- count(sentiment, "sentiment") 
  
  # check for no neg
  if(!(grepl("negative", sentiment$sentiment)))
  {
    return(c(sentiment[which(sentiment == "positive"), "freq"], year))
  }
  
  # check for no pos
  if(!(grepl("positive", sentiment$sentiment)))
  {
    return(c(-sentiment[which(sentiment == "negative"), "freq"], year))
  }
  
  which(sentiment == "negative")
  
  r <- sentiment[which(sentiment == "positive"), "freq"] - sentiment[which(sentiment == "negative"), "freq"]
  
  return(c(r, year))
}

Visualize <- function(){
  plot(sentiments$X2017, sentiments$X2)
}

Setup()
Run()
Visualize()


