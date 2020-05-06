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
  library(zoo)
  library(ggplot2)
  
  # set working directory
  setwd("~/elon-tweet-analysis")
  
  # read in the csv file
  elonTweets <- read.csv(file = "data/data_elonmusk.csv")
  
  # get input values
  print("Tweets will be analyzed for a random 60 day period")
  
  return(elonTweets)
}

Run <- function(elonTweets){
  sentiments <- tibble()
  
  options <- c(seq(1:nrow(elonTweets)))
  start <- sample(options, 1, FALSE)
  
  # loop through the csv
  for(i in start:(start+60)){
    dateFull <- as.character(elonTweets[i, 3])
    dateComponents <- strsplit(dateFull, " ")
    date <- as.Date(dateComponents[[1]][1])
    vector <- c(GetSentiment(elonTweets[i, 2]), date)
    print(vector)
    sentiments <- rbind(vector, sentiments)
  }
  names(sentiments)[1] <- "Sentiment"
  names(sentiments)[2] <- "Date"
  
  return(sentiments)
}

GetSentiment <- function(tweet){
  
  # load tweet
  tweet <- as.character(tweet)
  
  # remove "$"
  tweet <- gsub("\\$", "", tweet)
  
  # tokenize 
  tokens <- data_frame(text = tweet) %>% unnest_tokens(word, text)
  
  # get the tweet sentiment
  sentiment <- tokens %>% 
    inner_join(get_sentiments("bing"))
  
  if(empty(sentiment)){
    print("empty")
    return(0)
  }
  
  sentiment <- count(sentiment, "sentiment") 
  
  # check for no neg
  if(!(grepl("negative", sentiment$sentiment)))
  {
    return(sentiment[which(sentiment == "positive"), "freq"])
  }
  
  # check for no pos
  if(!(grepl("positive", sentiment$sentiment)))
  {
    return(-sentiment[which(sentiment == "negative"), "freq"])
  }
  
  which(sentiment == "negative")
  
  r <- sentiment[which(sentiment == "positive"), "freq"] - sentiment[which(sentiment == "negative"), "freq"]
  
  return(r)
}

Visualize <- function(sentiments){
  par("mar")
  par(mar=c(1,1,1,1))
  ggplot(sentiments, aes(x = as.Date(Date, origin = "1970-1-1"), y = Sentiment)) +
    geom_smooth(method = "auto") +
    scale_x_date(date_breaks = "15 day", 
                 date_labels = "%d-%b-%Y") +
    labs(title= "Elon Musk's Tweet Sentiments",
         y="Sentiment Value", x = "Date")
}

Setup() %>%
  Run() %>%
  Visualize()



