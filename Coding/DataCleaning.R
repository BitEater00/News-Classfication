##Data Analytics Project
##Data Cleaning

## Importing all the necessary libraries
library(readr)
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyr)
library(tm)
library(textstem) 
library(tidytext)
library(wordcloud2)
library(pROC)
library(ROCR)
library(randomForest)  
library(naivebayes)
library(caret)
library(janeaustenr)
library(igraph)
library(ggraph)

## Importing data
fake_news <- read_csv('Fake.csv')
true_news <- read_csv('True.csv')


##Dropping NA rows
true_news <- true_news %>% drop_na()
fake_news <- fake_news %>% drop_na()


## Merging Datasets
fake_news$type <- 0
true_news$type <- 1
news <- bind_rows(fake_news, true_news)
news$type <- as.factor(news$type)
type = news$type
news$text <- paste(news$title , news$text , sep = ' ')
news <- cbind(news["text"] , type)

write.csv(news, "news.csv",row.names = FALSE)