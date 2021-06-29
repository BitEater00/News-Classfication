news <- read.csv("news.csv")
news <- news[sample(nrow(news)),]
news_part1 <- news %>% filter(news$type == 1) %>% sample_frac(0.1)
news_part2 <- news %>% filter(news$type == 0) %>% sample_frac(0.1)
news <- bind_rows(news_part1 , news_part2)

doc <- VCorpus(VectorSource(news$text))
doc <- tm_map(doc, removePunctuation)
doc <- tm_map(doc, removeNumbers)
doc <- tm_map(doc, content_transformer(tolower))
doc <- tm_map(doc, removeWords, stopwords("english"))
doc <- tm_map(doc, stripWhitespace)
doc <- tm_map(doc, content_transformer(lemmatize_strings))

dtm <- DocumentTermMatrix(doc)
dtm_clean <- removeSparseTerms(dtm, sparse = 0.99)
dtm_mat <- as.matrix(dtm_clean)
y_prediction = news$type
dtm_mat <- cbind(dtm_mat,y_prediction)
dtm_df <- as.data.frame(dtm_mat)
#dtm_df$y_prediction <- ifelse(dtm_df$y_prediction == 2, 1, 0)
dtm_df$y_prediction <- as.factor(dtm_df$y_prediction)

set.seed(2020)
index <- sample(nrow(dtm_df), nrow(dtm_df)*0.8, replace = FALSE)
train_news <- dtm_df[index,]
test_news <- dtm_df[-index,]
names(train_news) <- make.names(names(train_news))
names(test_news) <- make.names(names(test_news))


#Logistics Regression
cls_lr <- glm(formula = y_prediction~.,  data = train_news, family = 'binomial')


#Random Forest
k <- round(sqrt(ncol(train_news)-1))
cls_rf <- randomForest(formula = y_prediction ~ .,data = train_news,ntree = 25,mtry = k,method = 'class')

#Naive Bayes
cls_nb <- naive_bayes(y_prediction ~ ., data = train_news)



# Predicted values
train_news$pred_nb <- predict(cls_nb, type = 'class')
train_news$pred_lr <- predict(cls_lr, type = 'response')
train_news$pred_rf <- predict(cls_rf, type = 'response')

# Predicted Values for test set
test_news$pred_nb <- predict(cls_nb, newdata = test_news)
test_news$pred_lr <- predict(cls_lr, newdata = test_news, type = 'response')
test_news$pred_rf <- predict(cls_rf, newdata = test_news, type = 'response')

# Confussion Matrix
conf <- confusionMatrix(reference = test_set$y_prediction, data = test_set$pred_lr)
conf_nb <- caret::confusionMatrix(test_set$y_prediction, test_set$pred_nb)
conf_rf <- caret::confusionMatrix(test_set$y_prediction, test_set$pred_rf)
