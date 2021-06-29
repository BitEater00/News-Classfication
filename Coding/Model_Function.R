
Model_Function <- function(data){
  data <- news[sample(nrow(data)),]
  data <- data %>%  sample_frac(0.1)
  
  ##Preprocessing
  doc <- VCorpus(VectorSource(data$text))
  doc <- tm_map(doc, removePunctuation)
  doc <- tm_map(doc, removeNumbers)
  doc <- tm_map(doc, content_transformer(tolower))
  doc <- tm_map(doc, removeWords, stopwords("english"))
  doc <- tm_map(doc, stripWhitespace)
  doc <- tm_map(doc, content_transformer(lemmatize_strings))
  
  ## Data Preparation
  dtm <- DocumentTermMatrix(doc)
  dtm_clean <- removeSparseTerms(dtm, sparse = 0.99)
  dtm_mat <- as.matrix(dtm_clean)
  y_prediction = data$type
  dtm_mat <- cbind(dtm_mat,y_prediction)
  dtm_df <- as.data.frame(dtm_mat)
  dtm_df$y_prediction <- ifelse(dtm_df$y_prediction == 2, 1, 0)
  dtm_df$y_prediction <- as.factor(dtm_df$y_prediction)
  
  ## Train Test Split
  set.seed(2020)
  index <- sample(nrow(dtm_df), nrow(dtm_df)*0.8, replace = FALSE)
  train_set <- dtm_df[index,]
  test_set <- dtm_df[-index,]
  names(train_set) <- make.names(names(train_set))
  names(test_set) <- make.names(names(test_set))
  
  ## Fitting Model
  #Random Forest
  k <- round(sqrt(ncol(train_set)-1))
  clf_rf <- randomForest(formula = y_prediction ~ .,data = train_set,ntree = 5 ,mtry = k,method = 'class')
  
  #Naive Bayes
  clf_nb <- naive_bayes(y_prediction ~ ., data = train_set)
  
  ##Meta Classifier Stacking
  # Predicted values
  train_set$pred_nb <- as.factor(predict(clf_nb, type = 'class'))
  train_set$pred_rf <- as.factor(predict(clf_rf, type = 'response'))
  
  # Predicted Values for test set
  test_set$pred_nb <- as.factor(predict(clf_nb, newdata = test_set))
  test_set$pred_rf <- as.factor(predict(clf_rf, newdata = test_set, type = 'response'))
  
  #Stacking
  train_set <- train_set[c("pred_nb" , "pred_rf" , "y_prediction")]
  test_set <-  test_set[c("pred_nb" , "pred_rf" , "y_prediction")]
  
  
  ##Logistics Regression
  clf_lr <- glm(formula = y_prediction~.,  data = train_set, family=binomial(link="logit"))
  
  
  ##printconsufion matrix
  test_set$pred_lr <- predict(clf_lr, newdata = test_set, type = 'response')
  test_set$pred_lr <- ifelse(test_set$pred_lr > 0.5,1,0)
  test_set$pred_lr <- as.factor(test_set$pred_lr)
  return(test_set)
}