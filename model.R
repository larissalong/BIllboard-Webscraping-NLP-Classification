library(dplyr)
library(tidyverse)
library(tidytext)
#library(tm)
library(caret)
library(wordVectors)
library(magrittr)
library(forcats)

## --------------------------------------- Part 5: Classification --------------------------------------
### There are 2 ways to predict whether a song is rap or not based on its lyrics: word frequency or word2vec
## https://36kr.com/p/5082687.html
## The below approach is to use word frequency for classification

billboard_wrk <- readRDS("data/billboard_join_final.rds") 

billboard_wrk$lyrics <- as.character(billboard_wrk$lyrics)

# select rap and non-rap songs with equal quantity
rap <- billboard_wrk %>% 
  filter(rap == 1) %>% 
  select(song_name, lyrics) %>% 
  mutate(genre = "rap")

rap_non <- billboard_wrk %>% 
  filter(rap == 0) %>% 
  sample_n(nrow(rap)) %>% 
  select(song_name, lyrics) %>% 
  mutate(genre = "non_rap")

dat_model <- rbind(rap, rap_non)

#************************************ Section 1: Word Count ************************************#

# data cleaning: remove stopwords and get word counts
dat_model_new <- dat_model %>% 
  group_by(song_name, genre) %>% 
  unnest_tokens(word, lyrics) %>% 
  anti_join(stop_words, by = "word") %>% 
  count(song_name, genre, word) %>% 
  arrange(desc(n)) %>% 
  mutate(genre_rap = ifelse(genre == "rap", 1, 0)) %>% 
  spread(word, n, fill = 0)

dat_model_new$genre_rap <- as.factor(dat_model_new$genre_rap)

## 75% of the sample size
smp_size <- floor(0.75 * nrow(dat_model_new))

## set the seed and split to train / test
set.seed(123)
train_ind <- sample(seq_len(nrow(dat_model_new)), size = smp_size)
train <- dat_model_new[train_ind, ]
test <- dat_model_new[-train_ind, ]

#a <- head(train[, c(1:10, 13260:13277)]) ## Japanese Characters?

# SVM -----------------------------
# set control parameters
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

# use svmLinear method to train model (takes time to run!!!)
svm_Linear <- train(genre_rap ~., data = train[, -1], method = "svmLinear",
                    trControl = trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)

svm_Linear # 84.3% accuracy on training set; 82.46%

# predict on test set
test_pred <- predict(svm_Linear, newdata = test)
confusionMatrix(test_pred, test$genre_rap)  # 85.78%

# testing:  Billie Jean by Michael Jackson
test_pred[64]

# NN (error) -----------------------------
## set control parameters
# numFolds <- trainControl(method = 'cv', 
#                          number = 10, 
#                          classProbs = TRUE, 
#                          verboseIter = TRUE, 
#                          summaryFunction = twoClassSummary, 
#                          preProcOptions = list(thresh = 0.75, ICAcomp = 3, k = 5))
# 
# ## re-assign column names to avoid errors
# colnames(train) <- sapply(colnames(train), function(x) {make.names(as.factor(x))})
# colnames(test) <- sapply(colnames(test), function(x) {make.names(as.factor(x))})
# 
# ## keep the last duplicated column names to avoid errors
# train <- train[, !duplicated(colnames(train), fromLast = TRUE)] 
# test <- test[, !duplicated(colnames(test), fromLast = TRUE)]
# 
# ## get column numbers for special characters
# x <- ncol(train) - 5
# y <- ncol(train)
# #b <- as.data.frame(colnames(train[, -c(1, 3:155, x:y)]))
# 
# ## ERROR MESSAGES:
# ## At least one of the class levels is not a valid R variable name; 
# ## This will cause errors when class probabilities are generated because 
# ## the variables names will be converted to X0, X1...
# 
# ## define a function to deal with errors/ warnings
# nn_output <- function(train, which) {
#   out <- tryCatch(
#     {
#       #readLines(con=train, warn=FALSE) 
#       
#       nn <- train(genre_rap ~., data = cbind(train[, 2], train[, which]), 
#                   method = 'nnet', 
#                   preProcess = c('center', 'scale'), 
#                   trControl = numFolds, tuneGrid=expand.grid(size = c(10), decay = c(0.1)))
#       
#       return(nn)
#       
#     },
#     error=function(cond) {
#       #message("Here's the original error message:")
#       #message(cond)
#       return("Error!")
#     },
#     warning=function(cond) {
#       return("Warning!")
#     },
#     finally={}
#   )    
#   return(out)
# }
# 
# my_output <- c()
# 
# ## find which columns result in errors (take time to run!)
# for(i in 3:ncol(train)){
#   my_output[i-2] <- nn_output(train, i)
# }
# 
# ##-- -- to be done -- --
# ## train the model 
# nn <- train(genre_rap ~., data = train[, -c(1, 3:155, x:y)], method = 'nnet', 
#             preProcess = c('center', 'scale'), 
#             trControl = numFolds, tuneGrid=expand.grid(size = c(10), decay = c(0.1)))
# print(nn)
# 
# ## predict on test set
# pred_nn_test <- predict(nn, newdata = test)
# confusionMatrix(pred_nn_test, test$genre_rap)

# Logistic Regression -----------------------------
## (take time to run!)
logit <- train(genre_rap ~., data = train[, -1], method = "glm", family = "binomial")
print(logit)

## predict on test set
pred_logit_test <- predict(logit, newdata = test)
confusionMatrix(pred_logit_test, test$genre_rap)

#************************************ Section 2: 0/1 Binary Input ************************************#
dat_model_new2 <- dat_model %>% 
  group_by(song_name, genre) %>% 
  unnest_tokens(word, lyrics) %>% 
  anti_join(stop_words, by = "word") %>% 
  mutate(if_not = 1) %>% 
  mutate(genre_rap = ifelse(genre == "rap", 1, 0)) %>% 
  unique(.) %>% 
  spread(word, if_not, fill = 0)

dat_model_new2$genre_rap <- as.factor(dat_model_new2$genre_rap)

## 75% of the sample size
smp_size <- floor(0.75 * nrow(dat_model_new2))

## set the seed and split to train / test
set.seed(123)
train_ind <- sample(seq_len(nrow(dat_model_new2)), size = smp_size)
train <- dat_model_new2[train_ind, ]
test <- dat_model_new2[-train_ind, ]

# SVM -----------------------------
# set control parameters
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

# use svmLinear method to train model (takes time to run)
svm_Linear <- train(genre_rap ~., data = train[, -1], method = "svmLinear",
                    trControl = trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)

svm_Linear # accuracy on training set: 84.63% - Xiaoyi

# predict on test set
test_pred <- predict(svm_Linear, newdata = test)
confusionMatrix(test_pred, test$genre_rap)  # Accuracy: 83.33%

# Logistic Regression -----------------------------
## (take time to run!)
# logit <- train(genre_rap ~., data = train[, -1], method = "glm", family = "binomial")
# print(logit)
# 
# ## predict on test set
# pred_logit_test <- predict(logit, newdata = test)
# confusionMatrix(pred_logit_test, test$genre_rap)



#********************* Section 3: word vector embedding by word2vec*************************#
## load pre-trained Glove (word2vec)
rawVecs <- read_rds('data/glove300.rds') # each column represents one axis in the vector space in 300-D space
wvM <- as.matrix(select(rawVecs,-word))
rownames(wvM) <- rawVecs$word
wv <- as.VectorSpaceModel(wvM)
dict <- data.frame("word" = rownames(wvM), "vec" = rowSums(wvM)) # sum up vectors per word

# use embedding vectors to represent each word in lyrics as input
dat_model_new3 <- dat_model %>% 
  group_by(song_name, genre) %>% 
  unnest_tokens(word, lyrics) %>% 
  anti_join(stop_words, by = "word") %>% 
  inner_join(dict, by = "word") %>% 
  mutate(genre_rap = ifelse(genre == "rap", 1, 0)) %>% 
  unique(.) %>% 
  spread(word, vec, fill = 0)

dat_model_new3$genre_rap <- as.factor(dat_model_new3$genre_rap)

## 75% of the sample size
smp_size <- floor(0.75 * nrow(dat_model_new3))

## set the seed and split to train / test
set.seed(123)
train_ind <- sample(seq_len(nrow(dat_model_new3)), size = smp_size)
train <- dat_model_new3[train_ind, ]
test <- dat_model_new3[-train_ind, ]

# use svmLinear method to train model
svm_Linear <- train(genre_rap ~., data = train[, -1], method = "svmLinear",
                    trControl = trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)

svm_Linear # 85.11% accuracy on training set

# predict on test set
test_pred <- predict(svm_Linear, newdata = test)
confusionMatrix(test_pred, test$genre_rap) # 83.41%



### Another approach: sum up the embedding vectors for each word in the lyrics component-wise
dat_model_new4 <- dat_model %>% 
  group_by(song_name, genre) %>% 
  unnest_tokens(word, lyrics) %>% 
  anti_join(stop_words, by = "word") %>% 
  inner_join(dict, by = "word") %>% 
  mutate(genre_rap = ifelse(genre == "rap", 1, 0)) %>% 
  unique(.) %>% 
  spread(word, vec, fill = 0) 

# sum up embedding vectors
dat_model_new4$sumVec <- rowSums(dat_model_new4[, 3:11183])
dat_model_new4 <- dat_model_new4 %>% select(song_name, genre_rap, sumVec)
dat_model_new4$genre_rap <- as.factor(dat_model_new4$genre_rap)
dat_model_new4$genre <- NULL

## 75% of the sample size
smp_size <- floor(0.75 * nrow(dat_model_new4))

## set the seed and split to train / test
set.seed(123)
train_ind <- sample(seq_len(nrow(dat_model_new4)), size = smp_size)
train <- dat_model_new4[train_ind, ]
test <- dat_model_new4[-train_ind, ]

# use svmLinear method to train model
svm_Linear <- train(genre_rap ~., data = train[, -1], method = "svmLinear",
                    trControl = trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)

svm_Linear # 79.34% accuracy on training set

# predict on test set
test_pred <- predict(svm_Linear, newdata = test)
confusionMatrix(test_pred, test$genre_rap) # 82.46%
