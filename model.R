library(dplyr)
library(tidyverse)
library(tidytext)
#library(tm)
library(caret)
library(wordVectors)
library(magrittr)
library(forcats)

### There are 2 ways to predict whether a song is rap or not based on its lyrics: word frequency or word2vec

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


#********************* Section 3: word vector embedding by word2vec ************************#
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
