#install.packages("DataExplorer") - install this package
#install.packages("DataExplorer")
library(DataExplorer)
#install.packages("tidyverse")
library(tidyverse)
#install.packages("dplyr")
library(dplyr)
#install.packages("ggplot2")
library(ggplot2)

#setwd - set working directory or folder path
#notice the direction of the slashes
setwd("/Users/alfonsoberumen/Desktop/UCLA JESIE")

#load saved data
vegas_final<-readRDS("vegas_modified.rds")

################################################
#partition data based on training and test
################################################
set.seed(1234)
#80/20 split
training_test <- c(rep(1,length=trunc((8/10)*nrow(vegas_final))),
                   rep(2,length=(nrow(vegas_final) - trunc((8/10)*nrow(vegas_final)))))

vegas_final$training_test <- sample(training_test) #random permutation 
vegas_final$training_test <- factor(vegas_final$training_test, 
                                       levels=c(1,2), labels=c("TRAIN","TEST"))
table(vegas_final$training_test)
#test~20%
95/472

#training
vegas_train <- subset(vegas_final, training_test == "TRAIN")
glimpse(vegas_train)
summary(vegas_train)
#test
vegas_test <- subset(vegas_final, training_test == "TEST")
glimpse(vegas_test)
summary(vegas_test)

######################
######################
#PREP THE DATA
######################
######################
table(vegas_final$Score)
#below 5
(11+29+66+151)/472
#~50%

vegas_train<-vegas_train %>%
  mutate(Score_Over4=ifelse(Score>4,1,0))
summary(vegas_train)

vegas_test<-vegas_test %>%
  mutate(Score_Over4=ifelse(Score>4,1,0))
summary(vegas_test)

#fairly even
table(vegas_train$Score_Over4)
174/(203+174)
table(vegas_test$Score_Over4)
41/(54+41)

############################################
############################################
#PRELIMINARY LOOK AT LOGISTIC
############################################
############################################
logistic_model <- glm(Score_Over4~Member.years+
                      Nr..reviews+
                      Nr..hotel.reviews+
                      Helpful.votes+
                      Period.of.stay+
                      Traveler.type+
                      Pool+
                      Gym+
                      Tennis.court+
                      Spa+
                      Casino+
                      Free.internet+
                      Hotel_Stars_Consol+
                      Continent_Consol, 
                      data = vegas_train, 
                      family = binomial("logit"))
summary(logistic_model)
vif(logistic_model)

#log-likelihood
log_likelihoods <- function(y_labels, y_probs) {
  y_a <- as.numeric(y_labels)
  y_p <- as.numeric(y_probs)
  y_a * log(y_p) + (1 - y_a) * log(1 - y_p)
}
dataset_log_likelihood <- function(y_labels, y_probs) {
  sum(log_likelihoods(y_labels, y_probs))
}
#deviances
deviances <- function(y_labels, y_probs) {
  -2 * log_likelihoods(y_labels, y_probs)
}
dataset_deviance <- function(y_labels, y_probs) {
  sum(deviances(y_labels, y_probs))
}

#model deviance
model_deviance <- function(model, data, output_column) {
  y_labels = data[[output_column]]
  y_probs = predict(model, newdata = data, type = "response")
  dataset_deviance(y_labels, y_probs)
}
model_deviance(logistic_model, 
               data = vegas_train, 
               output_column = "Score_Over4")

#null deviance
null_deviance <- function(data, output_column) {
  y_labels <- data[[output_column]]
  y_probs <- mean(data[[output_column]])
  dataset_deviance(y_labels, y_probs)
}
null_deviance(data = vegas_train, 
              output_column = "Score_Over4")

#pseudo r-squared
model_pseudo_r_squared <- function(model, data, output_column) {
  1 - ( model_deviance(model, data, output_column) /
          null_deviance(data, output_column) )
}
model_pseudo_r_squared(logistic_model, data = vegas_train,
                       output_column = "Score_Over4")
#Our logistic regression model is said 
#to explain roughly 18% 
#of the null deviance

##############################
#PREDICTIONS
##############################
#training
train_predictions <- predict(logistic_model, newdata = vegas_train,
                             type = "response")
train_class_predictions <- as.numeric(train_predictions > 0.5)
mean(train_class_predictions == vegas_train$Score_Over4)
#70% accuracy

#test
test_predictions <- predict(logistic_model, newdata = vegas_test,
                             type = "response")
test_class_predictions <- as.numeric(test_predictions > 0.5)
mean(test_class_predictions == vegas_test$Score_Over4)
#53% accuracy

#CONFUSION MATRIX
#using the caret package

# 用caret包对logistic模型的结果进行验证
library(caret)
#training
confusionMatrix(table(factor(vegas_train$Score_Over4),
                      factor(train_class_predictions)),
                positive="1")

#test
confusionMatrix(table(factor(vegas_test$Score_Over4),
                      factor(test_class_predictions)),
                positive = "1")
