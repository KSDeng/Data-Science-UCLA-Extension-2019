library(tidyverse)
library(dplyr)
library(ggplot2)
library(caret)
library(car)

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
summary(logistic_model$deviance)
#check the residual calculation

#deviance residuals-manual calculation
model_deviance_residuals <- function(model, data, output_column) {
  y_labels = data[[output_column]]
  y_probs = predict(model, newdata = data, type = "response")
  residual_sign = sign(y_labels - y_probs)
  residuals = sqrt(deviances(y_labels, y_probs))
  residual_sign * residuals
}
#check with model
summary(model_deviance_residuals(logistic_model, 
                                 data = vegas_train,
                                 output_column = "Score_Over4"))

summary(logistic_model)

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
library(caret)
#training
confusionMatrix(table(factor(vegas_train$Score_Over4),
                      factor(train_class_predictions)),
                positive="1")

#test
confusionMatrix(table(factor(vegas_test$Score_Over4),
                      factor(test_class_predictions)),
                positive="1")

#curves
#install.packages("ROCR")
library(ROCR)
#precision-recall curve
train_predictions <- predict(logistic_model, 
                             newdata = vegas_train,
                             type = "response")
pred <- prediction(train_predictions, 
                   vegas_train$Score_Over4)
perf <- performance(pred,
                    measure = "prec", 
                    x.measure = "rec")
plot(perf)

?performance

#ROC curve
ROC <- performance(pred, measure="tpr", x.measure="fpr")
plot(ROC)

#AUC
auc_ROCR <- performance(pred, measure = "auc")
auc_ROCR <- auc_ROCR@y.values[[1]]
auc_ROCR

#what if we wanted a specific accuracy?
thresholds <- data.frame(cutoffs = perf@alpha.values[[1]], 
                         recall = perf@x.values[[1]], 
                         precision = perf@y.values[[1]])
subset(thresholds,(recall > 0.9) & (precision > 0.9))

#not possible
#recall is also called sensitivity
recall = 115/(115+53)
recall
#precision
precision = 115/(115+59)
precision
subset(thresholds,(recall > 0.6) & (precision > 0.7))

############################################
############################################
#VARIABLE SELECTION
############################################
############################################
library(MASS)
#Fit the model
logistic_model_step <- glm(Score_Over4~Member.years+
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
             family = binomial("logit")) %>%
  #select based on AIC
  stepAIC(trace = FALSE,
          direction="both")

summary(logistic_model_step)

#WE COULD HAVE DONE THIS
steps1<-stepAIC(logistic_model,
    trace = FALSE,
    direction="both")
coef(steps1)
steps2<-stepAIC(logistic_model,
               trace = FALSE,
               direction="forward")
coef(steps2)
steps3<-stepAIC(logistic_model,
               trace = FALSE,
               direction="backward")
coef(steps3)

#selected variables
coef(logistic_model_step)

#Summarize the final selected model
summary(model)
# Make predictions
probabilities <- model %>% predict(test.data, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "pos", "neg")
# Model accuracy
mean(predicted.classes==test.data$diabetes)

##############################
#PREDICTIONS
##############################
#training
train_predictions <- predict(logistic_model_step, newdata = vegas_train,
                             type = "response")
train_class_predictions <- as.numeric(train_predictions > 0.5)
mean(train_class_predictions == vegas_train$Score_Over4)
#68% accuracy

#test
test_predictions <- predict(logistic_model_step, newdata = vegas_test,
                            type = "response")
test_class_predictions <- as.numeric(test_predictions > 0.5)
mean(test_class_predictions == vegas_test$Score_Over4)
#52% accuracy

#training
confusionMatrix(table(factor(vegas_train$Score_Over4),
                      factor(train_class_predictions)),
                positive="1")

#test
confusionMatrix(table(factor(vegas_test$Score_Over4),
                      factor(test_class_predictions)),
                positive="1")

#some additional techniques
table(vegas_train$Score)
table(vegas_test$Score)

vegas_train<-vegas_train %>%
  mutate(Score_123=factor(ifelse(Score<=3,1,
                                 ifelse(Score==4,
                                        2,
                                        3))))
table(vegas_train$Score,vegas_train$Score_123)

summary(vegas_train)

vegas_test<-vegas_test %>%
  mutate(Score_123=factor(ifelse(Score<=3,1,
                          ifelse(Score==4,
                                 2,
                                 3))))
table(vegas_test$Score,vegas_test$Score_123)

#multinomial
#(maxit sets the maximum number of iterations)
library(nnet)
multi_model <- multinom(Score_123~Member.years+
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
                        maxit = 1000)
summary(multi_model)

#z-statistic
z_p <- data.frame(var=summary(multi_model)$coefnames,
                  z=as.numeric(summary(multi_model)$coefficients/
  summary(multi_model)$standard.errors))
glimpse(z_p)

#p-values
z_p$p <- (1 - pnorm(abs(z_p$z), 0, 1)) * 2
z_p$sig<-ifelse(z_p$p<0.05,"significant","not significant")
z_p

#check out the predicted probabilities
head(pp <- fitted(multi_model))

vegas_multi_predictions_train <- predict(multi_model, vegas_train)
mean(vegas_multi_predictions_train == vegas_train$Score_123)
#56%

vegas_multi_predictions_test <- predict(multi_model, vegas_test)
mean(vegas_multi_predictions_test == vegas_test$Score_123)
#41%

table(predicted = vegas_multi_predictions_train, 
      actual = vegas_train$Score_123)

table(predicted = vegas_multi_predictions_test, 
      actual = vegas_test$Score_123)

#ordinal logistic
library(MASS)
ordinal_model <- polr(Score_123~Member.years+
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
                   Hess = T)
summary(ordinal_model)
names(ordinal_model)

#check out the predicted probabilities
head(pp_ordinal <- fitted.values(ordinal_model))

vegas_ord_predictions_train <- predict(ordinal_model, 
                                         vegas_train)
mean(vegas_ord_predictions_train == vegas_train$Score_123)
#54%

vegas_ord_predictions_test <- predict(ordinal_model, 
                                       vegas_test)
mean(vegas_ord_predictions_test == vegas_test$Score_123)
#45%

table(predicted = vegas_ord_predictions_train, 
      actual = vegas_train$Score_123)

table(predicted = vegas_ord_predictions_test, 
      actual = vegas_test$Score_123)

#compare
table(predicted = vegas_multi_predictions_train, 
      actual = vegas_train$Score_123)

table(predicted = vegas_multi_predictions_test, 
      actual = vegas_test$Score_123)