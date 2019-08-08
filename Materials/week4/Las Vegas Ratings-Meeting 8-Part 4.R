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

table(vegas_train$Score,vegas_train$Score_Over4)

############################################
############################################
#PART 1: Decision Tree Based on Regression
############################################
############################################
#for a regression tress
#install.packages("rpart")
library(rpart)
#install.packages("rattle")
library(rattle)
#install.packages("rpart.plot")
library(rpart.plot)
#install.packages("RColorBrewer")
library(RColorBrewer)

#tree
regtree <- rpart(Score~Member.years+
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
                 data = vegas_train)

plot(regtree, uniform = TRUE)
text(regtree, use.n = FALSE, all = TRUE, cex = 1)

#wow, that can't be read
#fancy plot (rattle)
fancyRpartPlot(regtree,cex=0.3)

#variable importance
library(dplyr)
var_importance<-data.frame(regtree$variable.importance)
var_importance <- tibble::rownames_to_column(var_importance, 
                                             "VARIABLE")
glimpse(var_importance)

library(ggplot2)
#barplot of variable importance
p<-ggplot(data=var_importance, aes(x=reorder(VARIABLE,
                                             regtree.variable.importance), 
                               y=regtree.variable.importance)) +
  geom_bar(stat="identity",fill="blue")
p+coord_flip()

######################
#PREDICTED VALUES
######################
#training prediction
vegas_train$reg_predicted<-predict(regtree,vegas_train)
vegas_train$reg_residual<-vegas_train$Score-vegas_train$reg_predicted

#WHAT'S A RESIDUAL?
head(vegas_train,n=10)  
#MAKES SENSE NOW
check<- vegas_train %>%
  filter(reg_predicted<3.0)

#test prediction
vegas_test$reg_predicted<-predict(regtree,vegas_test)
vegas_test$reg_residual<-vegas_test$Score-vegas_test$reg_predicted
head(vegas_test,n=10) 

#accuracy measures
library(MLmetrics)
#training error
regression_training_MSE<-MLmetrics::MSE(y_true=vegas_train$Score,
                                        y_pred=vegas_train$reg_predicted)

regression_training_RMSE<-MLmetrics::RMSE(y_true=vegas_train$Score,
                                          y_pred=vegas_train$reg_predicted)

regression_training_MAE<-MLmetrics::MAE(y_true=vegas_train$Score,
                                        y_pred=vegas_train$reg_predicted)

regression_training_MAPE<-MLmetrics::MAPE(y_true=vegas_train$Score,
                                          y_pred=vegas_train$reg_predicted)

#error summary
regression_results_training<-data.frame(regression_training_MSE,
                                        regression_training_RMSE,
                                        regression_training_MAE,
                                        regression_training_MAPE)
regression_results_training

#test error
regression_test_MSE<-MLmetrics::MSE(y_true=vegas_test$Score,
                                    y_pred=vegas_test$reg_predicted)

regression_test_RMSE<-MLmetrics::RMSE(y_true=vegas_test$Score,
                                      y_pred=vegas_test$reg_predicted)

regression_test_MAE<-MLmetrics::MAE(y_true=vegas_test$Score,
                                    y_pred=vegas_test$reg_predicted)

regression_test_MAPE<-MLmetrics::MAPE(y_true=vegas_test$Score,
                                      y_pred=vegas_test$reg_predicted)

#error summary
regression_results_test<-data.frame(regression_test_MSE,
                                    regression_test_RMSE,
                                    regression_test_MAE,
                                    regression_test_MAPE)

#compare
regression_results_training
regression_results_test

#maybe try pruning?
printcp(regtree) #display the results
plotcp(regtree) #visualize cross-validation results
summary(regtree) #detailed summary of splits

regtree$cptable[which.min(regtree$cptable[,"xerror"]),"CP"]
pfit<- prune(regtree, cp=0.02368707)
fancyRpartPlot(pfit)

######################
#PREDICTED VALUES
######################
#training prediction
vegas_train$reg_predicted<-predict(pfit,vegas_train)
vegas_train$reg_residual<-vegas_train$Score-vegas_train$reg_predicted
#WHAT'S A RESIDUAL?
head(vegas_train,n=10)  
#MAKES SENSE NOW

#test prediction
vegas_test$reg_predicted<-predict(pfit,vegas_test)
vegas_test$reg_residual<-vegas_test$Score-vegas_test$reg_predicted
head(vegas_test,n=10) 

#accuracy measures
library(MLmetrics)
#training error
regression_training_MSE<-MLmetrics::MSE(y_true=vegas_train$Score,
                                        y_pred=vegas_train$reg_predicted)

regression_training_RMSE<-MLmetrics::RMSE(y_true=vegas_train$Score,
                                          y_pred=vegas_train$reg_predicted)

regression_training_MAE<-MLmetrics::MAE(y_true=vegas_train$Score,
                                        y_pred=vegas_train$reg_predicted)

regression_training_MAPE<-MLmetrics::MAPE(y_true=vegas_train$Score,
                                          y_pred=vegas_train$reg_predicted)

#error summary
regression_results_training<-data.frame(regression_training_MSE,
                                        regression_training_RMSE,
                                        regression_training_MAE,
                                        regression_training_MAPE)
regression_results_training

#test error
regression_test_MSE<-MLmetrics::MSE(y_true=vegas_test$Score,
                                    y_pred=vegas_test$reg_predicted)

regression_test_RMSE<-MLmetrics::RMSE(y_true=vegas_test$Score,
                                      y_pred=vegas_test$reg_predicted)

regression_test_MAE<-MLmetrics::MAE(y_true=vegas_test$Score,
                                    y_pred=vegas_test$reg_predicted)

regression_test_MAPE<-MLmetrics::MAPE(y_true=vegas_test$Score,
                                      y_pred=vegas_test$reg_predicted)

#error summary
regression_results_test<-data.frame(regression_test_MSE,
                                    regression_test_RMSE,
                                    regression_test_MAE,
                                    regression_test_MAPE)

#compare
regression_results_training
regression_results_test

################################################
################################################
#PART 2: Decision Tree Based on Classification
################################################
################################################
classtree <- rpart(Score_Over4~Member.years+
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
                   method = "class",
                 data = vegas_train)

#fancy plot (rattle)
fancyRpartPlot(classtree)

#pruning?
printcp(classtree) # display the results
plotcp(classtree) # visualize cross-validation results
#summary(classtree) # detailed summary of splits
classtree$cptable[which.min(classtree$cptable[,"xerror"]),"CP"]

class_pfit<-
  prune(classtree, cp=classtree$cptable[which.min(classtree$cptable[,"xerror"]),"CP"])

fancyRpartPlot(class_pfit)

##############################
#PREDICTIONS
##############################
#training
train_predictions <- predict(class_pfit, newdata = vegas_train,
                             type = "class")
mean(train_predictions == vegas_train$Score_Over4)
#64% accuracy

#test
test_predictions <- predict(class_pfit, newdata = vegas_test,
                             type = "class")
mean(test_predictions == vegas_test$Score_Over4)
#57% accuracy

#Confusion matrix
#training
confusionMatrix(table(factor(vegas_train$Score_Over4),
                      factor(train_predictions)),
                positive="1")

#test
confusionMatrix(table(factor(vegas_test$Score_Over4),
                      factor(test_predictions)),
                positive="1")