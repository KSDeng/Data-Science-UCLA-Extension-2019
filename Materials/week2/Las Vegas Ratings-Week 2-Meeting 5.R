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

#import the CSV file
vegas <- read.csv("LasVegasTripAdvisorReviews-Dataset.csv",
                 header=TRUE, sep=",") 
str(vegas)
dplyr::glimpse(vegas)

########################
#Some Adjustments
########################
vegas_adjusted<-vegas %>%
  filter(Nr..reviews<=200) %>% #exclude reviews with over 200 reviews
  filter(Nr..hotel.reviews<=50) %>% #exclude hotel reviews with over 50 reviews
  filter(Helpful.votes<=150) %>% #exclude hotel reviews with over 150 reviews
  filter(Member.years>=0) %>% #exclude those with Member years<0
  mutate(Continent_Consol=factor(ifelse(User.continent=="North America",
                                        "North America",
                                 ifelse(User.continent=="Europe","Europe"
                                        ,"Other")))) %>% #create new continent variable
  mutate(Hotel_Stars_Consol=factor(ifelse(Hotel.stars %in% c("3","3,5","4"),"<=4",">4")))%>% #consolidate the stars
  mutate(Quarter=factor(ifelse(Review.month %in% c("January",
                                                       "February",
                                                       "March"),"1",
                                   ifelse(Review.month %in% c("April",
                                                              "May",
                                                              "June"),"2",
                                   ifelse(Review.month %in% c("July",
                                                              "August",
                                                              "September"),"3",
                                          "4"))))) #create a qtr variable based on month

#Excluded: 
((504-472)/504)*100  
#new variables: check that it worked
table(vegas_adjusted$Continent_Consol,vegas_adjusted$User.continent)
table(vegas_adjusted$Hotel_Stars_Consol,vegas_adjusted$Hotel.stars)
table(vegas_adjusted$Quarter,vegas_adjusted$Review.month)

summary(vegas_adjusted)

#drop variables
vegas_final<-vegas_adjusted %>%
  select(-c(User.country,User.continent,Hotel.stars,Review.month))

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
######################
#MODELLING
######################
######################
######################
#MULTIPLE LINEAR REGRESSION
reg_model1 <- lm(Score~Member.years+
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
                  Quarter+
                  Continent_Consol, 
                 data = vegas_train)
summary(reg_model1)

#vif: test for multicollinearity
library(car)
vif(reg_model1)

#MULTIPLE LINEAR REGRESSION
reg_model2 <- lm(Score~Member.years+
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
summary(reg_model2)
vif(reg_model2)

#better output
library(stargazer)
stargazer(reg_model2,
          title="Results",
          align=TRUE,
          out="reg_table.html",
          digits=3)

######################
#PREDICTED VALUES
######################
#LET'S GO WITH MODEL 2
#training prediction
vegas_train$reg_predicted<-predict(reg_model2,vegas_train)
vegas_train$reg_residual<-vegas_train$Score-vegas_train$reg_predicted
#WHAT'S A RESIDUAL?
head(vegas_train,n=10)  
#MAKES SENSE NOW

#test prediction
vegas_test$reg_predicted<-predict(reg_model2,vegas_test)
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

regression_results_training
regression_results_test

#visualize it
################################
#PLOT
################################
#set together training and test sets for plotting
plotting.frame <- rbind(vegas_train,vegas_test)

#generate predictive modeling visual
library(lattice)

group.symbols <- c(21,21)
group.colors <- c("black","black") 
group.fill <- c("yellow","yellow")  
xyplot(reg_predicted ~ Score | training_test, 
       data = plotting.frame,cex = 1,
       pch = group.symbols, 
       col = group.colors, 
       fill = group.fill, 
       layout = c(2, 1), xlim = c(0,7), ylim = c(0,7), 
       aspect=1, type = c("p","g"),
       panel=function(x,y, ...)
       {panel.xyplot(x,y,...)
         panel.segments(0,0,7,7,col="blue",cex=1)
       },
       strip=function(...) strip.default(..., style=1),
       xlab = "Actual", 
       ylab = "Predicted")

plot(reg_model2)


######################################
#Up next: variable selection
######################################
#leaps package for variable selection

# 用leaps包进行参数选择, 作用类似于ols_step_best_subset(),个人觉得不如后者。
# install.packages("leaps")
library(leaps)
reg_model_bestsub=regsubsets(Score~Member.years+
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
summary(reg_model_bestsub)

#default is 8, let's use the maximum in our data
reg_model_bestsub=regsubsets(Score~Member.years+
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
                               data=vegas_train,
                               nvmax=50,
                               method="exhaustive")
#14 variables but it considers categories as well
bestsub_summary=summary(reg_model_bestsub)
bestsub_summary
names(bestsub_summary)
#do you recognize these?
bestsub_summary$rsq
bestsub_summary$adjr2

#Plot different criteria
#par(mar=c(2,2))
plot(bestsub_summary$rss,xlab="Number of Variables",ylab="RSS",type="l")
plot(bestsub_summary$rsq,xlab="Number of Variables",ylab="R-squared",type="l")
which.max(bestsub_summary$rsq)
points(20,bestsub_summary$adjr2[20], col="red",cex=2,pch=10)

plot(bestsub_summary$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="l")
which.max(bestsub_summary$adjr2)
points(13,bestsub_summary$adjr2[13], col="red",cex=2,pch=10)

plot(bestsub_summary$cp,xlab="Number of Variables",ylab="Cp",type='l')
which.min(bestsub_summary$cp)
points(10,bestsub_summary$cp[10],col="red",cex=2,pch=10)

which.min(bestsub_summary$bic)
plot(bestsub_summary$bic,xlab="Number of Variables",ylab="BIC",type='l')
points(5,bestsub_summary$bic[5],col="red",cex=2,pch=10)

#six variable model
coef(reg_model_bestsub,5)
coef(reg_model_bestsub,10)
coef(reg_model_bestsub,13)
coef(reg_model_bestsub,14)

#Forward and Backward Stepwise Selection
#forward
reg_model_forward=regsubsets(Score~Member.years+
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
                      data=vegas_train,
                      nvmax=20,method="forward")
summary(reg_model_forward)
summary(reg_model_forward)$adjr2#fit

#backward
reg_model_back=regsubsets(Score~Member.years+
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
                          data=vegas_train,
                          nvmax=20,method="backward")
summary(reg_model_back)
summary(reg_model_back)$adjr2#fit

#subset of variables
coef(reg_model_bestsub,5)
coef(reg_model_forward,5)
coef(reg_model_back,5)

######################################################
#Run the model based on the best 5 on the training set
######################################################
#MULTIPLE LINEAR REGRESSION
reg_model3 <- lm(Score~Traveler.type+
                   Pool+
                   Free.internet+
                   Hotel_Stars_Consol, 
                 data = vegas_train)
summary(reg_model3)
vif(reg_model3)

######################
#PREDICTED VALUES
######################
#LET'S GO WITH MODEL 3 NOW
#training prediction
vegas_train$reg_predicted<-predict(reg_model3,vegas_train)
vegas_train$reg_residual<-vegas_train$Score-vegas_train$reg_predicted
#WHAT'S A RESIDUAL?
head(vegas_train,n=10)  
#MAKES SENSE NOW

#test prediction
vegas_test$reg_predicted<-predict(reg_model3,vegas_test)
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

#VARIABLE SELECTION RESULTS: LESS VARAIBLES
regression_results_training
regression_results_test

################################
#LET'S SAVE THE MODIFIED DATASET
################################
saveRDS(vegas_final,file="vegas_modified.rds")