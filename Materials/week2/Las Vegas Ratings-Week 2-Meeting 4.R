rm(list = ls())
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
#setwd("/Users/alfonsoberumen/Desktop/UCLA JESIE")

#import the CSV file
vegas <- read.csv("LasVegasTripAdvisorReviews-Dataset.csv", header=TRUE, sep=",") 
str(vegas)

dplyr::glimpse(vegas)

#data profiling
#create_report(vegas)
#specific to the target we are studying
#create_report(vegas,y="Score")

#take a step back and learn this
summary(vegas)

#EDA: our target
#boxplot
boxplot(vegas$Score)
score_boxplot<-ggplot(data=vegas, aes(x="", y=Score, 
                              color=Score)) +
  geom_boxplot()
score_boxplot

#frequency
#counts
score_frequency <- ggplot(vegas, aes(Score)) + 
  geom_bar()
score_frequency 

##############################
#EDA: categorical variables
##############################
#User country
# 坐标映射中x轴映射到某个变量, y轴未指定时默认为该变量的频数
# coord_flip()函数旋转x轴和y轴
# color参数指定条形图的边缘颜色, fill指定条形图的填充颜色
country_plot<-ggplot(vegas, aes(User.country)) +
  geom_bar(color = "red",
          fill = "white")+
  coord_flip()

country_plot
# table()函数用于统计变量的不同取值及其频数, 并返回一个"table"类型的变量
table(vegas$User.country)

#continent
user_continent_plot<-ggplot(vegas, 
                            aes(x=User.continent, 
                                y=Score, 
                                color=User.continent)) +
  geom_boxplot()
user_continent_plot

# geom_text()为图形增加文本标注, stat="count"指定标注信息为频数
# position = position_stack()指定标注位置为每段条形的顶部
user_continent_plot2<-ggplot(vegas, aes(fill=as.factor(Score),
                                        User.continent)) + 
  geom_bar()+
  geom_text(stat = "count",
            aes(label=..count..),
            position=position_stack())
user_continent_plot2

user_continent_plot3<-ggplot(vegas, aes(fill=as.factor(Score),
                                        User.continent)) + 
  geom_bar(stat = "count",position="fill")
user_continent_plot3
xtabs(~vegas$Score+vegas$User.continent)

#America
144/(144+86+39+20+6)
#South America
4/(4+2+1+0+0)

#addtional check
#install.packages("psych")
library(psych)
psych::describeBy(vegas$Score, 
                  vegas$User.continent, 
                  mat = TRUE)

#Period of stay
period_plot<-ggplot(vegas, 
                    aes(x=Period.of.stay, 
                        y=Score, 
                              color=Period.of.stay)) +
  geom_boxplot()
period_plot

period_plot2<-ggplot(vegas, aes(fill=as.factor(Score),
                                Period.of.stay)) + 
  geom_bar(stat = "count",position="fill")
period_plot2
xtabs(~vegas$Score+vegas$Period.of.stay)

#install.packages("psych")
library(psych)
#addtional check
psych::describeBy(vegas$Score, 
           vegas$Period.of.stay, 
           mat = TRUE) 

#Review month
month_plot<-ggplot(vegas, 
                   aes(x=Review.month, 
                       y=Score, 
                       color=Review.month)) +
  geom_boxplot()
month_plot

month_plot2<-ggplot(vegas, aes(fill=as.factor(Score),
                               Review.month)) + 
  geom_bar(stat = "count",position="fill")
month_plot2
#cross tab
xtabs(~vegas$Score+vegas$Review.month)

#addtional check
psych::describeBy(vegas$Score, 
                  vegas$Review.month, 
                  mat = TRUE)

#Review weekday
weekday_plot<-ggplot(vegas, 
                     aes(x=Review.weekday, 
                         y=Score, 
                         color=Review.weekday)) +
  geom_boxplot()
weekday_plot

weekday_plot2<-ggplot(vegas, aes(fill=as.factor(Score),
                                 Review.weekday)) + 
  geom_bar(stat = "count",position="fill")
weekday_plot2
#cross tab
xtabs(~vegas$Score+vegas$Review.weekday)

#addtional check
psych::describeBy(vegas$Score, 
                  vegas$Review.weekday, 
                  mat = TRUE)

#Traveler type
traveler_plot<-ggplot(vegas, 
                    aes(x=Traveler.type, 
                        y=Score, 
                        color=Traveler.type)) +
  geom_boxplot()
traveler_plot

traveler_plot2<-ggplot(vegas, aes(fill=as.factor(Score),
                                  Traveler.type)) + 
  geom_bar(stat = "count",position="fill")
traveler_plot2
xtabs(~vegas$Score+vegas$Traveler.type)

#addtional check
psych::describeBy(vegas$Score, 
                  vegas$Traveler.type, 
                  mat = TRUE) 

#Pool
pool_plot<-ggplot(vegas, 
                      aes(x=Pool, 
                          y=Score, 
                          color=Pool)) +
  geom_boxplot()
pool_plot

pool_plot2<-ggplot(vegas, aes(fill=as.factor(Score),
                              Pool)) + 
  geom_bar(stat = "count",position="fill")
pool_plot2
#cross tab
xtabs(~vegas$Score+vegas$Pool)

#addtional check
psych::describeBy(vegas$Score, 
                  vegas$Pool, 
                  mat = TRUE) 

#Gym
gym_plot<-ggplot(vegas, 
                  aes(x=Gym, 
                      y=Score, 
                      color=Gym)) +
  geom_boxplot()
gym_plot

gym_plot2<-ggplot(vegas, aes(fill=as.factor(Score),
                              Gym)) + 
  geom_bar(stat = "count",position="fill")
gym_plot2
#cross tab
xtabs(~vegas$Score+vegas$Gym)

#addtional check
psych::describeBy(vegas$Score, 
                  vegas$Gym, 
                  mat = TRUE) 

#Tennis court
tennis_plot<-ggplot(vegas, 
                 aes(x=Tennis.court, 
                     y=Score, 
                     color=Tennis.court)) +
  geom_boxplot()
tennis_plot

tennis_plot2<-ggplot(vegas, aes(fill=as.factor(Score),
                                Tennis.court)) + 
  geom_bar(stat = "count",position="fill")
tennis_plot2
#cross tab
xtabs(~vegas$Score+vegas$Tennis.court)

#addtional check
psych::describeBy(vegas$Score, 
                  vegas$Tennis.court, 
                  mat = TRUE)

#Spa
spa_plot<-ggplot(vegas, 
                    aes(x=Spa, 
                        y=Score, 
                        color=Spa)) +
  geom_boxplot()
spa_plot

spa_plot2<-ggplot(vegas, aes(fill=as.factor(Score),
                             Spa)) + 
  geom_bar(stat = "count",position="fill")
spa_plot2
#cross tab
xtabs(~vegas$Score+vegas$Spa)

#addtional check
psych::describeBy(vegas$Score, 
                  vegas$Spa, 
                  mat = TRUE)

#Casino
casino_plot<-ggplot(vegas, 
                 aes(x=Casino, 
                     y=Score, 
                     color=Casino)) +
  geom_boxplot()
casino_plot

casino_plot2<-ggplot(vegas, aes(fill=as.factor(Score),
                                Casino)) + 
  geom_bar(stat = "count",position="fill")
casino_plot2
#cross tab
xtabs(~vegas$Score+vegas$Casino)

#addtional check
psych::describeBy(vegas$Score, 
                  vegas$Casino, 
                  mat = TRUE)

#Free internet
internet_plot<-ggplot(vegas, 
                    aes(x=Free.internet, 
                        y=Score, 
                        color=Free.internet)) +
  geom_boxplot()
internet_plot

internet_plot2<-ggplot(vegas, aes(fill=as.factor(Score),
                                  Free.internet)) + 
  geom_bar(stat = "count",position="fill")
internet_plot2
#cross tab
xtabs(~vegas$Score+vegas$Free.internet)

#addtional check
psych::describeBy(vegas$Score, 
                  vegas$Free.internet, 
                  mat = TRUE)

#Hotel name
hotel_plot<-ggplot(vegas, 
                      aes(x=Hotel.name, 
                          y=Score, 
                          color=Hotel.name)) +
  geom_boxplot()
hotel_plot

hotel_plot2<-ggplot(vegas, aes(fill=as.factor(Score),
                                  Hotel.name)) + 
  geom_bar(stat = "count",position="fill")+
  coord_flip()

hotel_plot2
#cross tab
#xtabs(~vegas$Score+vegas$Hotel.name)

#addtional check
psych::describeBy(vegas$Score, 
                  vegas$Hotel.name, 
                  mat = TRUE)

#Hotel stars
hotel_stars_plot<-ggplot(vegas, 
                   aes(x=Hotel.stars, 
                       y=Score, 
                       color=Hotel.stars)) +
  geom_boxplot()
hotel_stars_plot

hotel_stars_plot2<-ggplot(vegas, aes(fill=as.factor(Score),
                               Hotel.stars)) + 
  geom_bar(stat = "count",position="fill")
hotel_stars_plot2
#cross tab
xtabs(~vegas$Score+vegas$Hotel.stars)

#addtional check
psych::describeBy(vegas$Score, 
                  vegas$Hotel.stars, 
                  mat = TRUE)

#################################
#Up next: EDA: numeric variables
#################################
library(dplyr)
#Correlation matrix
glimpse(vegas)
#pull the numeric variables in here
vegas_num_feats <- select(vegas,
                          Nr..reviews,
                          Nr..hotel.reviews,
                          Helpful.votes,
                          Nr..rooms,
                          Member.years,
                          Score)

#install.packages("corrplot")
library(corrplot)
# 将变量之间的相关系数可视化
correlations = cor(vegas_num_feats)
corrplot(correlations, method = "circle")
# 将数字与图形结合, number.cex表示数字的放缩比例
corrplot.mixed(corr = correlations)
corrplot.mixed(correlations, number.cex = .7)
# 个人比较喜欢以下这种画法
# corr表示用于画图的数据, method表示画图的类型, type设置full, lower, upper分别对应全画、只画下三角、只画上三角
# diag设置是否画出对角线上的值, addCoef.col设置标出相关系数大小的颜色, 默认为不标出
corrplot(corr = cor(vegas_num_feats), method = "color", type = "lower", diag = FALSE, addCoef.col = "grey")


#Nr..reviews
ggplot(vegas, 
       aes(x="", 
           y=Nr..reviews, 
           color=Score)) +
  geom_boxplot()
summary(vegas$Nr..reviews)

ggplot(data=vegas, aes(x=Nr..reviews, y=Score)) + 
  geom_point()

ggplot(vegas, 
       aes(x=as.factor(Score), 
           y=Nr..reviews, 
           color=Score)) +
  geom_boxplot()

#addtional check
psych::describeBy(vegas$Nr..reviews, 
                  vegas$Score, 
                  mat = TRUE)

#Nr..hotel.reviews
ggplot(vegas, 
       aes(x="", 
           y=Nr..hotel.reviews, 
           color=Score)) +
  geom_boxplot()
summary(vegas$Nr..hotel.reviews)

ggplot(data=vegas, aes(x=Nr..hotel.reviews, y=Score)) + 
  geom_point()

ggplot(vegas, 
       aes(x=as.factor(Score), 
           y=Nr..hotel.reviews, 
           color=Score)) +
  geom_boxplot()

#addtional check
psych::describeBy(vegas$Nr..hotel.reviews, 
                  vegas$Score, 
                  mat = TRUE)

#Helpful.votes
ggplot(vegas, 
       aes(x="", 
           y=Helpful.votes, 
           color=Score)) +
  geom_boxplot()
summary(vegas$Helpful.votes)

ggplot(data=vegas, aes(x=Helpful.votes, y=Score)) + 
  geom_point()

ggplot(vegas, 
       aes(x=as.factor(Score), 
           y=Helpful.votes, 
           color=Score)) +
  geom_boxplot()

#addtional check
psych::describeBy(vegas$Helpful.votes, 
                  vegas$Score, 
                  mat = TRUE)

#Nr..rooms
ggplot(vegas, 
       aes(x="", 
           y=Nr..rooms, 
           color=Score)) +
  geom_boxplot()
summary(vegas$Nr..rooms)

ggplot(data=vegas, aes(x=Nr..rooms, y=Score)) + 
  geom_point()

ggplot(vegas, 
       aes(x=as.factor(Score), 
           y=Nr..rooms, 
           color=Score)) +
  geom_boxplot()

#addtional check
psych::describeBy(vegas$Nr..rooms, 
                  vegas$Score, 
                  mat = TRUE)

#Member.years
ggplot(vegas, 
       aes(x="", 
           y=Member.years, 
           color=Score)) +
  geom_boxplot()
summary(vegas$Member.years)

ggplot(data=vegas, aes(x=Member.years, y=Score)) + 
  geom_point()

ggplot(vegas, 
       aes(x=as.factor(Score), 
           y=Member.years, 
           color=Score)) +
  geom_boxplot()

#additional check
psych::describeBy(vegas$Member.years, 
                  vegas$Score, 
                  mat = TRUE)

########################
#Some Adjustments
########################
#make decisions about what you are willing to accept
#
ggplot(vegas, 
       aes(x="", 
           y=Nr..reviews)) +
  geom_boxplot(color="red")
summary(vegas$Nr..reviews)
#
ggplot(vegas, 
       aes(x="", 
           y=Nr..hotel.reviews)) +
  geom_boxplot(color="blue")
#
ggplot(vegas, 
       aes(x="", 
           y=Helpful.votes)) +
  geom_boxplot(color="orange")
#
ggplot(vegas, 
       aes(x="", 
           y=Member.years )) +
  geom_boxplot(color="purple")
summary(vegas$Member.years)

#adjustments
summary(vegas)
# 此处实际上为dataframe的一些操作技巧
# filter()按照给定条件筛选数据
# mutate()创建新的变量
# ifelse()参数类似于C++中的三元运算符
vegas_adjusted<-vegas %>%
  filter(Nr..reviews<=400) %>% #exclude reviews with over 300 reviews
  filter(Nr..hotel.reviews<=100) %>% #exclude hotel reviews with over 100 reviews
  filter(Helpful.votes<=250) %>% #exclude hotel reviews with over 100 reviews
  filter(Member.years>=0) %>% #exclude those with Member years<=0
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
((504-491)/504)*100  
#new variables: check that it worked
table(vegas_adjusted$Continent_Consol,vegas_adjusted$User.continent)
table(vegas_adjusted$Hotel_Stars_Consol,vegas_adjusted$Hotel.stars)
table(vegas_adjusted$Quarter,vegas_adjusted$Review.month)

summary(vegas_adjusted)

#drop variables
# select函数的参数前加上负号"-"表示去掉某些变量
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
99/491

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
# stargazer是一个格式化的输出工具, 可将模型结果以HTML, Latex, ASCII格式输出, 便于生成标准化的表格
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
# MLmetrics包中有一系列函数用于评估模型性能
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
regression_results_test

#visualize it
################################
#PLOT
################################
#set together training and test sets for plotting
plotting.frame <- rbind(vegas_train,vegas_test)

#generate predictive modeling visual
# lattice包用于高级绘图
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

######################################
#Up next: variable selection
######################################