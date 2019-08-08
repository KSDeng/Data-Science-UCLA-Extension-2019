#####################################################################################################
# D03b. Missing Value Management  
# By William Yu, UCLA Anderson Forecast
# updated on 7/12/2019
##################################################################################################### 
# setwd("C:/Users/wyu/documents/zip08/2019 Q3 Summer_XData/Data")
rm(list = ls())
caschool = read.csv("W01d_caschool.csv")
summary(caschool)
str(caschool)

# 
fit05 = lm(api~ chci,data=caschool)
summary(fit05) 

## (1) Regression automatically delete those rows/observations with missing values 
## in one or more variables
# lm()函数的na.action参数默认值为na.omit, 即默认删除含有na值的行
fit05a = lm(api~ chci,data=caschool, na.action=na.omit)
summary(fit05a) 

# 733 observations deleted due to missingness

##
## (2) Deleting the variable
##
## If a paricular variable is having more missing values that rest of the variables 
## in the dataset, and, if by removing that one variable you can save many observations, 
## then you are better off without that variable unless it is a really important predictor that 
## makes a lot of business sense. It is a matter of deciding between the importance of the variable 
## and losing out on a number of observations.

##
## (3) Imputation with mean, median, mode, max, etc.
##
# install.packages("Hmisc")
library(Hmisc)
# impute()函数用于对NA值进行处理
caschool$api.1=impute(caschool$api, mean) # replace with mean, 用平均值替代
caschool$api.2=impute(caschool$api, median) # replace with median, 用中位数替代
caschool$api.3=impute(caschool$api, 300) # replace with specific number, 用指定的数值替代
summary(caschool)

##
## (4) KNN Imputation
##
# install.packages("DMwR")
library(DMwR)
caschool.knn=caschool
# Exclude the dependent/responsive variable: api
# knnImputation()函数用KNN算法对NA进行填充
knn1 = knnImputation(caschool.knn[, !names(caschool.knn) %in% "api"])   # 这里把"api"这一列删掉了
!names(caschool.knn) %in% "api"
summary(knn1)
anyNA(knn1)

caschool = knnImputation(caschool)
anyNA(caschool)

#Include all variables
knn2 =knnImputation(caschool.knn)
summary(knn2)

##
## (5) MICE (Multivariate Imputation by Chained Equations) 
##

# MICE的思想是用其它变量对缺失值进行拟合
# install.packages("mice")
library(mice)
?mice
caschool.1=caschool[,-1]
str(caschool.1)
md.pattern(caschool)      # A tabular form of missing value prsent in each variable
mice.1=mice(caschool.1)
mice.out1=complete(mice.1)
summary(mice.out1)


## 
## (6) na.interp (Good for time series)
##
# install.packages("forecast")
# 插值法对NA进行填充
library(forecast)
caschool$api.4=na.interp(caschool$api)
summary(caschool)


