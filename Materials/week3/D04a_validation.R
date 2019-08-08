######################################################################################################
# D04a Cross-Validation
# By William Yu, UCLA Anderson Forecast
# 7/12/2019
##################################################################################################### 
setwd("C:/Users/wyu/documents/zip08/2019 Q3 Summer_XData/Data")

##
## Chapter 5 Lab : Cross-Validation and the Bootstrap
##

# The Validation Set Approach
library(ISLR)
library(ggplot2)
library(corrplot)

?Auto    # Gas mileage, horsepower for 392 vehicles
Auto=Auto
str(Auto)
head(Auto)
attach(Auto)
plot(horsepower, mpg, pch=20)

fit00=lm(mpg~.-name-origin+factor(origin),data=Auto)
summary(fit00)

fit01=lm(mpg~horsepower,data=Auto)
summary(fit01)
fit02=lm(mpg~poly(horsepower,2),data=Auto)
summary(fit02)
fit03=lm(mpg~poly(horsepower,3),data=Auto)
summary(fit03)

corrplot(cor(Auto[,-9]), type="lower", method="number")

ggplot(Auto, aes(x=horsepower,y=mpg))+
  geom_point()+
  geom_line(aes(y=predict(fit01)),color="black", size=1)+
  geom_line(aes(y=predict(fit02)),color="blue", size=1)+
  geom_line(aes(y=predict(fit03)),color="red", size=1)

?sample
what=sample(20,10)
what

set.seed(10)
train=sample(392,196)
train
lm.fit=lm(mpg~horsepower,data=Auto,subset=train)
summary(lm.fit)

# Calculating MSE (Mean squared error)
mean((mpg-predict(lm.fit,Auto))[-train]^2)

lm.fit2=lm(mpg~poly(horsepower,2),data=Auto,subset=train)
mean((mpg-predict(lm.fit2,Auto))[-train]^2)

lm.fit3=lm(mpg~poly(horsepower,3),data=Auto,subset=train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2)

set.seed(3)
train=sample(392,196)
lm.fit=lm(mpg~horsepower,subset=train)
mean((mpg-predict(lm.fit,Auto))[-train]^2)
lm.fit2=lm(mpg~poly(horsepower,2),data=Auto,subset=train)
mean((mpg-predict(lm.fit2,Auto))[-train]^2)
lm.fit3=lm(mpg~poly(horsepower,3),data=Auto,subset=train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2)

##
## Leave-One-Out Cross-Validation
##
glm.fit=glm(mpg~horsepower,data=Auto) # linear model
summary(glm.fit)

install.packages("boot")
library(boot)

glm.fit=glm(mpg~horsepower,data=Auto)
cv.err=cv.glm(Auto,glm.fit)
cv.err$delta

# Run polynoimal model 1 through 5
cv.error=rep(0,5)
for (i in 1:5){
  glm.fit=glm(mpg~poly(horsepower,i),data=Auto)
  cv.error[i]=cv.glm(Auto,glm.fit)$delta[1]
}
cv.error

##
## 10-Fold Cross-Validation
##
set.seed(17)
glm.fit=glm(mpg~horsepower,data=Auto)
cv.err.10=cv.glm(Auto,glm.fit,K=10)
cv.err.10$delta[1]

cv.error.10=rep(0,5)
for (i in 1:5){
  glm.fit=glm(mpg~poly(horsepower,i),data=Auto)
  cv.error.10[i]=cv.glm(Auto,glm.fit,K=10)$delta[1]
}
cv.error.10