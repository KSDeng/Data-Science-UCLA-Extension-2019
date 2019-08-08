######################################################################################################
# D03c Classification
# By William Yu, UCLA Anderson Forecast
# 7/12/2019
##################################################################################################### 
setwd("C:/Users/wyu/documents/zip08/2019 Q3 Summer_XData/Data")
install.packages("caret")
install.packages("pROC")
install.packages("e1071")
library(class)
library(ISLR)
library(MASS)
library(caret)
library(pROC)
library(e1071)

# An Application to Caravan Insurance Data
?Caravan   # 5822 observations and 86 variables
Caravan = Caravan  # import the data
str(Caravan)
attach(Caravan)
summary(Purchase)
348/5822

# Scaling variables
summary(Caravan)
?scale
standardized.X=scale(Caravan[,-86])
var(Caravan[,1])
var(Caravan[,2])
var(standardized.X[,1])
var(standardized.X[,2])
test=1:1000

train.X=standardized.X[-test,]
test.X=standardized.X[test,]
train.Y=Purchase[-test]
test.Y=Purchase[test]
set.seed(1)
knn.pred=knn(train.X,test.X,train.Y,k=1)
mean(test.Y!=knn.pred)
mean(test.Y!="No")
table(knn.pred,test.Y)
(873+9)/1000
knn.pred=knn(train.X,test.X,train.Y,k=3)
table(knn.pred,test.Y)
(920+5)/1000
knn.pred=knn(train.X,test.X,train.Y,k=5)
table(knn.pred,test.Y)
(930+4)/1000
knn.pred=knn(train.X,test.X,train.Y,k=10)
table(knn.pred,test.Y)
(941+1)/1000
knn.pred=knn(train.X,test.X,train.Y,k=50)
table(knn.pred,test.Y)
(941+0)/1000

# Logistic regression
glm.fits=glm(Purchase~.,data=Caravan,family=binomial,subset=-test)
summary(glm.fits)

glm.probs=predict(glm.fits,Caravan[test,],type="response")
glm.pred=rep("No",1000)
glm.pred[glm.probs>.5]="Yes"
table(glm.pred,test.Y)
934/1000

glm.pred=rep("No",1000)
glm.pred[glm.probs>.1]="Yes"
table(glm.pred,test.Y)
(746+28)/1000
28/(31+28)

##
## Confusion Matrix and ROC Curve
##
glm.pred=factor(glm.pred)
test.Y=factor(test.Y)

?confusionMatrix
conf1=confusionMatrix(glm.pred,test.Y, positive = "Yes")
conf1

glm.roc = roc(response = test.Y, predictor = as.numeric(glm.probs))
plot(glm.roc, legacy.axes = TRUE, print.auc.y = 1.0, print.auc = TRUE)

##
## Linear Discriminant Analysis
##
head(Smarket)
Smarket=Smarket

attach(Smarket)
train=(Year<2005)
Smarket.2005=Smarket[!train,]
Direction.2005=Direction[!train]

lda.fit=lda(Direction~Lag1+Lag2,data=Smarket,subset=train)
lda.fit
plot(lda.fit)
lda.pred=predict(lda.fit, Smarket.2005)
names(lda.pred)
lda.class=lda.pred$class
table(lda.class,Direction.2005)
mean(lda.class==Direction.2005)
sum(lda.pred$posterior[,1]>=.5)
sum(lda.pred$posterior[,1]<.5)
lda.pred$posterior[1:20,1]
lda.class[1:20]
sum(lda.pred$posterior[,1]>.9)


