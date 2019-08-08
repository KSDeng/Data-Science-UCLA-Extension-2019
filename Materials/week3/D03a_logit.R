######################################################################################################
# D03a Logistic Regression
# By William Yu, UCLA Anderson Forecast
# 7/12/2019
##################################################################################################### 
setwd("C:/Users/wyu/documents/zip08/2019 Q3 Summer_XData/Data")
library(corrplot)
library(readxl) 
library(ggplot2) 

# Simulation of Some Scatter Sharts
x1=rnorm(600, mean=10, sd=0.2)
y1=rnorm(600, mean=0, sd=0.2)
plot(x1,y1, pch=20)
abline(lm(y1 ~ x1), col="blue")

fit1=lm(y1~x1)
summary(fit1)

x2=rnorm(60, mean=10, sd=5)
y2=4+0.8*x2+rnorm(60, mean=0, sd=2)
plot(x2,y2,pch=20)
abline(lm(y2 ~ x2), col="blue")

fit2=lm(y2~x2)
summary(fit2)

xout=c(18,20,21)   # Create 3 outliers
yout=-5+0.5*xout

x3=c(x2,xout)
y3=c(y2,yout)

fit3=lm(y3~x3)
summary(fit3)

plot(x3,y3,pch=20)
abline(lm(y2 ~ x2), col="blue")
abline(lm(y3~x3),col="red")
text(20, 3,"Outliers")      # Adding texts onto the chart
text(19, 15,"Beta=0.5 with outliers")
text(19, 19,"Beta=0.8 without outliers")

##
## Robust Regression
## An alternative to least squares regression when data are contaminated with outliers 
## or influential observations. It can also be used for the purpose of detecting influential observations
##
library(MASS)
library(foreign)
cdata <- read.dta("https://stats.idre.ucla.edu/stat/data/crime.dta")
head(cdata)
str(cdata)
# crime: violent crime per 100,000 people. murder: murder per 1M people. pctmetro: % of people living in metro area.
# pctwhite: % of population that is white. pcths: % of population with a high school education or above. 
# poverty: % of population living under poverty line. single: % of population that are single parents.
cor(cdata[,-c(1,2)])
fit1=lm(crime~pctmetro+pctwhite+pcths+poverty+single,data=cdata) 
summary(fit1)
fit2=lm(crime~pctmetro+poverty+single,data=cdata) 
summary(fit2)

opar=par(mfrow=c(2,2), oma=c(0,0,1.1,0))
plot(fit2, las=1)

# Robust Regression
rbt1=rlm(crime~pctmetro+poverty+single,data=cdata) 
summary(rbt1)
names(rbt1)
# See the weight of each obersvation. If all observations have weights of 1, then it is linear regression (OLS)
weights=data.frame(state=cdata$state, resid=rbt1$resid, weight=rbt1$w)
weights1=weights[order(weights$weight),]
weights1

# Simulation of a Logistic function chart
y=seq(-5,5,0.1)
p1=exp(y)
p2=1/(1+exp(y))
p3=-exp(y)
p4=exp(-y)
p5=1/(1+exp(-y))
p6=exp(y)/(1+exp(y))

par(mfrow=c(3,2))  
plot(y,p1, type="l")
plot(y,p2, type="l")
plot(y,p3, type="l")
plot(y,p4, type="l")
plot(y,p5, type="l", main="Logistic function")
plot(y,p6, type="l", main="Logistic function")
par(mfrow=c(1,1))  

##
## Logistic Regression
## 
## To learn more models, you can see here: https://stats.idre.ucla.edu/other/dae/

admitg = read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")
# gre:GRE (Graduate Record Exam scores)
# rank: the prestige of the undergraduate college. 1 has the highest prestige.
str(admitg)
summary(admitg)
sapply(admitg,sd)

table(admitg$rank)
table(admitg$admit)

?xtabs
xtabs(~admit+rank,data=admitg)
corrplot(cor(admitg))
corrplot(cor(admitg), type="lower", method="number")

logit1 = glm(admit~gre+gpa+rank, data=admitg, family="binomial")
summary(logit1)   # AIC = 467
names(logit1)
# Calculate the probablity of admission for a student: GRE: 760, GPA:3.8, University rank: 2
# Probablity=exp(a+bx)/(1+exp(a+bx))
logit1$coefficients
logit1$coef

v_gre=100
v_gpa=1
v_rank=4
y=logit1$coef[1]+logit1$coef[2]*v_gre+logit1$coef[3]*v_gpa+logit1$coef[4]*v_rank
y
prob=exp(y)/(1+exp(y))
prob

# "response" will produce predicted probability
prob2=predict(logit1,data.frame(gre=100,gpa=1,rank=4), data=admitg,type="response")
prob2

# Convert rank to a facotr to indicate that rank should be treated as a categorical variable
admitg$rank=factor(admitg$rank)
logit2 = glm(admit~gre+gpa+rank, data=admitg, family="binomial")
summary(logit2)  # AIC=470

corrplot(cor(admitg), type="lower", method="number") # Doesn't work!

#
# Violin Charts: Compare distribtuions among catergory variables
#
# Reference: http://www.sthda.com/english/wiki/ggplot2-violin-plot-quick-start-guide-r-software-and-data-visualization
ggplot(admitg, aes(x=rank, y=gre)) + geom_violin()
ggplot(admitg, aes(x=admit, y=gre)) + geom_violin()
ggplot(admitg, aes(x=factor(admit), y=gre)) + geom_violin()
ggplot(admitg, aes(x=rank, y=gpa)) + geom_violin() + coord_flip()

#
# Logistic Regression: Heart data with Qualitative variables
#
         
heart = read_excel("W03a_heart.xlsx")
head(heart)
str(heart)

table(heart$Ca)
table(heart$ChestPain)
table(heart$Thal)
ggplot(heart, aes(x=factor(Sex), y=Chol)) + geom_violin()

logi.fit=glm(AHD~.,data=heart,family=binomial)
# Problem: AHD is a character

# Solution 1:
heart$AHD1 = factor(heart$AHD)
head(heart)
str(heart)

logi.fit=glm(AHD1~.,data=heart,family=binomial)
# Warning message:glm.fit: algorithm did not converge 

logi.fit=glm(AHD1~.-AHD,data=heart,family=binomial)
summary(logi.fit)

# Soultion 2:
heart$AHD2 <- ifelse(heart$AHD =="Yes", 1,0)       # Replace character "Yes","No" with 1, 0
str(heart)

logi.fit2=glm(AHD2~.-AHD-AHD1,data=heart,family=binomial)    # Ca is supposed to be numeric but turns into string because there are several NAs.
summary(logi.fit2)

# Change "Ca" to a nmueric variables
summary(heart)
sum(heart$Ca=="NA")
heart$Ca=as.numeric(gsub("NA",0,heart$Ca))
str(heart)
summary(heart)

logi.fit3=glm(AHD2~.-AHD-AHD1,data=heart,family=binomial)   
summary(logi.fit3)

#
# Logistic Regression: Stock market data
#
library(ISLR)
?Smarket
Smarket[1:10,]
Smarket=Smarket
dim(Smarket)
summary(Smarket)
str(Smarket)
cor(Smarket)
corrplot(cor(Smarket[,-9]),type="lower", method="number")

# Full sample regression
attach(Smarket)
plot(Volume)

glm.fit1=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Smarket,family=binomial)
summary(glm.fit1)

fit1=lm(Today~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Smarket)
summary(fit1)

coef(glm.fit1)
summary(glm.fit1)$coef
summary(glm.fit1)$coef[,4]
glm.prob1=predict(glm.fit1,type="response")
glm.prob1[1:10]
contrasts(Direction)
glm.pred1=rep("Down",1250)
glm.pred1[glm.prob1>.5]="Up"
table(glm.pred1,Direction)
(507+145)/1250
mean(glm.pred1==Direction)

# Train set regression
train=(Year<2005)
plot(train)
plot(!train)
Smarket.2005=Smarket[!train,]
Smarket.05 <- subset(Smarket, Year==2005) 
dim(Smarket.2005)
dim(Smarket.05)

Direction.2005=Direction[!train]
glm.fit2=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Smarket,family=binomial,subset=train)
summary(glm.fit2)

glm.prob2=predict(glm.fit2,Smarket.2005,type="response")
glm.pred2=rep("Down",252)
glm.pred2
glm.pred2[glm.prob2>.5]="Up"

table(glm.pred2,Direction.2005)
(77+44)/252
mean(glm.pred2==Direction.2005)
mean(glm.pred2!=Direction.2005)

glm.fit3=glm(Direction~Lag1+Lag2,data=Smarket,family=binomial,subset=train)
glm.prob3=predict(glm.fit3,Smarket.2005,type="response")
glm.pred3=rep("Down",252)
glm.pred3[glm.prob3>.5]="Up"
table(glm.pred3,Direction.2005)
(35+106)/252
mean(glm.pred3==Direction.2005)
106/(106+76)
predict(glm.fit3,newdata=data.frame(Lag1=c(1.2),Lag2=c(1.1)),type="response")

##
## K-Nearest Neighbors (KNN)
##
install.packages("class")
library(class)
train.X=cbind(Lag1,Lag2)[train,]
test.X=cbind(Lag1,Lag2)[!train,]
train.Direction=Direction[train]
#set.seed(1)
?knn
knn.pred=knn(train.X,test.X,train.Direction,k=1)
table(knn.pred,Direction.2005)
(83+43)/252
knn.pred=knn(train.X,test.X,train.Direction,k=3)
table(knn.pred,Direction.2005)
mean(knn.pred==Direction.2005)

knn.pred=knn(train.X,test.X,train.Direction,k=10)
table(knn.pred,Direction.2005)
mean(knn.pred==Direction.2005)

knn.pred=knn(train.X,test.X,train.Direction,k=100)
table(knn.pred,Direction.2005)
mean(knn.pred==Direction.2005)


