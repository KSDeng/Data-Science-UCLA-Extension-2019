######################################################################################################
# D04b Model Selection and Regularization 
# By William Yu, UCLA Anderson Forecast
# 7/12/2019
##################################################################################################### 
setwd("C:/Users/wyu/documents/zip08/2019 Q3 Summer_XData/Data")

##
## Chapter 6 Lab 1: Subset Selection Methods
##
## Best Subset Selection: Fully in-sample

library(ISLR)
Hitters=Hitters
?Hitters
str(Hitters)
summary(Hitters)
sum(is.na(Hitters$Salary))
Hitters=na.omit(Hitters)
dim(Hitters)
sum(is.na(Hitters))
str(Hitters)

install.packages("leaps")
library(leaps)

fit01 = lm(Salary~.,Hitters)
summary(fit01)

regfit.full=regsubsets(Salary~.,Hitters)
summary(regfit.full)
regfit.full=regsubsets(Salary~.,data=Hitters,nvmax=19)
summary(regfit.full)
reg.summary=summary(regfit.full)
names(reg.summary)

# Show R^2 increases from 0.32 to 0.55 when the RHS variables increase from 1 to 19.
reg.summary$rsq

par(mfrow=c(2,2))
plot(reg.summary$rss,xlab="Number of Variables",ylab="RSS",type="l")
plot(reg.summary$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="l")

which.max(reg.summary$adjr2)  # identify the location of the maximum point of a vector
points(11,reg.summary$adjr2[11], col="red",cex=2,pch=20)
plot(reg.summary$cp,xlab="Number of Variables",ylab="Cp",type='l')
which.min(reg.summary$cp) # Cp is AIC
points(10,reg.summary$cp[10],col="red",cex=2,pch=20)
which.min(reg.summary$bic)
plot(reg.summary$bic,xlab="Number of Variables",ylab="BIC",type='l')
points(6,reg.summary$bic[6],col="red",cex=2,pch=20)

plot(regfit.full,scale="r2")
plot(regfit.full,scale="adjr2")
plot(regfit.full,scale="Cp")
plot(regfit.full,scale="bic")

par(mfrow=c(1,1))

coef(regfit.full,6)

# Forward and Backward Stepwise Selection

regfit.fwd=regsubsets(Salary~.,data=Hitters,nvmax=19,method="forward")
summary(regfit.fwd)
regfit.bwd=regsubsets(Salary~.,data=Hitters,nvmax=19,method="backward")
summary(regfit.bwd)
coef(regfit.full,7)
coef(regfit.fwd,7)
coef(regfit.bwd,7)

library(MASS)
fit01 <-lm(Salary ~ ., data=Hitters)
summary(fit01)
fit02 <- stepAIC(fit01, direction="both")
summary(fit02)

##
## Choosing Among Models: In-sample and Out-of-sample--Train and Test Sets
##
set.seed(2)
train=sample(c(TRUE,FALSE), nrow(Hitters),rep=TRUE)
test=(!train)
regfit.best=regsubsets(Salary~.,data=Hitters[train,],nvmax=19)
test.mat=model.matrix(Salary~.,data=Hitters[test,])
val.errors=rep(NA,19)
for(i in 1:19){
  coefi=coef(regfit.best,id=i)
  pred=test.mat[,names(coefi)]%*%coefi
  val.errors[i]=mean((Hitters$Salary[test]-pred)^2)
}
val.errors
which.min(val.errors)
coef(regfit.best,10)

predict.regsubsets=function(object,newdata,id,...){
  form=as.formula(object$call[[2]])
  mat=model.matrix(form,newdata)
  coefi=coef(object,id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
}

regfit.best=regsubsets(Salary~.,data=Hitters,nvmax=19)
coef(regfit.best,10)
k=10
set.seed(1)
folds=sample(1:k,nrow(Hitters),replace=TRUE)
cv.errors=matrix(NA,k,19, dimnames=list(NULL, paste(1:19)))
for(j in 1:k){
  best.fit=regsubsets(Salary~.,data=Hitters[folds!=j,],nvmax=19)
  for(i in 1:19){
    pred=predict.regsubsets(best.fit,Hitters[folds==j,],id=i)
    cv.errors[j,i]=mean( (Hitters$Salary[folds==j]-pred)^2)
  }
}
mean.cv.errors=apply(cv.errors,2,mean)
mean.cv.errors
par(mfrow=c(1,1))
plot(mean.cv.errors,type='b')
reg.best=regsubsets(Salary~.,data=Hitters, nvmax=19)
coef(reg.best,11)

##
## Chapter 6 Lab 2: Ridge and Lasso Regression
##
## alpha = 0 Ridge Model
## alpha = 1 Lasso Model

install.packages("glmnet")
library(glmnet)
library(ISLR)

Hitters=na.omit(Hitters)
x=model.matrix(Salary~.,Hitters)[,-1]  # remove the intercept cofficient (1)
y=Hitters$Salary

# Ridge Regression

grid=10^seq(10,-2,length=100) # 10^10,.....10^(-2)
ridge.mod=glmnet(x,y,alpha=0,lambda=grid)
dim(coef(ridge.mod)) #20 (coefficients) by 100 (lambda)

# When lamda is bigger (penality is larger), coeficients are smaller
ridge.mod$lambda[50]
coef(ridge.mod)[,50]
sqrt(sum(coef(ridge.mod)[-1,50]^2))
ridge.mod$lambda[60]
coef(ridge.mod)[,60]
sqrt(sum(coef(ridge.mod)[-1,60]^2))

predict(ridge.mod,s=10,type="coefficients")[1:20,] # lambda=10

set.seed(1)
train=sample(1:nrow(x), nrow(x)/2)
test=(-train)
y.test=y[test]
ridge.mod=glmnet(x[train,],y[train],alpha=0,lambda=grid, thresh=1e-12)
ridge.pred=predict(ridge.mod,s=4,newx=x[test,])
mean((ridge.pred-y.test)^2)
ridge.pred=predict(ridge.mod,s=6,newx=x[test,])
mean((ridge.pred-y.test)^2)
ridge.pred=predict(ridge.mod,s=8,newx=x[test,])
mean((ridge.pred-y.test)^2)
ridge.pred=predict(ridge.mod,s=800,newx=x[test,])
mean((ridge.pred-y.test)^2)
ridge.pred=predict(ridge.mod,s=10000,newx=x[test,])
mean((ridge.pred-y.test)^2)

# OLS/linear model = when Lidge Model with lambda=0
ridge.pred=predict(ridge.mod,s=0,newx=x[test,],exact=T,x=x[train,],y=y[train])
mean((ridge.pred-y.test)^2)

lm(y~x, subset=train)
predict(ridge.mod,s=0,exact=T,type="coefficients",x=x[train,],y=y[train])[1:20,]

set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=0)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam
ridge.pred=predict(ridge.mod,s=bestlam,newx=x[test,])
mean((ridge.pred-y.test)^2)
out=glmnet(x,y,alpha=0)
predict(out,type="coefficients",s=bestlam)[1:20,]

# Lasso

lasso.mod=glmnet(x[train,],y[train],alpha=1,lambda=grid)
plot(lasso.mod)
set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min
lasso.pred=predict(lasso.mod,s=bestlam,newx=x[test,])
mean((lasso.pred-y.test)^2)
out=glmnet(x,y,alpha=1,lambda=grid)
lasso.coef=predict(out,type="coefficients",s=bestlam)[1:20,]
lasso.coef
lasso.coef[lasso.coef!=0]

##
## Example on California School District Data
##
caschool.data = read.csv("W01d_caschool.csv")
str(caschool.data)
caschool=na.omit(caschool.data[,-1]) # remove ID and missing values
x=model.matrix(api~.,caschool)[,-1]
y=caschool$api

grid = 10^seq(10,-2,by=-0.1) 

#(1) Ridge model
fit01 = glmnet(x,y,alpha = 0, lambda = grid)
summary(fit01)

cv.01 =cv.glmnet(x,y,alpha = 0, lambda = grid )
plot(cv.01)

opt_lambda = cv.01$lambda.min
opt_lambda

fit02 = cv.01$glmnet.fit
summary(fit02)

y_pred = predict(fit02, s = opt_lambda, newx = x)
y_pred[1:10,]
mean((y_pred-y)^2)

#(2) Lasso model
cv.02 =cv.glmnet(x,y,alpha = 1, lambda = grid )
plot(cv.02)
opt_lambda = cv.02$lambda.min
fit03 = cv.02$glmnet.fit
summary(fit03)

y_pred = predict(fit03, s = opt_lambda, newx = x)
y_pred[1:10,]
mean((y_pred-y)^2)

#(3) OLS/linear model
ols=lm(api~., caschool)
y_pred1 = predict(ols)
y_pred1[1:10]
mean((y_pred1-y)^2)

