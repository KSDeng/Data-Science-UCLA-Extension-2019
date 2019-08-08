# TODO
# Q1: It is confusing that when I consider two independent variables, and although they have high correlation value,
# it seems that the linear model prefers one of them much better than the other one, why is that possible?
# For example, GDP and SAV, correlation > 0.9, however the linear model prefers SAV much better than GDP.
# Q2: What should I do when different criterias contradict with each other?
# Model 1: MR ~ POP + UR + GR + SAV + GDPC
# Model 2: MR ~ POP + UR + GR + SAV + DLI
# According to the result of olsrr and leaps, model 2 is better.
# However, after I trained model 2, I found the p-value of Intercept is insignificant.
# By contrast, all the p-values of model 1 is significant.

rm(list = ls())

library(readxl)
library(car)
library(corrplot)

# Import data.
cleanedDataSet = data.frame(read.csv("cleaned_data.csv"))

# library(stargazer)
# stargazer(cleanedDataSet,summary = F)

# Testing the corrlations between independent variables.
corrplot(cor(cleanedDataSet[, which(colnames(cleanedDataSet) == "POP"):ncol(cleanedDataSet)]),
         method = "color", diag = FALSE, type = "lower", addCoef.col = "black", cl.cex = 1.2)


# CON-INC:0.99 
# HCL-INC:0.98
# HCL-CON:0.98
# EDUF-GDP:0.95
# HP-INC:0.89
# HP-CON;0.88
# HP-HCL:0.86
# To ensure the non-correlation between independent variables,
# exliminate CON, HCL, EDUF, HP
# cleanedDataSet = cleanedDataSet[,-which(colnames(cleanedDataSet) %in% c("CON","HCL","EDUF","HP"))]
# corrplot(cor(cleanedDataSet[, which(colnames(cleanedDataSet) == "POP"):ncol(cleanedDataSet)]),
#         method = "color",  diag = FALSE, type = "lower", addCoef.col = "black")


# Prepare training data and testing data.
# training_data = data.frame(read.csv("training_set.csv"))
# training_data = training_data[, - which(colnames(training_data) %in% c("CON", "HCL", "EDUF", "HP"))]
# testing_data = data.frame(read.csv("testing_set.csv"))
# testing_data = testing_data[, - which(colnames(testing_data) %in% c("CON", "HCL", "EDUF", "HP"))]

# Train linear model with all independent variables.
linear_model.whole = lm(MR ~ . - year - province - DR - MN - DN, data = cleanedDataSet)
linear_model.modify = lm(MR ~ POP + UR + GDPC + SAVPG + GR, data = cleanedDataSet)
linear_model.final = lm(MR ~ POP + UR + GDPC + SAVPG, data = cleanedDataSet)
stargazer(linear_model.whole,linear_model.modify,linear_model.final, single.row = TRUE, out = "summary.tex")

summary(linear_model.whole)
summary(linear_model.modify)
summary(linear_model.final)
library(stargazer)
stargazer(linear_model.whole, single.row = TRUE, out = "summary.tex")

co = linear_model.final$coefficients
stargazer(data.frame(co),summary = F)

summary(linear_model.modify)
vif(linear_model.modify)


# linear_model.test = lm(MR ~ POP + UR + GR + SAV, data = training_data)
# summary(linear_model.test)
# vif(linear_model.test)

# Linear model selection, using two different methods.
# It seems that SAV is better predictor than GDP.
# library(olsrr)
# best_subset = ols_step_best_subset(linear_model.whole)
# write.csv(best_subset,"ModelSelectionReport.csv")
library(leaps)
reg_model_bestsub = regsubsets(MR ~ .- year - province - DR - MN - DN - SAV - GDP, data = cleanedDataSet)
summary(reg_model_bestsub)

# Considering the results of model selection, choose POP, UR, GR, SAV and GDPC as independent variables.
#linear_model.best1 = lm(MR ~ POP + UR + GR + SAV + GDPC, data = training_data)

#linear_model.best2 = lm(MR ~ POP + UR + GR + SAV + DLI, data = training_data)
#summary(linear_model.best2)

# linear_model.best = lm(MR ~ POP + INC + UR, data = cleanedDataSet)
# summary(linear_model.best)
# vif(linear_model.best) # VIF values of the model are satisfying.
# predicted = predict(linear_model.best, newdata = testing_data)
# Evaluation
# library(MLmetrics)
# mse_prediction = MSE(y_pred = predicted, y_true = testing_data$MR)

# Compare best model with the whole model using anova. 
anova(linear_model.whole, linear_model.modify)
# The p-value of anova is insignificant, which shows that the additional variables
# in the whole model do little help.


# Prediction
# predicted = predict(linear_model.best, newdata = testing_data)
# predictionRes = data.frame(cbind(testing_data$year, as.character(testing_data$province)))
# predictionRes = cbind(predictionRes, testing_data$MR, predicted)
# colnames(predictionRes) = c("year", "province", "actual_MN", "predicted_MN")
# predictionRes$error = predictionRes$actual_MN - predictionRes$predicted_MN
# write.csv(predictionRes, "linear_model_prediction.csv", row.names = F)



   

