rm(list = ls())

library(readxl)
# Prepare training data and testing data.
training_data = data.frame(read.csv("training_set.csv"))
training_data = training_data[, - which(colnames(training_data) %in% c("CON", "HCL", "EDUF", "HP", "MR", "DR"))]
testing_data = data.frame(read.csv("testing_set.csv"))
testing_data = testing_data[, - which(colnames(testing_data) %in% c("CON", "HCL", "EDUF", "HP", "MR", "DR"))]

linear_model.whole = lm(MN ~ . - year - province - DN, data = training_data)
summary(linear_model.whole)
linear_model.modify = lm(MN ~ . - year - province - DN - INC - HEET, data = training_data)
summary(linear_model.modify)
library(leaps)
reg_selection = regsubsets(MN ~ . - year - province - DN - INC - HEET, data = training_data)
summary(reg_selection)
library(olsrr)
best_subset = ols_step_best_subset(linear_model.modify)

linear_model.best = lm(MN ~ POP + CPI + UR + GR + SAV + GDPC, data = training_data)
summary(linear_model.best)
linear_model.best$coefficients

library(stargazer)
stargazer(linear_model.whole, linear_model.modify, linear_model.best, single.row = TRUE, out = "prediction_models.tex")

library(MLmetrics)
predicted = predict(linear_model.best, newdata = testing_data)
mse_predict = MSE(predicted, testing_data$MN)

predictionRes = data.frame(cbind(testing_data$year, as.character(testing_data$province)))
predictionRes = cbind(predictionRes, testing_data$MN, predicted)
colnames(predictionRes) = c("year", "province", "actual_MN", "predicted_MN")
write.csv(predictionRes, "linear_model_prediction.csv", row.names = F)
stargazer(predictionRes[10:20,], summary = F)

library(ggplot2)
# install.packages("ggthemes")
library(ggthemes)
ggplot(data = predictionRes, mapping = aes(x = seq(1, nrow(predictionRes)))) +
    geom_line(mapping = aes(y = actual_MN, colour = "Actual")) +
    geom_line(mapping = aes(y = predicted_MN, colour = "Prediction")) +
    geom_point(mapping = aes(y = actual_MN, colour = "Actual"), size = 1.5) +
    geom_point(mapping = aes(y = predicted_MN, colour = "Prediction"), size = 1.5) +
    labs(colour = "", x = "Testing Set Index", y = "Number of Marriage Couples (10,000)") +
    theme(legend.position = "right") + scale_colour_manual(values = c("blue", "red")) + theme_economist()