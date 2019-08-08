
rm(list = ls())

# install.packages('readxl')
# install.packages('ggplot2')
library(readxl)
library(ggplot2)

corporateTax = data.frame(read_excel("P02_Corporate tax.xlsx"))
str(corporateTax)
# Equation 1
linear_model1 = lm(corporateTax$ypcg ~ corporateTax$ctax, data = corporateTax)
summary(linear_model1)
# Equation 2
linear_model2 = lm(ypcg ~ ctax + ypc2000, data = corporateTax)
summary(linear_model2)
# Equation 3
linear_model3 = lm(ypcg ~ ctax + ypc2000 + dty + (ctax*dty), data = corporateTax)
summary(linear_model3)

# Predict with equation 3
pred_model3 = predict(linear_model3, newdata = data.frame(ctax = 20, ypc2000 = 10000, dty = 35))
pred_model3         # 3.23925

# Plot figure 4
plot(corporateTax$ctax, corporateTax$ypcg, pch = 19, col = "blue",
     xlab = "Average Corporate Tax Rate 2000-2008(%)", ylab = "Average GDP per capita Growth 2000-2015(%)",
     xlim = c(10, 45), ylim = c(-1, 6), xaxt = "n", yaxt = "n")
abline(linear_model1, col = "red", lwd = 2); abline(h = 0)
axis(1); axis(2,las = 1)
grid(lty = 1)
title(main = "The association between the average corporate tax rate 2000-2008 \n and the average GDP growth per capita 2000-2015")
# figure4 = ggplot(data = corporateTax, mapping = aes(x = corporateTax$ctax, y = corporateTax$ypcg)) + geom_point()

# Select best linear model with package "olsrr"
# install.packages("olsrr")

library(olsrr)
# Train a linear model with all variables(excluding "country")
linear_model_whole = lm(ypcg ~ . - country, data = corporateTax)
# all_possible = ols_step_all_possible(model)
# Select the best linear model using Best Subset Algorithm
best_subset = ols_step_best_subset(linear_model_whole)
best_subset
plot(best_subset)
write.csv(as.data.frame(best_subset), "Model_Selection_Report.csv", row.names = FALSE)
# The model using all predictors have the largest Adjusted R-square Value

# Compare the model with 6 predictors and 4 predictors.
summary(linear_model_whole)
linear_model4 = lm(ypcg ~ ctax + ypc2000 + trade + ihc, data = corporateTax)
summary(linear_model4)
write.csv(as.data.frame(linear_model_whole$coefficients), "Model_6_predictors.csv")
write.csv(as.data.frame(linear_model4$coefficients),"Model_4_predictors.csv")

# TODO: none-linear model selection
# Reference: http://r-statistics.co/Model-Selection-in-R.html


