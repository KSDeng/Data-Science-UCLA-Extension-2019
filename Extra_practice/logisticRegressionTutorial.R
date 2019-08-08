rm(list = ls())
# Learn to use logistic regression.
# http://r-statistics.co/Logistic-Regression-With-R.html

# Import data
inputData = read.csv("adult.csv")
head(inputData)
str(inputData)

# Check class bias
count(inputData %>% filter(class == ">50K"))
count(inputData %>% filter(class == "<=50K"))
# Clearly, there is a class bias, a condition observed when the proportion of events 
# is much smaller than proportion of non-events. So we must sample the observations 
# in approximately equal proportions to get better models.

# Eliminate the dots in the column names
names(inputData)
colnames(inputData)[5] = "educationNum"
colnames(inputData)[6] = "maritalStatus"
colnames(inputData)[14] = "nativeCountry"

# Create pointer variable.
inputData$Above50K = ifelse(inputData$class == ">50K", 1, 0)

# Segregate continuous and factor variables
factor_vars <- c("workclass", "education", "maritalStatus", "occupation",
                 "relationship", "race", "sex", "nativeCountry")
continuous_vars <- c("age", "fnlwgt", "educationNum", "hoursperweek", "capitalgain", "capitalloss")

# Create WOE for categorical variables
# install.packages("InformationValue")
library(InformationValue)
for (factor_var in factor_vars) {
    inputData[[factor_var]] = WOE(X = inputData[, factor_var], Y = inputData$Above50K)
}

# Prepare training data.
input_ones = inputData[which(inputData$class == ">50K"),]   # all '>50K's
input_zeros = inputData[which(inputData$class == "<=50K"),] # all '<=50K's
set.seed(100)
input_ones_training_rows = sample(1:nrow(input_ones), 0.7 * nrow(input_ones))
input_zeros_training_rows = sample(1:nrow(input_zeros), 0.7 * nrow(input_ones))
training_ones = input_ones[input_ones_training_rows,]   # '>50K's for training
training_zeros = input_zeros[input_zeros_training_rows,]# '<=50K's for training
trainingData = rbind(training_ones, training_zeros) # combine training data


# Prepare testing data.
test_ones = input_ones[-input_ones_training_rows,]      # 直接用负号表示去掉对应的行
test_zeros = input_zeros[-input_ones_training_rows,]
testingData = rbind(test_ones, test_zeros)

# Compute Information Values
# install.packages("smbinning")
library(smbinning)
iv_df = data.frame(VARS = c(factor_vars, continuous_vars),
                   IV = numeric(length(factor_vars) + length(continuous_vars)))

for (column in colnames(trainingData)) {
    smb = smbinning(trainingData, y = "Above50K", x = column)
    #print(class(smb))
    if (class(smb) == "character") {
        print(paste(c(column, smb), ": "))
    }
    if (class(smb) != "character") {
        iv_df[iv_df$VARS == column, "IV"] = smb$iv
    }
}

# Only if not calculate the WOE for factors.
for (factor_var in factor_vars) {
    smb = smbinning.factor(trainingData, y = "Above50K", x = factor_var)        # WOE table
    #print(class(smb))
    if (class(smb) == "character") {
        print(paste(c(factor_var, smb),": "))
    }
    if (class(smb) != "character") {
        iv_df[iv_df$VARS == factor_var, "IV"] = smb$iv
    }
}

for (continuous_var in continuous_vars) {
    smb = smbinning(trainingData, y = "Above50K", x = continuous_var) # WOE table
    #print(class(smb))
    if (class(smb) == "character") {
        print(paste(c(continuous_var, smb), ": "))
    }
    if (class(smb) != "character") {
        iv_df[iv_df$VARS == continuous_var, "IV"] = smb$iv
    }
}


iv_df <- iv_df[order(-iv_df$IV),] # sort
iv_df

# Build full model.
names(inputData)
full.model = glm(Above50K ~ . - class, data = trainingData, family = binomial)

# Stepwise Logistic Regression Essentials in R
# http://www.sthda.com/english/articles/36-classification-methods-essentials/150-stepwise-logistic-regression-essentials-in-r/
library(tidyverse)
# install.packages("caret")
library(caret)
# install.packages("MASS")
library(MASS)
step.model = full.model %>% stepAIC(trace = TRUE)
coef(step.model)


# library(olsrr)
# best_subset = ols_step_best_subset(logitMod)  # Can only be used in linear model variable selection.

logitMod = glm(Above50K ~ relationship + age +
               educationNum + capitalgain, data = trainingData,
               family = binomial(link = "logit"))

# The following two lines have the same effect.
# predicted = plogis(predict(logitMod, testingData))
predicted = predict(logitMod, testingData, type = "response")

# Decide on optimal prediction probability cutoff for the model.
optCutOff = optimalCutoff(actuals = testingData$Above50K, predictedScores = predicted)

# Model diagnostics
# Like in case of linear regression, we should check for multicollinearity in the model. 
# As seen below, all X variables in the model have VIF well below 4.
summary(logitMod)
vif(logitMod) # 需保证变量之间不存在强共线性
# Misclassification error is the percentage mismatch of predcited vs actuals, 
# irrespective of 1’s or 0’s. The lower the misclassification error, the better is your model.
misClassError(actuals = testingData$Above50K, predictedScores = predicted, threshold = optCutOff)

plotROC(actuals = testingData$Above50K, predictedScores = predicted)
Concordance(actuals = testingData$Above50K, predictedScores = predicted) # 0.87

sensitivity(as.factor(testingData$Above50K),as.factor(predicted), threshold = optCutOff)

specificity(testingData$Above50K, predicted, threshold = optCutOff)

confusionMatrix(testingData$Above50K, predicted, threshold = optCutOff)
