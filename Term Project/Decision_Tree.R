rm(list = ls())

# Import dataset.
training_data = data.frame(read.csv("training_set.csv"))
testing_data = data.frame(read.csv("testing_set.csv"))

library(rpart)
library(rattle)
# Train whole decision tree.
tree.whole = rpart(MR ~ . - year - province - DR, data = training_data)
summary(tree.whole)

fancyRpartPlot(tree.whole)

library(dplyr)
var_importance = data.frame(tree.whole$variable.importance)

predicted = predict(tree.whole, newdata = testing_data)
