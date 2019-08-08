######################################################################################################
# D04c Telco Customer Churn Analysis  
# Data: https://www.ibm.com/communities/analytics/watson-analytics-blog/predictive-insights-in-the-telco-customer-churn-data-set/
# R script: https://www.kaggle.com/farazrahman/telco-customer-churn-logisticregression/notebook
# 7/15/2019
##################################################################################################### 
setwd("C:/Users/wyu/documents/zip08/2019 Q3 Summer_XData/Data")
install.packages("cowplot")
install.packages("ggcorrplot")
library(tidyverse) 
library(MASS)
library(car)
library(e1071)
library(caret)
library(caTools)
library(cowplot)
library(pROC)
library(ggcorrplot)
library(corrplot)

telco = read.csv("W04a_churn.csv")
str(telco)

# Customers who left within the last month - the column is called Churn
# Services that each customer has signed up for - phone, multiple lines, 
#        internet, online security, online backup, device protection, tech support, and streaming TV and movies
# Customer account information - how long they've been a customer, contract, payment method, paperless billing, 
#        monthly charges, and total charges
# Demographic info about customers - gender, age range, and if they have partners and dependents

# Visualizing NAs
missing_data <- telco %>% summarise_all(funs(sum(is.na(.))/n()))
missing_data <- gather(missing_data, key = "variables", value = "percent_missing")
ggplot(missing_data, aes(x = reorder(variables, percent_missing), y = percent_missing)) +
  geom_bar(stat = "identity", fill = "red", aes(color = I('white')), size = 0.3)+
  xlab('variables')+
  coord_flip()+ 
  theme_bw()

summary(telco)

##
## EDA
##

# Churn
telco %>% 
  group_by(Churn) %>% 
  summarise(Count = n())%>% 
  mutate(percent = prop.table(Count)*100)%>%
  ggplot(aes(reorder(Churn, -percent), percent), fill = Churn)+
  geom_col(fill = c("grey", "light blue"))+
  geom_text(aes(label = sprintf("%.1f%%", percent)), hjust = 0.2, vjust = 2, size = 5)+ 
  theme_bw()+  
  xlab("Churn") + ylab("Percent") + ggtitle("Churn Percent")

corrplot(cor(telco[,-1]), type="lower", method="number")

# Churn against catergorial variables
plot_grid(ggplot(telco, aes(x=gender,fill=Churn))+ geom_bar()+ theme_bw(), 
          ggplot(telco, aes(x=SeniorCitizen,fill=Churn))+ geom_bar(position = 'fill')+theme_bw(),
          ggplot(telco, aes(x=Partner,fill=Churn))+ geom_bar(position = 'fill')+theme_bw(),
          ggplot(telco, aes(x=Dependents,fill=Churn))+ geom_bar(position = 'fill')+theme_bw(),
          ggplot(telco, aes(x=PhoneService,fill=Churn))+ geom_bar(position = 'fill')+theme_bw(),
          ggplot(telco, aes(x=MultipleLines,fill=Churn))+ geom_bar(position = 'fill')+theme_bw()+
            scale_x_discrete(labels = function(x) str_wrap(x, width = 10)), align = "h")

plot_grid(ggplot(telco, aes(x=InternetService,fill=Churn))+ geom_bar(position = 'fill')+ theme_bw()+
            scale_x_discrete(labels = function(x) str_wrap(x, width = 10)), 
          ggplot(telco, aes(x=OnlineSecurity,fill=Churn))+ geom_bar(position = 'fill')+theme_bw()+
            scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
          ggplot(telco, aes(x=OnlineBackup,fill=Churn))+ geom_bar(position = 'fill')+theme_bw()+
            scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
          ggplot(telco, aes(x=DeviceProtection,fill=Churn))+ geom_bar(position = 'fill')+theme_bw()+
            scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
          ggplot(telco, aes(x=TechSupport,fill=Churn))+ geom_bar(position = 'fill')+theme_bw()+
            scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
          ggplot(telco, aes(x=StreamingTV,fill=Churn))+ geom_bar(position = 'fill')+theme_bw()+
            scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),align = "h")

plot_grid(ggplot(telco, aes(x=StreamingMovies,fill=Churn))+ 
            geom_bar(position = 'fill')+ theme_bw()+
            scale_x_discrete(labels = function(x) str_wrap(x, width = 10)), 
          ggplot(telco, aes(x=Contract,fill=Churn))+ 
            geom_bar(position = 'fill')+theme_bw()+
            scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
          ggplot(telco, aes(x=PaperlessBilling,fill=Churn))+ 
            geom_bar(position = 'fill')+theme_bw()+
            scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
          ggplot(telco, aes(x=PaymentMethod,fill=Churn))+
            geom_bar(position = 'fill')+theme_bw()+
            scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
          align = "h")

# Churn against numeric variables
ggplot(telco, aes(x=Churn, y=tenure, fill=Churn)) + geom_violin()+
        geom_boxplot(width=0.1, fill="white") + labs(title="Tenure")
ggplot(telco, aes(x=Churn, y=MonthlyCharges, fill=Churn)) + geom_violin()+
  geom_boxplot(width=0.1, fill="white") + labs(title="Monthly Charges")
ggplot(telco, aes(x=Churn, y=TotalCharges, fill=Churn)) + geom_violin()+
  geom_boxplot(width=0.1, fill="white") + labs(title="Total Charges")

# Data cleaning and management
telco$MultipleLines = gsub("No phone service", "No", telco$MultipleLines)
telco$OnlineSecurity = gsub("No internet service", "No", telco$OnlineSecurity)
telco$OnlineBackup = gsub("No internet service", "No", telco$OnlineBackup)
telco$DeviceProtection = gsub("No internet service", "No", telco$DeviceProtection)
telco$TechSupport = gsub("No internet service", "No", telco$TechSupport)
telco$StreamingTV = gsub("No internet service", "No", telco$StreamingTV)
telco$StreamingMovies = gsub("No internet service", "No", telco$StreamingMovies)


# Split the data for train and test set
set.seed(123)
indices = sample.split(telco$Churn, SplitRatio = 0.7)
train = telco[indices,]
validation = telco[!indices,]

# Decide the model
model01 = glm(Churn ~ .-customerID, data = train, family = "binomial")
summary(model01)

model02 = stepAIC(model01, direction="both")
summary(model02)
vif(model02)

pred = predict(model02, type = "response", newdata = validation)
summary(pred)
validation$prob = pred

# Threshold = 0.5
pred_churn = factor(ifelse(pred >=0.5, "Yes", "No"))
real_churn = factor(validation$Churn)
table(pred_churn, real_churn)
confusionMatrix(pred_churn, real_churn)  # Not good!
confusionMatrix(pred_churn, real_churn, positive = "Yes")
(1381+302)/(1381+259+166+302)
302/(259+302)   # Sensitivity
1381/(1381+166) # Specificity

conf1=confusionMatrix(pred_churn, real_churn, positive = "Yes")
conf1$overall[1]
conf1$byClass[1]
conf1$byClass[2]

# Decide the optimal threshold
perform_fn <- function(cutoff) 
{
  pred_churn <- factor(ifelse(pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(pred_churn, real_churn, positive = "Yes")
  accuray <- conf$overall[1]
  sensitivity <- conf$byClass[1]
  specificity <- conf$byClass[2]
  out <- t(as.matrix(c(sensitivity, specificity, accuray))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

s = seq(0.01,0.99,length=100)
OUT = matrix(0,100,3)

for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),
     type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend("bottom",col=c(2,"darkgreen",4,"darkred"),text.font =3,inset = 0.02,
       box.lty=0,cex = 0.8, 
       lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))
abline(v = 0.3, col="red", lwd=1, lty=2)
axis(1, at = seq(0.1, 1, by = 0.1))

# ROC Curve
glm.roc <- roc(response = validation$Churn, predictor = as.numeric(pred))
plot(glm.roc, legacy.axes = TRUE, print.auc.y = 1.0, print.auc = TRUE)

