#####################################################################################################
# D01c. Data Frame, and Data Cleaning, Processing, and Management
# updated on 6/25/2019 by William Yu
##################################################################################################### 

#
# Import data from your computer:
#
# (1) Easy way:
#
# Click File --> Import dataset --> From Text/Excel/SPSS/SAS/STATA  -->    Find your data in your computer. 
# In R Console, you will see the following result: 
# > @#$ <- read.csv("~/Zip08/2019 Q2 Spring_XData/Data/@#$.txt")
# You have successfully import the data into R 
#
# (2) Session -> Set Working Directory -> Choose Directory
#
# (3) Professional way:
#
# First Change the working directory where the data are for your own computer!!
# Click the top bar of your file. For PC, it reads "C:\Users\WYU\Documents\Zip08\2019 Q1 Winter_XData\Data" backward slash
# Note that in R, it should be changed to forward slash: setwd("C:/.../..../..")
# Note that maybe Mac users' start with setwd("/.../..../..")

# Check your current working directory!!
getwd()
# Set your working directory in R
setwd("C:/Users/wyu/documents/zip08/2019 Q3 Summer_XData/Data")
# or
setwd("~/zip08/2019 Q3 Summer_XData/Data")

#
# Data Import and Data Frame 
#
# If the imported data are not csv files, you can either (1) convert them to csv files or (2): 
# install.packages("readxl")                        # Importing xls,xlsx files is slower than read.csv. 
# library(readxl)                                   # So I don't recommend this way for importing big data.
# @#$ <- read_excel("@#$.xlsx")

install.packages("readxl")  
library(readxl)    

##
## (1) Homeless Data
##
# Import the Date file into data frame
# By default, it will only import the data in the first tab, e.g. Sheet1 
homeless <- read_excel("W01b_homeless.xlsx")  
colnames(homeless)

# Data Frame: two-dimensional rows-and-columns structure.
# Each column may have a different mode: some might have nmubers while others might have character strings.
# TOASK: Why the following line of code has three different outputs?
class(homeless)

# See structure of the data
str(homeless)
head(homeless)
homeless[1:10,]
summary(homeless)

# Create a new variable
# 用$符号限定的方式制定变量名，原有的变量名默认为数据表格的列名
homeless$pir = homeless$hv / homeless$mhi
# 用这种方式限定之后原表格自动增加一列
homeless[1:30,]

# as.data.frame()由表格数据生成dataframe
homeless.df=as.data.frame(homeless)
homeless.df[1:30,]

# Subset
# subset()函数从指定的数据集中按条件筛选出子集
?subset
homeless17=subset(homeless, year==2017)
warm=subset(homeless, tem>=50)
warm
# '&'表示逻辑与
warm17=subset(homeless, tem>=50 & year==2017)  # "&" is AND
warm17
# '|'表示逻辑或
highcost=subset(homeless17, hv>300000 | rent>=1100) # "|" is OR
highcost

##
## (2) Election Data and Merge
##
election <- read_excel("W01c_election.xlsx")  
colnames(election)
election12=subset(election, year==2012)
homeless12 = subset(homeless, year == 2012)
# merge()函数按指定方式连接两个dataframe
newdata=merge(homeless12,election12, by="state")

# Exporting the data in R to your specific computer folder 
write.csv(newdata,"newdata.csv")

##
## (3) California School Districts Data
##
# 用read.csv()函数读取的数据直接就是data.frame类型
# R语言的变量定义中允许有'.'
caschool.data = read.csv("W01d_caschool.csv")
class(caschool.data)
names(caschool.data)        # 该行等价于colnames(caschool.data)
colnames(caschool.data)
str(caschool.data)          # 数据集的结构信息，包括变量名、变量类型
summary(caschool.data)       # Show summary statistics of the data frame
caschool=caschool.data[,-1]  # Remove the first column (cdc), which is a factor
# caschool.data$cdc = NULL   # Alternative way to remove specific column variable
# 计算每列数据两两之间的相关系数
# use = "complete.obs"将无视数据中的NA，此参数的默认值为"everything"，此时数据集中含有NA将导致结果为NA
cor.ca = cor(caschool, use="complete.obs") 
cor.ca < 0.9

write.csv(cor.ca,"corca.csv") 

# Correlation plot for all variables
install.packages("corrplot")  
library(corrplot)
# corrplot画出相关系数图, type = "lower"表示只画原矩阵的下三角，type = "upper"表示只画上三角
# type参数的默认值为"full"，即全画
corrplot(cor.ca)
corrplot(cor.ca, type = "lower")
corrplot(cor.ca, type = "upper")
# order = "AOE"表示按照特征向量角序(?)进行排序
corrplot(cor.ca, order = "AOE")
# addCoef.col将相关系数显示在图上并设置字体颜色
corrplot(cor.ca, addCoef.col = "black")
# diag = FALSE不显示对角线上的值(1)
corrplot(cor.ca, type = "lower", addCoef.col = "grey", diag = FALSE)

? corrplot
# method参数指定画图的类型
corrplot(cor.ca, type = "lower", method = "number")
corrplot(cor.ca, type = "lower", method = "shade")
# Other methods = "ellipse", "number", "pie", "shade", "color"
# 更多用法见 https://zhuanlan.zhihu.com/p/28076189

# correlation plot for two variables
plot(caschool$chci,caschool$api, col="blue",xlim=c(40,240))

# which.max(cashool$api)显示caschool列表(data.frame)中api这个变量最大的元素的序号(从1开始)
which.max(caschool$api) # show the observation # with the highest API
# which.min(cashool$chci)显示caschool列表(data.frame)中chci这个变量最小的元素的序号
which.min(caschool$chci)
# 更多用法及介绍见 https://blog.csdn.net/OYY_90/article/details/82228408

# 使用subset函数进行筛选时变量直接用变量名指定即可，无须加上双引号
lausd.data <- subset(caschool.data, lausd==1) # For LA Unified School District only.
str(lausd.data)
lausd=lausd.data[,-1]  
summary(lausd)
hist(lausd$api)
mean(lausd$api)     # 所有元素进行计算，此时若数据中有NA则输出将为NA
mean(lausd$api, na.rm=TRUE) # 忽略NA元素进行计算

# Apply: applying a specific function to all columns (variables) with "2" or rows with "1"
# 对每一列求标准差
apply(lausd, 2, sd)
# 每一列去除NA后求标准差
apply(lausd, 2, mean, na.rm = TRUE)
# list apply, 返回一个List(逐个变量列出)
lapply(lausd, mean, na.rm = TRUE)
# simple apply, 将结果打包使更安全
sapply(lausd, mean, na.rm = TRUE)
# 更多用法见 https://www.cnblogs.com/xihehe/p/7473981.html

write.csv(lausd, 'lausd.csv')
# Table: a great function to count the values(integers) of variables 
# table()函数统计一个变量不同的取值各有多少个
table(lausd$asian)
table(lausd$latino)
poverty=table(lausd$poverty)
write.csv(poverty,"lausd_poverty.csv")

# Infix Operator %>%
install.packages("dplyr")  
library(dplyr)

# (1)
lausd.data = subset(caschool.data, lausd==1)
str(lausd.data)
# (2)
str(subset(caschool.data,lausd==1))
# (3)
# "%>%"是一个特殊运算符(必须引用dplyr包),类似管道,对数据逐个执行指定的操作
caschool.data %>% subset(lausd==1) %>% str()

#
# Select certain variables
#
fewer.var = caschool.data %>% select(api, chci, lausd, poverty)
# 筛选出fewer.var中lausd = 1的行
lausd.1 = fewer.var %>% filter(lausd==1)

# Managing missing values
summary(caschool.data)
# 将caschool.data表格变量api中的"NA"替换为0
caschool.data$api= gsub("NA",0,caschool.data$api)
summary(caschool.data)
# 若CHCI的值缺失, !is.na(chci)的结果为FALSE，此时该行数据将被filter丢弃
caschool.data1 = caschool.data %>% filter(!is.na(chci)) # Remove those rows that are missing values for CHCI
summary(caschool.data1)

# is.na(dataframe)对矩阵每个元素判断是否为NA，返回一个布尔变量组成的同型矩阵
# dataframe[indexes]访问多个元素
caschool.data[is.na(caschool.data)] = 0     # 将矩阵中的NA替换为0
summary(caschool.data)
