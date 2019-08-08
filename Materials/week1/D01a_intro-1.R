#####################################################################################################
# D01a. Introduction to R  
# By William Yu, UCLA Anderson Forecast
# updated on 6/25/2019
##################################################################################################### 

# R Scripts
# Open an existing R script: File -> Open File... and then go to where the R script is located
# Create a new R script: File -> New File -> R Script

#
# After # are comments lines in green color.
# Run:    
# (1) Selective command lines: Highlight the command and click "Run"  
# (2) Entire single command line: place the cursor on the command line and then click "Run" or simply hit "Ctrl"+"Enter"
#

help(mean)
?sd
?rnorm
example(seq)

2+3
# assign "=" "<-" are identical and interchangable
a = (3*8/4)^2   # ^ exponentiation
a

a1 = (7-1)^3-9/2
a1
a2 <- ((7-1)^3-9)/2
a2

b <- log(100)
b

exp(b)

3angeles = sqrt(49)     # Variables cannot begin with a number
angeles = sqrt(49)
angeles
Angeles                 # R is case sensitive
test = 1,800            # No comma!!!
test = 1800
test

# Construct vector of number
x = c(8, 1, 2, 5);
x
x^2
x%%2        # modular arithmetric
sort(x);
x
x = sort(x);
x
cumsum(x)

y=c(-1:12)
y
length(y)               # The length of the data

# Construct matrix and data management
y1=matrix(0,4,5)        # A 4 rows by 5 columns matrix of zeros
y1
# matrix函数不管指定nrow参数还是ncol参数，在组织的时候数据都是按列优先的形式组织
y2=matrix(1:10,nrow=2)  # A 2 by 5 matrix
y2
y3=matrix(1:10,ncol=5)
y3
y4=y2+y3
y4
y5=t(y4)                # The transpose of 2 by 5 becomes a matrix of 5 by 2
y5
dim(y5)
nrow(y5)
ncol(y5)

# Matrix multiplcation
# y2: 2 by 5    (2 by 5)x(5 by 2)=(2 by 2)
# y5: 5 by 2    (5 by 2)x(2 by 5)=(5 by 5)
y2 %*% y5           # "%*%" 为特殊操作符，表示矩阵相乘
y5 %*% y2

y6=as.vector(y5)    # as.vector函数将矩阵按列扁平化
y5; y6  # Use ; to list two commands in one line
y7=as.vector(t(y5))
y7
matrix(y7,ncol=2)

# as.data.frame函数将矩阵数据结构化，包装成内置的类
y5d=as.data.frame(y5)
y5d
data.frame(y5)

y8=rbind(y3,y4) # row bind, 矩阵按行连接，只对列数相同的矩阵有效
y3; y4; y8
y9=cbind(y3,y4) # column bind，矩阵按列连接，只对行数相同的矩阵有效
y9
y10=cbind(y8,y9)    # 此行报错，因为y8和y9的行数不一样

# Matrix elements and showing   矩阵元素遍历
y5
# 矩阵元素访问用中括号，下标统一从1开始
y5[3, 2] # The third row and the second column
# 访问一行时列索引直接空着
y5[3,] # The third row
# 访问一列时行索引直接空着
y5[,2]                  # The second column
y5[3:5,] # Row 3 to 5
# 索引相隔进行访问时先要用c()将索引连成一个变量
y5[c(1, 3, 5),] # Row 1, 3, and 5
# 负数索引表示删除行/列
y10=y5[-2,]             # Delete the second row
y5
y10
y11=y5[,-2]             # Delete the second column
y11

# Logical value: TRUE(T) and FALSE(F)
# Logical operator: >,<,>=,<=,!=
x1= 1:6
x1
# 对矩阵使用逻辑操作表示对矩阵的每个元素进行该逻辑运算
x1<5
x1==4 # equal sign
x1!=4 # not equal sign
y5 <= 6

# Generate random number with standard normal distribution
x2=rnorm(5000)
x2

plot(x2)
plot(x2, type="l")      # Plot type is "line"
hist(x2, breaks=100)
?hist
x3=rnorm(1000, mean=10, sd=0.5)
mean(x3)
var(x3)
sqrt(var(x3))
sd(x3)

# 用par函数的mfrow参数把绘图区设置为2行1列
par(mfrow=c(2,1))
hist(x2)
hist(x3)
par(mfrow=c(1,1))

# Previous command: up arrow

rm(list=ls()) # Clear up the global enviroment

# Data cleaning is very important for handling dirty data, in particular for missing value!!
k1 = c(1,2,3,NA, 4)
k1

?is.na
is.na(k1)
mean(k1)
mean(k1, na.rm=TRUE) # na.rm: Should missing values be removed from the calculation ("TRUE" or "T")
mode(k1)

# Character strings
k2=abc
k2 = "abc"
# character类型是整个字符串作为1个元素
length(k2)
k3=c("abc","1984")
length(k3)
k3
mode(k3)

# Replace non-numeric values to clean it up for numerical analysis
test=c("$1000","$2,000","3,000",4000, "5000  ")
test
mean(test)
class(test)

# Solutions: gsub(A,B,data) is a useful substitution function. Replace A with B.  
test=gsub(",","",test)  # Replace "," with "", meaning to remove "," in test.
test
test=gsub("$","",test)
test
# $ 是R语言特殊符号，需要用转义字符
test=gsub("\\$","",test)
test
class(test)
# as.numeric()函数可将字符串转为数字(自动忽略空格)
test=as.numeric(test)
test
class(test)
mean(test)

# string manipulation
# paste()函数将多个向量转换成字符串后连接，默认的分隔符为空格
s1 =paste("I", "am","excited!")
s1
s2 =paste("I", "am","excited!", sep=" ")
s2
s3 =paste("I", "am","excited!", sep="@")
s3
s4 =paste("I", "am","excited!", sep="")
s4

# list: container for values or other types of data
cfootball=list(team="Clemson vs Alambama", score="44-16") 
cfootball
mode(cfootball)
# 美元符号表示从列表中取出某个变量
cfootball$team
cfootball$score

