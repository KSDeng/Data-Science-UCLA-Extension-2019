#####################################################################################################
# D01b. Introduction to Package/Library  
# By William Yu, UCLA Anderson Forecast
# updated on 6/25/2019
##################################################################################################### 

#
# Install packages and import data from Yahoo Finance
#
install.packages("tseries")       # You only need to install package for the first time
library(tseries)                  # You need to call on library every time you restart the R script

# Import the data from Yahoo finance directly: Chnage instrument for different stocks
sp500 = get.hist.quote(instrument="^gspc", start="1976-01-01", end="2019-5-01", quote="AdjClose", 
                       provider="yahoo", origin="1970-01-01", compression="m", retclass="zoo")

msft = get.hist.quote(instrument="msft", start="1976-01-01", end="2019-5-01", quote="AdjClose", 
                      provider="yahoo", origin="1970-01-01", compression="m", retclass="zoo")

wmt = get.hist.quote(instrument="wmt", start="1976-01-01", end="2019-5-01", quote="AdjClose", 
                     provider="yahoo", origin="1970-01-01", compression="m", retclass="zoo")

par(mfrow=c(2,2))       # Make mutiple chart display 2 rows by 2 columns
plot(sp500)
plot(msft)
plot(wmt)
par(mfrow=c(1,1))       # Change chart setting back to the default
barplot(sp500)

# 'zoo'是R内置的一种时间序列对象
class(sp500)            # Check the object class of data, zoo is one kind of time series object      
sp500[1:5,]             # See first five rows of the data

# Frequency Coversion: Monthly data to Quarterly data
# First, use "ts" function to convert the series to another form of time series
sp500.month = ts(sp500, start=c(1976,1), frequency=12)
class(sp500.month)
sp500.quarter = aggregate(sp500.month, nfrequency=4,mean) # Another way is sum
sp500.quarter
? aggregate
? zoo
# 
# Export your data into your folder in csv file
#
write.csv(sp500.quarter, "sp500.quarter.csv") # export your data
write.table(sp500.quarter, "sp500.quarter")


# Generate continuosly compounded returns from stock prices: log(Pt)-log(Pt-1)
?diff
sp500.r=diff(log(sp500))
msft.r=diff(log(msft))
wmt.r=diff(log(wmt))

# main参数设置图表标题，xlab和ylab参数分别设置x轴和y轴
plot(sp500.r,main="S&P 500 Stock Returns",xlab="Year",ylab="returns")  # main="" is for the title of the chart
# 在图中增加一条水平线作为参照
abline(h=mean(sp500.r))  # horizontal line is the S&P500 return mean. v= for vertical line.
# 输出数据集的一些统计数据
summary(sp500.r)

# Useful data management tool
stocks = cbind(sp500.r, msft.r, wmt.r) # Combine columns;  Alternative:cbind.data.frame
# stocks_test = cbind.data.frame(sp500.r,msft.r,wmt.r)
colnames(stocks) = c("SP500", "MSFT", "WMT")    # Change the column names
stocks[1:10,]         # Show the first 10 rows
plot(stocks, xlab="Monthly Returns")
colMeans(stocks)
colMeans(stocks, na.rm=T)   
summary(stocks)
# na.exclude()函数直接去掉含有不完整数据的行
stocks1=na.exclude(stocks)  # na.exclude() function returns the data with incomplete row removed.
# na.omit() function is the same as na.exclude
stocks1[1:5,]

# Show smoothed histogram of the data
hist(sp500.r, breaks = 40)

TOASK: What's the usage of the function density ? I can't understand.
? density
# 设置线条风格、宽度、颜色
plot(density(sp500.r),xlim=c(-0.4,0.4), main="Smoothed Histogram of Monthly Stock Returns",lty=1)
points(density(msft.r), type="l", lty=1, lwd=5, col="red")  #lty is line style, lwd is line width
points(density(wmt.r), type = "l", lty = 1, lwd = 1, col = "blue")
# 设置图例
legend(-0.4, 10, legend=c("S&P500", "MSFT","WMT"), lty=c(1,1,1), col=c("black","red","blue"))

# 以变量矩阵画散点图
pairs(stocks)
# Calculate correlation
colnames(stocks)
# 计算相关系数，每一列代表一个变量的序列
cor(stocks)
cor(stocks, use="complete.obs")   # use="complete.obs" handle missing value

# 3-dimenion plots
?contour
?persp
x=seq(-20,20)
y = x

dim(x) # TOASK: Why does this line output "NULL"
# 将x中元素与y中元素逐个列举，进行第三个函数中的操作
# 默认为乘法操作，也可由用户自定义传入函数指针，此处使用匿名函数
z = outer(x, y, function(x, y) 3 * x ^ 2 + y ^ 2)
# 以x,y 为平面坐标，以z为高度坐标画出轮廓图
contour(x, y, z)
# 画三维透视图
persp(x,y,z)
# 旋转角度
persp(x,y,z,theta=30,phi=20)
persp(x,y,z,theta=40,phi=20)
persp(x,y,z,theta=50,phi=20)
persp(x,y,z,theta=60,phi=20)

# Update R version. Better Do it in R.
install.packages("installr") 
library(installr) 

updateR() 




     