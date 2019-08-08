rm(list = ls())
# Learn to use lattice.
# https://www.jianshu.com/p/5885aaeda6c1
# https://www.statmethods.net/advgraphs/trellis.html


# lattice是基于变量之间的关系进行数据可视化的
# install.packages("lattice")
library(lattice)
# 调用attach()函数将mtcars加入R数据库, 之后访问其中的变量只需要直接引用变量名
attach(mtcars)
data(mtcars)
m = mtcars
# factor()函数将numeric的变量编码成factor的变量(即categorical)
gear.f <- factor(gear, levels = c(3, 4, 5),
               labels = c("3 gears", "4 gears", "5 gears"))
cyl.f <- factor(cyl, levels = c(4, 6, 8),
              labels = c("4 cylinders", "6 cylinders", "8 cylinders"))

# Kernel density plot.
# “~mpg”表示作出以mpg为自变量的"数据图", 具体以什么作为因变量跟调用的函数有关
# main参数设置图标题, xlab参数设置横轴标题
densityplot(~mpg, main = "Density Plot", xlab = "Miles per Gallon")

# Kernel density plots by factor level.
# “~mpg | cyl.f”前半部分表示作出以mpg为自变量的"数据图", 具体以什么作为因变量跟调用的函数有关
#  后半部分表示以不同的cyl.f的值进行分类, 每一类画在不同的面板上
densityplot(~mpg | cyl.f, main = "Density Plot by Number of Cylinders",
            xlab = "Miles per Gallon")

# layout = c(1,3)将绘图排列成一列三行
densityplot(~mpg | cyl.f, main = "Density Plot by Number of Cylinders",
            xlab = "Miles per Gallon", layout = c(1, 3))

bwplot(cyl.f ~ mpg | gear.f, ylab = "Cylinders", xlab = "Miles per Gallon",
       main = "Mileage by Cylinders and Gears", layout = c(1,3))

# "mpg~wt | cyl.f*gear.f 表示画出mpg与wt的关系图, 并且按cyl.f与gear.f的不同组合分面板
xyplot(mpg ~ wt | cyl.f * gear.f, main = "Scatterplots by Cylinders and Gears",
       ylab = "Miles per Gallon", xlab = "Car Weight")

# cloud()函数用于作三维散点图, mpg~wt*qsec表示mpg为因变量, wt和qsec为自变量
cloud(mpg ~ wt * qsec | cyl.f, main = "3D Scatterplot by Cylinders")

# 用于分面板的变量必须是factor类型的
dotplot(cyl.f ~ mpg | gear.f, main = "Dotplot Plot by Number of Gears and Cylinders",
   xlab = "Miles Per Gallon")
? lattice

# scatterplot matrix 
splom(mtcars[c(1, 3, 4, 5, 6)], main = "MTCARS Data")
mtcars[1] # 表示矩阵mtcars的第一列

# Customized Lattice Example
panel.smoother <- function(x, y) {
    panel.xyplot(x, y) # show points 
    panel.loess(x, y) # show smoothed line 
}
attach(mtcars)
# cut()函数将numeric型变量切分成多个区间, 并转换成Factor型变量
hp <- cut(hp, 3) # divide horse power into three bands 
xyplot(mpg ~ wt | hp, scales = list(cex = .8, col = "red"),
   panel = panel.smoother,
   xlab = "Weight", ylab = "Miles per Gallon",
   main = "MGP vs Weight by Horse Power")
str(mtcars)