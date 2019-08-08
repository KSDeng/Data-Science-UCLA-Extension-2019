# ggplot绘图1
# https://www.plob.org/article/7264.html
rm(list = ls())

install.packages('ggplot2')
library(ggplot2)
# 加载钻石数据集
data(diamonds)
# 设置随机数种子
set.seed(42)
# 从数据集中随机选择n行数据
# nrow()/ncol()返回数据集的行数/列数, sample()用于从大样本中取样
small = diamonds[sample(nrow(diamonds), 300),]
# 输出开头部分(初探)
# head(small)

p <- ggplot(data = small, mapping = aes(x = carat, y = price)) # 建立数据到坐标轴的映射
p + geom_point() # 数据到几何对象的映射, geom_point()表示散点图

# 将"切工(cut)"属性映射到形状上(即让不同cut的数据显示不同的形状)
p <- ggplot(data = small, mapping = aes(x = carat, y = price, shape = cut))
p + geom_point()

# 将钻石的"颜色(color)"属性映射到颜色参数(colour)
p <- ggplot(data = small, mapping = aes(x = carat, y = price, shape = cut, colour = color)) # 数据到属性的映射
p + geom_point() # (数据+属性)到几何对象的映射

# 属性也可以在数据->几何对象的过程中进行设置
p = ggplot(small)
p + geom_point(aes(x = carat, y = price, shape = cut ,colour = color))

# ggplot2支持图层, 可以把不同的图层中共用的映射提供给ggplot函数
# 而某一几何独享才需要的映射参数提供给geom_xxx()函数


# 直方图

ggplot(small) + geom_histogram(aes(x = price))
# 用切工的种类填充颜色
ggplot(small) + geom_histogram(aes(x = price, fill = cut))
# 每个直方分开画
ggplot(small) + geom_histogram(aes(x = price, fill = cut), position = "dodge")
# 每列完全填充, 按照不同颜色的相对比例来画
ggplot(small) + geom_histogram(aes(x = price, fill = cut), position = "fill")


# 柱状图(常用来表示计数数据)

# 以透明度为例, 因变量未指定则默认为数量
ggplot(small) + geom_bar(aes(x = clarity))
# 柱状图按指定高度画图
ggplot() + geom_bar(aes(x = c(LETTERS[1:3]), y = 1:3), stat = "identity")

# 概率密度函数(Probability Density Function)图
# 数量随价格的概率密度函数, 根据切工的不同使用不同的颜色
ggplot(small) + geom_density(aes(x = price, colour = cut))
# 数量随价格的概率密度函数, 根据透明度等级(clarity)不同用不同颜色进行填充
ggplot(small) + geom_density(aes(x = price, fill = clarity))

# 箱式图
# 横轴按切工(cut)类型分类, 纵轴为价格(price), 根据不同颜色进行填充
ggplot(small) + geom_boxplot(aes(x = cut, y = price, fill = color))


# 标尺

# 用scale_y_log10()函数将y轴进行对数变换
# scale_colour_manual()进行颜色设置, 这里取7个彩虹色(颜色数量应该与变量的取值对应)
ggplot(diamonds) + geom_point(aes(x = carat, y = price, shape = cut,
    colour = color)) + scale_y_log10() + scale_colour_manual(values = rainbow(7))


# 统计变换
# 统计变换对原始数据进行某种计算，然后在图上表示出来

# 例如对散点图上加一条回归线
ggplot(small, aes(x = carat, y = price)) + geom_point() + scale_y_log10() + stat_smooth()

# 这里，aes所提供的参数，就通过ggplot提供，而不是提供给geom_point，因为ggplot里的参数，相当于全局变量
# geom_point()和stat_smooth()都知道x,y的映射，如果只提供给geom_point()，则相当于是局部变量
# geom_point知道这种映射，而stat_smooth不知道，当然你再给stat_smooth也提供x,y的映射，不过共用的映射，还是提供给ggplot好。


# 坐标系统

ggplot(small) + geom_bar(aes(x = cut, fill = cut)) + coord_flip()
# 使用极坐标(变为饼图)
# 柱状图的高度(y)对应于饼图的弧度(theta)
ggplot(small) + geom_bar(aes(x = factor(1), fill = cut)) + coord_polar(theta = "y")

# 靶心图
ggplot(small) + geom_bar(aes(x = factor(1), fill = cut)) + coord_polar()

# 分面
# 按cut分面作回归线
p = ggplot(small, aes(x = carat, y = price)) + geom_point(aes(colour = cut)) + scale_y_log10() + facet_wrap(~cut) + stat_smooth()
p


# 主题
# install.packages('ggthemes')
library(ggthemes)

p + theme_wsj()     # emm不太好看
p + theme_stata()   # 这个好！
p + theme_excel()
p + theme_excel_new()   # 这个也可


