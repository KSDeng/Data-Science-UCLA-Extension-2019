# 变量筛选、建立线性回归预测模型的范例
# https://blog.csdn.net/jiabiao1602/article/details/39177125

# 清空工作区
rm(list = ls())
# 加载R内置的数据集longley, 查看其结构
str(longley)
# 首先建立全变量模型
# 注意用于回归的变量类型必须均为数字
linear_model1 = lm(longley$GNP.deflator ~ ., data = longley)
summary(linear_model1)      # 模型结果是令人沮丧的, 有些变量并不适合建模

# 看各自变量是否存在共线性问题。此处利用方差膨胀因子进行判断：
# 方差膨胀因子VIF是指回归系数的估计量由于自变量共线性使得方差增加的一个相对度量。
# 一般建议，如VIF>10，表明模型中有很强的共线性问题。
library(car)
vif(linear_model1)
# 多个变量的VIF较大，说明模型中的共线性问题比较严重

# 计算变量间的相关系数
cor(longley[,2:7])
# 作出变量间的关系图, 使变量将的相关关系更加直观
plot(longley[, 2:7])
# 用step()函数筛选适合建模的变量, 筛选标准为AIC(赤池信息量准则)
# https://blog.csdn.net/yinyu19950811/article/details/60964782
linear_step = step(linear_model1, direction = "backward")
# 查看结果
summary(linear_step)
