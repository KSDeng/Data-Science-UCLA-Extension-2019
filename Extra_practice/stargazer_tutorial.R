# Learn to use stargazer
# https://www.jianshu.com/p/4ded6353dc44

rm(list = ls())
# install.packages("stargrazer")
library(stargazer)

data("attitude")
# Open the data editor
# fix(attitude)
# out参数设置输出的文件名
# 展示数据集描述性分析和部分内容
# header = FALSE 将不输出包作者的相关信息
stargazer(attitude, header = FALSE, out = "header.html")        # 生成html文件       
stargazer(attitude, out = "header.tex") # 生成tex文件(输出代码自动根据后缀识别)
# 输出数据集前4行, 不显示行号
# summary = F 只显示数据, summary = T 将显示数据的描述信息
stargazer(attitude[1:4,], summary = F, rownames = FALSE, out = "first_4_rows.html")

# 构建模型并用stargazer展示模型结果
linear.1 = lm(rating ~ ., data = attitude)
linear.2 = lm(rating ~ complaints + privileges + learning, data = attitude)
attitude$high.rating = (attitude$rating > 70)
probit.model = glm(high.rating ~ learning + critical + advance, data = attitude,
                   family = binomial(link = "probit"))
# title添加表名, out参数设置输出文件路径, align = T使数字排列整齐
# 一次可以统计输出多个模型
stargazer(linear.1, linear.2, probit.model, title = "Results", out = "Models.html", align = T)

# dep.var.labels 修改列名; covariate.labels 修改行名; no.space 删除空行
# 列名/行名逐个设置, 未设置的将继续使用原有的默认值
# omit.stat 根据代号删除部分统计量
stargazer(linear.1, linear.2, probit.model, title = "Regression Results", align = T,
          dep.var.labels = c("Overall Rating", "High Rating"),
          covariate.labels = c("Handling of Complaints", "No Special Privileges",
                               "Opportunity to learn", "Performance-Based Raises",
                               "Too Critical", "Advancement"),
                               omit.stat = c("LL", "ser", "f"),
                               no.space = T, out = "modify1.html")

# ci = TRUE展示置信区间, ci.level设置置信水平, single.row = TRUE使数据和置信区间在单行显示
stargazer(linear.1, linear.2, probit.model, title = "Regression Results", align = T,
          dep.var.labels = c("Overall Rating", "High Rating"),
          covariate.labels = c("Handling of Complaints", "No Special Privileges",
                               "Opportunity to learn", "Performance-Based Raises",
                               "Too Critical", "Advancement"),
                               omit.stat = c("LL", "ser", "f"),
                               no.space = T, out = "modify2.html",
                               ci = TRUE, ci.level = 0.9, single.row = TRUE)

# order 参数按行名调整顺序, 未设置的变量按默认顺序显示
# 使用keep.stat参数控制要展示的统计量，keep.stat="n"即只展示样本量的大小，并移除其他统计量
stargazer(linear.1, linear.2, probit.model, title = "Regression Results", align = T,
          dep.var.labels = c("Overall Rating", "High Rating"),
          order = c("learning", "privileges"),
          keep.stat = "n",
          no.space = T, out = "modify3.html",
          ci = TRUE, ci.level = 0.9, single.row = TRUE)

# type = "text" 以ASCII(文本)格式输出
# type参数可选择"text", "latex"和"html"
stargazer(linear.1, linear.2, probit.model, title = "Regression Results", align = T,
          dep.var.labels = c("Overall Rating", "High Rating"),
          order = c("learning", "privileges"),
          keep.stat = "n",
          no.space = T, out = "modify4.txt",
          ci = TRUE, ci.level = 0.9, single.row = TRUE, type = "text")

# 展示矩阵
correlation.matrix = cor(attitude[,c("rating","complaints","privileges")])
stargazer(correlation.matrix, title = "Correlation Matrix", out = "cor.txt", type = "text")
