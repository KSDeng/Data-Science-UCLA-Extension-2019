rm(list = ls())

library(readxl)
library(dplyr)

data = data.frame(read.csv("cleaned_data.csv"))

getDataOfWholeCountry = function(yearSelect, indexSelect) {
    dataFilter = data %>% filter(year == yearSelect)
    res = dataFilter %>% select(c("province",indexSelect))
    return(res)
}
getDataOfProvince = function(varSelect, areaSelect) {
    dataFilter = data %>% filter(province == areaSelect)
    res = dataFilter %>% select(c("year","province", varSelect))
    return(res)
}

HP2018 = getDataOfWholeCountry(2018, "HP")
write.csv(HP2018, "visualization/HP2018.csv", row.names = F)
MR2018 = getDataOfWholeCountry(2018, "MR")
write.csv(MN2018, "visualization/MR2018.csv", row.names = F)
GDPC2018 = getDataOfWholeCountry(2018, "GDPC")
write.csv(GDPC2018,"visualization/GDPC2018.csv", row.names = F)

Beijing_POP = getDataOfProvince(varSelect = "POP", areaSelect = "北京市")
plot(Beijing_POP$year, Beijing_POP$POP)
Anhui_POP = getDataOfProvince(varSelect = "POP", areaSelect = "安徽省")
plot(Anhui_POP$year, Anhui_POP$POP)


MR_DR = data.frame(read_excel("visualization/MR_DR.xlsx"))
nrows = nrow(MR_DR)
MR_DR = MR_DR[-c(nrows, nrows - 1),]
colnames(MR_DR) = c("year","MR","DR")
library(ggplot2)
# install.packages("ggthemes")
library(ggthemes)



# Visualize the change of marriage rate and divorce rate in China, 2010-2017
p = ggplot(data = MR_DR, mapping = aes(x = year, group = 1)) +
    geom_line(mapping = aes(y = MR, colour = "Marriage Rate")) +
    geom_point(mapping = aes(y = MR, colour = "Marriage Rate"), size = 1.5) +
    geom_text(mapping = aes(y = MR, label = MR)) +
    geom_line(mapping = aes(y = DR * 3, colour = "Divorce Rate")) +
    geom_point(mapping = aes(y = DR * 3, colour = "Divorce Rate"), size = 1.5) +
    geom_text(mapping = aes(y = DR * 3, label = DR)) +
    scale_y_continuous(sec.axis = sec_axis(~. / 3, name = "Divorce Rate(‰)")) +
    scale_colour_manual(values = c("Marriage Rate" = "blue","Divorce Rate" = "red")) +
    labs(colour = "", x = "Year", y = "Marriage Rate(‰)") + theme_economist()
p




sessionInfo()

library(stargazer)
training_data = data.frame(read.csv("training_set.csv"))
stargazer(training_data[1:10,1:9], summary = FALSE)