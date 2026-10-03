# 正文来源：CH1-R语言概述.tex，代码块 84；修订稿第 2160 行。
# 仅提取章末习题之前的正文；不含习题提示或答案。
# 语法已检查；未宣称全部外部数据与可选分支已执行。
library(ggplot2)

ggplot(mtcars, aes(x = mpg)) + geom_histogram(binwidth = 1, fill = "blue", color = "black") +
    labs(title = "Histogram of Miles Per Gallon")

ggplot(mtcars, aes(x = factor(cyl), y = mpg)) + geom_boxplot(fill = "orange", color = "black") +
    labs(title = "Boxplot of Miles Per Gallon by Cylinder Count")

ggplot(mtcars, aes(x = factor(cyl))) + geom_bar(fill = "green", color = "black") + labs(title = "Bar Plot of Car Counts by Cylinder")

ggplot(mtcars, aes(x = factor(1), fill = factor(cyl))) + geom_bar(width = 1) + coord_polar(theta = "y") +
    labs(title = "Pie Chart of Cylinder Counts")

ggplot(mtcars, aes(x = mpg)) + geom_density(fill = "magenta") + labs(title = "Density Plot of Miles Per Gallon")

ggplot(mtcars, aes(sample = mpg)) + stat_qq() + stat_qq_line() + labs(title = "QQ-Plot of Miles Per Gallon")

ggplot(mtcars, aes(x = wt, y = mpg)) + stat_density_2d(aes(fill = after_stat(level)), geom = "polygon",
    color = "white") + scale_fill_viridis_c() + labs(title = "2D Density Contour Plot of mtcars Dataset",
    x = "Weight (wt) ", y = "Miles Per Gallon (mpg) ", fill = "Density")
