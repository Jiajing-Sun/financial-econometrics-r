# 正文来源：CH1-R语言概述.tex，代码块 84；正文第 2187 行。
# 只提取章末习题之前的正文代码；原控制台输出未纳入。
# 手动示例：可能依赖前序代码、外部文件、额外R包；参见本章README与manual/index.csv。
# 已移除自动安装、清空工作空间、保存整个工作空间及本机工作目录切换。
dir.create("results/manual", recursive=TRUE, showWarnings=FALSE)
library(ggplot2)

ggplot(mtcars, aes(x = mpg)) + geom_histogram(binwidth = 1, fill = "blue", color = "black") + labs(title = "Histogram of Miles Per Gallon")

ggplot(mtcars, aes(x = factor(cyl), y = mpg)) + geom_boxplot(fill = "orange", color = "black") + labs(title = "Boxplot of Miles Per Gallon by Cylinder Count")

ggplot(mtcars, aes(x = factor(cyl))) + geom_bar(fill = "green", color = "black") + labs(title = "Bar Plot of Car Counts by Cylinder")

ggplot(mtcars, aes(x = factor(1), fill = factor(cyl))) + geom_bar(width = 1) + coord_polar(theta = "y") + labs(title = "Pie Chart of Cylinder Counts")

ggplot(mtcars, aes(x = mpg)) + geom_density(fill = "magenta") + labs(title = "Density Plot of Miles Per Gallon")

ggplot(mtcars, aes(sample = mpg)) + stat_qq() + stat_qq_line() + labs(title = "QQ-Plot of Miles Per Gallon")

ggplot(mtcars, aes(x = wt, y = mpg)) + stat_density_2d(aes(fill = after_stat(level)), geom = "polygon", color = "white") + 
    scale_fill_viridis_c() + labs(title = "2D Density Contour Plot of mtcars Dataset", x = "Weight (wt) ", y = "Miles Per Gallon (mpg) ", 
    fill = "Density")
