# 正文来源：CH1-R语言概述.tex，代码块 81；正文第 2122 行。
# 只提取章末习题之前的正文代码；原控制台输出未纳入。
# 手动示例：可能依赖前序代码、外部文件、额外R包；参见本章README与manual/index.csv。
# 已移除自动安装、清空工作空间、保存整个工作空间及本机工作目录切换。
dir.create("results/manual", recursive=TRUE, showWarnings=FALSE)
ggplot(data = mtcars, aes(x = wt, y = mpg)) + geom_point() + stat_smooth(method = lm, col = "firebrick") + scale_y_continuous("Efficiency (MPG) ", 
    limits = c(5, 35), expand = c(0, 0)) + scale_x_continuous("Car Weight", limits = c(1, 20), expand = c(0, 0)) + 
    coord_equal() + labs(title = "The Weight to Fuel Efficiency Relationship", x = "Vehicle Weight (1000 lbs) ", 
    y = "Efficiency (MPG) ")
