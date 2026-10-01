# 正文来源：CH1-R语言概述.tex，代码块 83；正文第 2156 行。
# 只提取章末习题之前的正文代码；原控制台输出未纳入。
# 手动示例：可能依赖前序代码、外部文件、额外R包；参见本章README与manual/index.csv。
# 已移除自动安装、清空工作空间、保存整个工作空间及本机工作目录切换。
dir.create("results/manual", recursive=TRUE, showWarnings=FALSE)
ggplot(data = mtcars, aes(x = hp, y = mpg)) + geom_point() + facet_grid(. ~ cyl) + theme_minimal() + labs(title = "Comparative Engine Power and Efficiency by Cylinder Count")

ggplot(data = mtcars, aes(x = wt, y = mpg)) + geom_point() + coord_cartesian(xlim = c(2, 5)) + labs(title = "Detailed View of Weight and Efficiency Correlation")
