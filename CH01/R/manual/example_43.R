# 正文来源：CH1-R语言概述.tex，代码块 43；正文第 1005 行。
# 只提取章末习题之前的正文代码；原控制台输出未纳入。
# 手动示例：可能依赖前序代码、外部文件、额外R包；参见本章README与manual/index.csv。
# 已移除自动安装、清空工作空间、保存整个工作空间及本机工作目录切换。
dir.create("results/manual", recursive=TRUE, showWarnings=FALSE)
library(scatterplot3d)

scatterplot3d(mtcars$hp, mtcars$wt, mtcars$mpg, pch = 19, color = "blue", main = "3D Scatterplot: HP vs. Weight vs. MPG")
