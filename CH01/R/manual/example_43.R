# 正文来源：CH1-R语言概述.tex，代码块 43；修订稿第 1015 行。
# 仅提取章末习题之前的正文；不含习题提示或答案。
# 语法已检查；未宣称全部外部数据与可选分支已执行。
library(scatterplot3d)

scatterplot3d(mtcars$hp, mtcars$wt, mtcars$mpg, pch = 19, color = "blue", main = "3D Scatterplot: HP vs. Weight vs. MPG")
