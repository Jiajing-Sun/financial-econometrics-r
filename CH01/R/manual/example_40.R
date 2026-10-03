# 正文来源：CH1-R语言概述.tex，代码块 40；修订稿第 993 行。
# 仅提取章末习题之前的正文；不含习题提示或答案。
# 语法已检查；未宣称全部外部数据与可选分支已执行。
boxplot(mpg ~ am, data = mtcars, main = "MPG by Transmission Type", xlab = "Transmission (0=Automatic, 1=Manual)",
    ylab = "MPG", col = c("lightblue", "lightgreen"))
