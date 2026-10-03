# 正文来源：CH1-R语言概述.tex，代码块 78；修订稿第 2068 行。
# 仅提取章末习题之前的正文；不含习题提示或答案。
# 语法已检查；未宣称全部外部数据与可选分支已执行。
p <- ggplot(data = mtcars, aes(x = hp, y = mpg)) + geom_point()

p + facet_grid(am ~ .) + labs(title = "Fuel Efficiency vs Engine Power by Transmission Style")
