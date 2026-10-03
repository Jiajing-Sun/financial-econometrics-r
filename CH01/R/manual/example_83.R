# 正文来源：CH1-R语言概述.tex，代码块 83；修订稿第 2129 行。
# 仅提取章末习题之前的正文；不含习题提示或答案。
# 语法已检查；未宣称全部外部数据与可选分支已执行。
ggplot(data = mtcars, aes(x = hp, y = mpg)) + geom_point() + facet_grid(. ~ cyl) + theme_minimal() +
    labs(title = "Comparative Engine Power and Efficiency by Cylinder Count")

ggplot(data = mtcars, aes(x = wt, y = mpg)) + geom_point() + coord_cartesian(xlim = c(2, 5)) +
    labs(title = "Detailed View of Weight and Efficiency Correlation")
