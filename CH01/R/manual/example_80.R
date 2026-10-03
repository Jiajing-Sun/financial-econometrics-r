# 正文来源：CH1-R语言概述.tex，代码块 80；修订稿第 2085 行。
# 仅提取章末习题之前的正文；不含习题提示或答案。
# 语法已检查；未宣称全部外部数据与可选分支已执行。
ggplot(data = mtcars, aes(x = hp, y = mpg)) + geom_point() + stat_smooth(method = lm, col = "firebrick") +
    labs(title = "Trend in Fuel Efficiency versus Horsepower")
