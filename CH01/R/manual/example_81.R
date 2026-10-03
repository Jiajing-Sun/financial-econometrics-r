# 正文来源：CH1-R语言概述.tex，代码块 81；修订稿第 2095 行。
# 仅提取章末习题之前的正文；不含习题提示或答案。
# 语法已检查；未宣称全部外部数据与可选分支已执行。
ggplot(data = mtcars, aes(x = wt, y = mpg)) + geom_point() + stat_smooth(method = lm, col = "firebrick") +
    scale_y_continuous("Efficiency (MPG) ", limits = c(5, 35), expand = c(0, 0)) + scale_x_continuous("Car Weight",
    limits = c(1, 20), expand = c(0, 0)) + coord_equal() + labs(title = "The Weight to Fuel Efficiency Relationship",
    x = "Vehicle Weight (1000 lbs) ", y = "Efficiency (MPG) ")
