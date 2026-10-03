# 正文来源：CH1-R语言概述.tex，代码块 77；修订稿第 2052 行。
# 仅提取章末习题之前的正文；不含习题提示或答案。
# 语法已检查；未宣称全部外部数据与可选分支已执行。
ggplot(data = mtcars, aes(x = hp)) + geom_histogram(binwidth = 10, fill = "skyblue", color = "black") +
    labs(title = "Distribution of Horsepower", x = "Engine Power (HP) ", y = "Frequency")
