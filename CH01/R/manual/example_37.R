# 正文来源：CH1-R语言概述.tex，代码块 37；修订稿第 927 行。
# 仅提取章末习题之前的正文；不含习题提示或答案。
# 语法已检查；未宣称全部外部数据与可选分支已执行。
plot(GDP.ts, type = "l", col = "blue", xlab = "Year", ylab = "GDP", main = "China's GDP", lwd = 2,
    ylim = c(min(China_GDP$GDP), max(China_GDP$GDP)))

grid(col = "gray85", lty = "solid")
