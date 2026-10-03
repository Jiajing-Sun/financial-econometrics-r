# 正文来源：CH4-自回归移动平均模型.tex，代码块 4；修订稿第 877 行。
# 仅提取章末习题之前的正文；不含习题提示或答案。
# 语法已检查；未宣称全部外部数据与可选分支已执行。
library(stats)

set.seed(123)

ma1_sim <- arima.sim(n = 1000, model = list(ma = c(0.5)))

png(filename = "ma1_acf.png", width = 400, height = 300)

acf(ma1_sim, main = "ACF for MA (1) Process")

dev.off()

ma2_sim <- arima.sim(n = 1000, model = list(ma = c(0.5, 0.3)))

png(filename = "ma2_acf.png", width = 400, height = 300)

acf(ma2_sim, main = "ACF for MA (2) Process")

dev.off()
