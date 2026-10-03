# 正文来源：CH4-自回归移动平均模型.tex，代码块 5；修订稿第 942 行。
# 仅提取章末习题之前的正文；不含习题提示或答案。
# 语法已检查；未宣称全部外部数据与可选分支已执行。
library(forecast)

set.seed(123456)

ar1_sim <- arima.sim(n = 1000, model = list(ar = c(0.9)))

png(filename = "ar1_pacf.png", width = 400, height = 300)

pacf(ar1_sim, main = "PACF for AR (1) Process")

dev.off()

ma1_sim <- arima.sim(n = 1000, model = list(ma = c(0.9)))

png(filename = "ma1_pacf.png", width = 400, height = 300)

pacf(ma1_sim, main = "PACF for MA (1) ")

dev.off()

pacf_values <- pacf(ma1_sim, plot = FALSE)$acf

print(pacf_values)
