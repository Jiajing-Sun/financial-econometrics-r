# 正文来源：CH4-自回归移动平均模型.tex，代码块 13；修订稿第 1397 行。
# 仅提取章末习题之前的正文；不含习题提示或答案。
# 语法已检查；未宣称全部外部数据与可选分支已执行。
set.seed(123)

n <- 500

data <- arima.sim(n = n, model = list(ar = 0.5, ma = 0.4))

library(forecast)

fit_aic <- auto.arima(data, ic = "aic", stepwise = FALSE, approximation = FALSE)

fit_bic <- auto.arima(data, ic = "bic", stepwise = FALSE, approximation = FALSE)

print(fit_aic)

print(fit_bic)
