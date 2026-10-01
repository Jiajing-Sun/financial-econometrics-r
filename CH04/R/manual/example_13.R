# 正文来源：CH4-自回归移动平均模型.tex，代码块 13；正文第 1391 行。
# 最新SVAR部分采用已识别AB限制；全章片段仅手动执行，见manual/index.csv。
dir.create("results/manual", recursive=TRUE, showWarnings=FALSE)
set.seed(123)

n <- 500

data <- arima.sim(n = n, model = list(ar = 0.5, ma = 0.4))

library(forecast)

fit_aic <- auto.arima(data, ic = "aic", stepwise = FALSE, approximation = FALSE)

fit_bic <- auto.arima(data, ic = "bic", stepwise = FALSE, approximation = FALSE)

print(fit_aic)

print(fit_bic)
