# 正文来源：CH4-自回归移动平均模型.tex，代码块 5；正文第 936 行。
# 最新SVAR部分采用已识别AB限制；全章片段仅手动执行，见manual/index.csv。
dir.create("results/manual", recursive=TRUE, showWarnings=FALSE)
library(forecast)

set.seed(123456)

ar1_sim <- arima.sim(n = 1000, model = list(ar = c(0.9)))

png(filename = "results/manual/ar1_pacf.png", width = 400, height = 300)

pacf(ar1_sim, main = "PACF for AR (1) Process")

dev.off()

ma1_sim <- arima.sim(n = 1000, model = list(ma = c(0.9)))

png(filename = "results/manual/ma1_pacf.png", width = 400, height = 300)

pacf(ma1_sim, main = "PACF for MA (1) ")

dev.off()

pacf_values <- pacf(ma1_sim, plot = FALSE)$acf

print(pacf_values)
