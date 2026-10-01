# 正文来源：CH4-自回归移动平均模型.tex，代码块 12；正文第 1315 行。
# 最新SVAR部分采用已识别AB限制；全章片段仅手动执行，见manual/index.csv。
dir.create("results/manual", recursive=TRUE, showWarnings=FALSE)
set.seed(123)

n <- 500

data <- arima.sim(n = n, model = list(ar = 0.5, ma = 0.4))

fit <- arima(data, order = c(1, 0, 1), method = "ML")

print(fit)
