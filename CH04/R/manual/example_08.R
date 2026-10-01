# 正文来源：CH4-自回归移动平均模型.tex，代码块 8；正文第 1077 行。
# 最新SVAR部分采用已识别AB限制；全章片段仅手动执行，见manual/index.csv。
dir.create("results/manual", recursive=TRUE, showWarnings=FALSE)
set.seed(123456)

ts_data <- arima.sim(n = 1000, model = list(ar = c(0.6, -0.4)))

ar_model <- ar.yw(ts_data, order.max = 2)

print(ar_model$ar)
