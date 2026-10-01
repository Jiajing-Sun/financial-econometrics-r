# 正文来源：CH4-自回归移动平均模型.tex，代码块 4；正文第 871 行。
# 最新SVAR部分采用已识别AB限制；全章片段仅手动执行，见manual/index.csv。
dir.create("results/manual", recursive=TRUE, showWarnings=FALSE)
library(stats)

set.seed(123)

ma1_sim <- arima.sim(n = 1000, model = list(ma = c(0.5)))

png(filename = "results/manual/ma1_acf.png", width = 400, height = 300)

acf(ma1_sim, main = "ACF for MA (1) Process")

dev.off()

ma2_sim <- arima.sim(n = 1000, model = list(ma = c(0.5, 0.3)))

png(filename = "results/manual/ma2_acf.png", width = 400, height = 300)

acf(ma2_sim, main = "ACF for MA (2) Process")

dev.off()
