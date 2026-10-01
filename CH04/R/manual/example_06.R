# 正文来源：CH4-自回归移动平均模型.tex，代码块 6；正文第 1036 行。
# 最新SVAR部分采用已识别AB限制；全章片段仅手动执行，见manual/index.csv。
dir.create("results/manual", recursive=TRUE, showWarnings=FALSE)
set.seed(123456)

ts_data <- arima.sim(n = 1000, model = list(ar = c(0.6, -0.4)))

acf_vals <- acf(ts_data, plot = FALSE, lag.max = 2)$acf

gamma0 <- acf_vals[1]

gamma1 <- acf_vals[2]

gamma2 <- acf_vals[3]

mat <- matrix(c(gamma0, gamma1, gamma1, gamma0), nrow = 2)

vec <- c(gamma1, gamma2)

phi <- solve(mat, vec)

print(phi)
