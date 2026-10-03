# 正文来源：CH4-自回归移动平均模型.tex，代码块 6；修订稿第 1042 行。
# 仅提取章末习题之前的正文；不含习题提示或答案。
# 语法已检查；未宣称全部外部数据与可选分支已执行。
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
