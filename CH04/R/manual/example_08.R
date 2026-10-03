# 正文来源：CH4-自回归移动平均模型.tex，代码块 8；修订稿第 1083 行。
# 仅提取章末习题之前的正文；不含习题提示或答案。
# 语法已检查；未宣称全部外部数据与可选分支已执行。
set.seed(123456)

ts_data <- arima.sim(n = 1000, model = list(ar = c(0.6, -0.4)))

ar_model <- ar.yw(ts_data, order.max = 2)

print(ar_model$ar)
