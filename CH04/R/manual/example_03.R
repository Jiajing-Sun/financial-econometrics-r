# 正文来源：CH4-自回归移动平均模型.tex，代码块 3；修订稿第 705 行。
# 仅提取章末习题之前的正文；不含习题提示或答案。
# 语法已检查；未宣称全部外部数据与可选分支已执行。
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable() && !is.null(rstudioapi::getActiveDocumentContext()$path)) {
}

if (!requireNamespace("ggplot2", quietly = TRUE)) NULL

library(ggplot2)

set.seed(12345)

n <- 1000

ar_param <- 0.6

ma_param <- 0.7

x <- arima.sim(model = list(order = c(1, 0, 1), ar = ar_param, ma = ma_param), n = n)

df <- data.frame(time = seq_len(n), value = as.numeric(x))

p <- ggplot(df, aes(x = time, y = value)) + geom_line(linewidth = 0.4) + labs(title = sprintf("Simulated ARMA(1,1) Process: ar=%.2f, ma=%.2f",
    ar_param, ma_param), x = "Time", y = "Value") + theme_minimal(base_size = 12)

print(p)
