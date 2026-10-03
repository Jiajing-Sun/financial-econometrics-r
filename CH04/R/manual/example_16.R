# 正文来源：CH4-自回归移动平均模型.tex，代码块 16；修订稿第 1845 行。
# 仅提取章末习题之前的正文；不含习题提示或答案。
# 语法已检查；未宣称全部外部数据与可选分支已执行。
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable() && !is.null(rstudioapi::getActiveDocumentContext()$path)) {
}

library(ggplot2)

library(forecast)

set.seed(123)

n_samples <- 500

alpha <- 0.9

y <- numeric(n_samples)

epsilon <- rnorm(n_samples, mean = 0, sd = 1)

for (t in 2:n_samples) y[t] <- alpha * y[t - 1] + epsilon[t]

df <- data.frame(t = seq_len(n_samples), y = y)

ts_plot <- ggplot(df, aes(x = t, y = y)) + geom_line(linewidth = 0.4) + labs(title = expression("Simulated series for " ~
    y[t] == 0.9 * y[t - 1] + epsilon[t]), x = "t", y = expression(y[t])) + theme_minimal(base_size = 12)

print(ts_plot)

ggsave("AR1_plot.png", plot = ts_plot, width = 8, height = 4.5, dpi = 300)

cat("时间序列图已保存：", normalizePath("AR1_plot.png"), "\n")

png("AR1_acf_pacf_plot.png", width = 8, height = 4.5, units = "in", res = 300)

par(mfrow = c(1, 2), mar = c(4, 4, 3, 1) + 0.1)

Acf(y, main = expression("ACF for " ~ y[t] == 0.9 * y[t - 1] + epsilon[t]), lag.max = 40)

Pacf(y, main = expression("PACF for " ~ y[t] == 0.9 * y[t - 1] + epsilon[t]), lag.max = 40)

dev.off()
