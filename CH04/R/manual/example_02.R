# 正文来源：CH4-自回归移动平均模型.tex，代码块 2；正文第 531 行。
# 最新SVAR部分采用已识别AB限制；全章片段仅手动执行，见manual/index.csv。
dir.create("results/manual", recursive=TRUE, showWarnings=FALSE)
invisible(NULL)

set.seed(123)

simulate_ma2 <- function(n, theta1, theta2, sigma = 1) {
    stopifnot(n >= 1, is.numeric(theta1), is.numeric(theta2), sigma > 0)
    eps <- rnorm(n + 2, sd = sigma)
    x <- numeric(n)
    for (i in seq_len(n)) {
        x[i] <- eps[i + 2] + theta1 * eps[i + 1] + theta2 * eps[i]
    }
    x
}

n <- 1000

theta1 <- 0.5

theta2 <- 0.3

sigma <- 1

x <- simulate_ma2(n, theta1, theta2, sigma)

df <- data.frame(time = seq_len(n), value = x)

if (!requireNamespace("ggplot2", quietly = TRUE)) stop("请按README手动安装依赖。")

library(ggplot2)

p <- ggplot(df, aes(x = time, y = value)) + geom_line(linewidth = 0.4) + labs(title = sprintf("Simulated MA(2) Process: theta1=%.2f, theta2=%.2f, sigma=%.2f", 
    theta1, theta2, sigma), x = "Time", y = "Value") + theme_minimal(base_size = 12)

print(p)
