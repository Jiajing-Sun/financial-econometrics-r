# 正文来源：CH4-自回归移动平均模型.tex，代码块 3；正文第 699 行。
# 最新SVAR部分采用已识别AB限制；全章片段仅手动执行，见manual/index.csv。
dir.create("results/manual", recursive=TRUE, showWarnings=FALSE)
invisible(NULL)

if (!requireNamespace("ggplot2", quietly = TRUE)) stop("请按README手动安装依赖。")

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
