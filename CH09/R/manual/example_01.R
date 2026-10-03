# 正文来源：CH9-连续金融模型.tex，代码块 1；修订稿第 91 行。
# 仅提取章末习题之前的正文；不含习题提示或答案。
# 语法已检查；未宣称全部外部数据与可选分支已执行。
set.seed(123)

n <- 1000

T <- 1

dt <- T/n

t <- seq(0, T, length.out = n + 1)

dW <- rnorm(n, mean = 0, sd = sqrt(dt))

W <- c(0, cumsum(dW))

plot(t, W, type = "l", main = "标准布朗运动", xlab = "时间", ylab = "W (t) ", col = "blue",
    lwd = 2)
