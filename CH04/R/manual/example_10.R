# 正文来源：CH4-自回归移动平均模型.tex，代码块 10；修订稿第 1219 行。
# 仅提取章末习题之前的正文；不含习题提示或答案。
# 语法已检查；未宣称全部外部数据与可选分支已执行。
set.seed(123)

data <- rnorm(100, mean = 5, sd = 3)

mle_mu <- mean(data)

mle_sigma2 <- sum((data - mle_mu)^2)/length(data)

cat("MLE for mu:", mle_mu, "\n")

cat("MLE for sigma^2:", mle_sigma2, "\n")
