# 正文来源：CH4-自回归移动平均模型.tex，代码块 9；修订稿第 1179 行。
# 仅提取章末习题之前的正文；不含习题提示或答案。
# 语法已检查；未宣称全部外部数据与可选分支已执行。
set.seed(123)

data <- rnorm(100, mean = 5, sd = 3)

log_likelihood <- function(params) {
    mu <- params[1]
    sigma2 <- params[2]
    n <- length(data)
    -(-n/2 * log(2 * pi) - n/2 * log(sigma2) - 1/(2 * sigma2) * sum((data - mu)^2))
}

start_values <- c(mu = 0, sigma2 = 1)

result <- optim(start_values, log_likelihood)

cat("MLE for mu:", result$par[1], "\n")

cat("MLE for sigma^2:", result$par[2], "\n")

mle_mu <- mean(data)

mle_sigma2 <- sum((data - mle_mu)^2)/length(data)

cat("MLE for mu:", mle_mu, "\n")

cat("MLE for sigma^2:", mle_sigma2, "\n")
