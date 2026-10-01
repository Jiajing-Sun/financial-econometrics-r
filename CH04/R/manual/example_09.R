# 正文来源：CH4-自回归移动平均模型.tex，代码块 9；正文第 1173 行。
# 最新SVAR部分采用已识别AB限制；全章片段仅手动执行，见manual/index.csv。
dir.create("results/manual", recursive=TRUE, showWarnings=FALSE)
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
