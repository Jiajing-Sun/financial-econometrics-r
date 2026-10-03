# 正文来源：CH4-自回归移动平均模型.tex，代码块 15；修订稿第 1716 行。
# 仅提取章末习题之前的正文；不含习题提示或答案。
# 语法已检查；未宣称全部外部数据与可选分支已执行。
set.seed(123)

mu_true <- 5

sigma_true <- 2

n <- 1000

num_simulations <- 1e+05

mu_mle_values <- numeric(num_simulations)

sigma_mle_values <- numeric(num_simulations)

for (i in 1:num_simulations) {
    sample <- rnorm(n, mu_true, sigma_true)
    mu_mle_values[i] <- mean(sample)
    sigma_mle_values[i] <- sqrt(sum((sample - mean(sample))^2)/length(sample))
}

cat("Average of mu MLE:", mean(mu_mle_values), "\n")

cat("Average of sigma^2 MLE:", mean(sigma_mle_values^2), "\n")

png("mu_mle_distribution.png")

hist(mu_mle_values, main = "Distribution of mu MLE", xlab = "mu MLE", breaks = 30, probability = TRUE,
    col = rgb(0.2, 0.8, 0.5, 0.7))

lines(density(mu_mle_values), col = "red", lwd = 2)

dev.off()

png("sigma_mle_distribution.png")

hist(sigma_mle_values, main = "Distribution of sigma MLE", xlab = "sigma MLE", breaks = 30,
    probability = TRUE, col = rgb(0.2, 0.8, 0.5, 0.7))

lines(density(sigma_mle_values), col = "red", lwd = 2)

dev.off()
