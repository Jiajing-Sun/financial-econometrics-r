# 正文来源：CH4-自回归移动平均模型.tex，代码块 15；正文第 1723 行。
# 最新SVAR部分采用已识别AB限制；全章片段仅手动执行，见manual/index.csv。
dir.create("results/manual", recursive=TRUE, showWarnings=FALSE)
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

png("results/manual/mu_mle_distribution.png")

hist(mu_mle_values, main = "Distribution of mu MLE", xlab = "mu MLE", breaks = 30, probability = TRUE, col = rgb(0.2, 
    0.8, 0.5, 0.7))

lines(density(mu_mle_values), col = "red", lwd = 2)

dev.off()

png("results/manual/sigma_mle_distribution.png")

hist(sigma_mle_values, main = "Distribution of sigma MLE", xlab = "sigma MLE", breaks = 30, probability = TRUE, 
    col = rgb(0.2, 0.8, 0.5, 0.7))

lines(density(sigma_mle_values), col = "red", lwd = 2)

dev.off()
