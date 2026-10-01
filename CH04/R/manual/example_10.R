# 正文来源：CH4-自回归移动平均模型.tex，代码块 10；正文第 1213 行。
# 最新SVAR部分采用已识别AB限制；全章片段仅手动执行，见manual/index.csv。
dir.create("results/manual", recursive=TRUE, showWarnings=FALSE)
set.seed(123)

data <- rnorm(100, mean = 5, sd = 3)

mle_mu <- mean(data)

mle_sigma2 <- sum((data - mle_mu)^2)/length(data)

cat("MLE for mu:", mle_mu, "\n")

cat("MLE for sigma^2:", mle_sigma2, "\n")
