# 正文来源：CH5-波动率模型.tex，代码块 1；正文第 151 行。
# 只提取章末习题之前的正文代码；原控制台输出未纳入。
# 手动示例：可能依赖前序代码、外部文件、额外R包；参见本章README与manual/index.csv。
# 已移除自动安装、清空工作空间、保存整个工作空间及本机工作目录切换。
dir.create("results/manual", recursive=TRUE, showWarnings=FALSE)
library(rugarch)

library(ggplot2)

library(FinTS)

set.seed(123)

n = 1000

omega = 0.1

alphas = c(0.5, 0.2, 0.1)

eps = rnorm(n)

sigma2 = rep(0, n)

sigma2[1:length(alphas)] = omega/(1 - sum(alphas))

for (i in (length(alphas) + 1):n) {
    sigma2[i] = omega + sum(alphas * eps[(i - 1):(i - length(alphas))]^2)
    eps[i] = rnorm(1, sd = sqrt(sigma2[i]))
}

df = data.frame(time = 1:n, eps = eps)

ggplot(df, aes(x = time, y = eps)) + geom_line() + labs(title = "模拟的ARCH (p) 过程", y = expression(epsilon[t]))

aic_values <- rep(NA, 5)

bic_values <- rep(NA, 5)

p_max <- 5

for (p in 1:p_max) {
    spec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(p, 0)), mean.model = list(armaOrder = c(0, 
        0), include.mean = FALSE))
    fit = try(ugarchfit(spec, data = eps), silent = TRUE)
    aic_values[p] <- infocriteria(fit)[1]
    bic_values[p] <- infocriteria(fit)[2]
}

best_p_aic <- which.min(aic_values)

best_p_bic <- which.min(bic_values)

print(paste("最佳的p值 (AIC) ：", best_p_aic))

print(paste("最佳的p值 (BIC) ：", best_p_bic))

best_spec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(best_p_aic, 0)), mean.model = list(armaOrder = c(0, 
    0), include.mean = FALSE))

best_fit = ugarchfit(best_spec, data = eps)

summary(best_fit)

ljung_box_test = Box.test(best_fit@fit$residuals/best_fit@fit$sigma, lag = 10, type = "Ljung-Box")

print(ljung_box_test)
