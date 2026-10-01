# 正文来源：CH5-波动率模型.tex，代码块 6；正文第 901 行。
# 只提取章末习题之前的正文代码；原控制台输出未纳入。
# 手动示例：可能依赖前序代码、外部文件、额外R包；参见本章README与manual/index.csv。
# 已移除自动安装、清空工作空间、保存整个工作空间及本机工作目录切换。
dir.create("results/manual", recursive=TRUE, showWarnings=FALSE)
library(lmtest)

library(fUnitRoots)

set.seed(123)

n <- 100

Y <- arima.sim(model = list(ar = 0.5), n = n)

model <- lm(Y[2:n] ~ Y[1:(n - 1)])

residuals <- model$residuals

p <- 1

residuals_squared <- residuals^2

auxiliary_model <- lm(residuals_squared[(p + 1):n] ~ residuals_squared[1:(n - p)])

lm_stat <- summary(auxiliary_model)$r.squared * (n - p)

lm_stat

sigma_hat_squared <- mean(residuals_squared)

demeaned_residuals_squared <- residuals_squared - sigma_hat_squared

gamma_hat_0 <- var(demeaned_residuals_squared)

p <- 1

gamma_hat_j <- sapply(1:p, function(j) mean(demeaned_residuals_squared[(j + 1):n] * demeaned_residuals_squared[1:(n - 
    j)], na.rm = TRUE))

rho_hat <- gamma_hat_j/gamma_hat_0

ml_stat <- n * sum(rho_hat^2)

ml_stat
