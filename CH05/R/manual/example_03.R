# 正文来源：CH5-波动率模型.tex，代码块 3；修订稿第 320 行。
# 仅提取章末习题之前的正文；不含习题提示或答案。
# 语法已检查；未宣称全部外部数据与可选分支已执行。
library(rugarch)

set.seed(123)

n = 1000

eps = rnorm(n, mean = 0, sd = 1)

sigma2 = rep(1, n)

y = rep(0, n)

alpha0 = 0.01

alpha1 = 0.05

beta1 = 0.9

sigma2[1] = alpha0/(1 - alpha1 - beta1)

for (i in 2:n) {
    sigma2[i] = alpha0 + alpha1 * y[i - 1]^2 + beta1 * sigma2[i - 1]
    y[i] = rnorm(1, mean = 0, sd = sqrt(sigma2[i]))
}

p_max = 3

q_max = 3

aic_values = matrix(NA, nrow = p_max, ncol = q_max, dimnames = list(p = 1:p_max, q = 1:q_max))

bic_values = matrix(NA, nrow = p_max, ncol = q_max, dimnames = list(p = 1:p_max, q = 1:q_max))

for (p in 1:p_max) {
    for (q in 1:q_max) {
        spec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(p, q)), mean.model = list(armaOrder = c(0,
            0), include.mean = FALSE))
        fit = try(ugarchfit(spec = spec, data = y), silent = TRUE)
        aic_values[p, q] = infocriteria(fit)[1]
        bic_values[p, q] = infocriteria(fit)[2]
    }
}

best_aic = which(aic_values == min(aic_values, na.rm = TRUE), arr.ind = TRUE)

best_bic = which(bic_values == min(bic_values, na.rm = TRUE), arr.ind = TRUE)

cat("Best (p,q) by AIC: (", best_aic[1, "p"], ",", best_aic[1, "q"], ") \n")

cat("Best (p,q) by BIC: (", best_bic[1, "p"], ",", best_bic[1, "q"], ") \n")
