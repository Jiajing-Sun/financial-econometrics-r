# 正文来源：CH1-R语言概述.tex，代码块 47；修订稿第 1142 行。
# 仅提取章末习题之前的正文；不含习题提示或答案。
# 语法已检查；未宣称全部外部数据与可选分支已执行。
set.seed(123456789)

compute_phi <- function(z) {
    integrand <- function(t) (1/sqrt(2 * pi)) * exp(-t^2/2)
    res <- integrate(integrand, lower = -Inf, upper = z)
    res$value
}

z_val <- 1.96

print(compute_phi(z_val))

print(pnorm(z_val))

inverse_phi <- function(phi_value) {
    f <- function(z) compute_phi(z) - phi_value
    root <- uniroot(f, interval = c(-8, 8), tol = 1e-08)
    root$root
}

u <- runif(1000)

x <- as.numeric(lapply(u, inverse_phi))

hist(x, probability = TRUE, main = "Histogram with Density Curve (Inverse CDF Sampling)", xlab = "Value",
    col = "lightblue", border = "black")

lines(density(x), col = "red", lwd = 2)
