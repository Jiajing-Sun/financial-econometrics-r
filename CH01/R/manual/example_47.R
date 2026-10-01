# 正文来源：CH1-R语言概述.tex，代码块 47；正文第 1126 行。
# 只提取章末习题之前的正文代码；原控制台输出未纳入。
# 手动示例：可能依赖前序代码、外部文件、额外R包；参见本章README与manual/index.csv。
# 已移除自动安装、清空工作空间、保存整个工作空间及本机工作目录切换。
dir.create("results/manual", recursive=TRUE, showWarnings=FALSE)
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

hist(x, probability = TRUE, main = "Histogram with Density Curve (Inverse CDF Sampling)", xlab = "Value", col = "lightblue", 
    border = "black")

lines(density(x), col = "red", lwd = 2)
