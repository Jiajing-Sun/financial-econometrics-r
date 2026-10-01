# 正文来源：CH9-连续金融模型.tex，代码块 6；正文第 1274 行。
# 只提取章末习题之前的正文代码；原控制台输出未纳入。
# 手动示例：可能依赖前序代码、外部文件、额外R包；参见本章README与manual/index.csv。
# 已移除自动安装、清空工作空间、保存整个工作空间及本机工作目录切换。
dir.create("results/manual", recursive=TRUE, showWarnings=FALSE)
set.seed(123)

rv_iq_ci <- function(x, level = 0.95) {
    n <- length(x) - 1
    dX <- diff(x)
    RV <- sum(dX^2)
    IQ_hat <- (n/3) * sum(dX^4)
    se <- sqrt(2 * IQ_hat/n)
    z <- qnorm((1 + level)/2)
    ci <- c(RV - z * se, RV + z * se)
    list(RV = RV, IQ_hat = IQ_hat, SE = se, CI = ci)
}

simulate_semimartingale <- function(n) {
    t <- seq(0, 1, length.out = n + 1)
    dt <- 1/n
    mu <- rep(0, n)
    sigma2 <- 0.04 + 0.02 * sin(2 * pi * t[-1])
    sigma <- sqrt(pmax(sigma2, 1e-10))
    eps <- rnorm(n)
    dX <- mu * dt + sigma * sqrt(dt) * eps
    X <- c(0, cumsum(dX))
    QV_true <- sum(sigma^2) * dt
    list(path = X, QV_true = QV_true, sigma2 = sigma2, t = t)
}

single_run <- function(n = 23 * 60) {
    sim <- simulate_semimartingale(n)
    out <- rv_iq_ci(sim$path, level = 0.95)
    Z_true <- sqrt(n) * (out$RV - sim$QV_true)/sqrt(2 * (sum(diff(sim$path)^4) * n/3))
    list(n = n, RV = out$RV, IQ_hat = out$IQ_hat, SE = out$SE, CI = out$CI, QV_true = sim$QV_true, Z_true = Z_true)
}

res1 <- single_run(n = 23 * 60)

cat("单次模拟结果：\n")

print(res1[c("n", "RV", "QV_true", "SE", "CI", "Z_true")])

mc_check <- function(n = 23 * 60, R = 500, level = 0.95) {
    z <- qnorm((1 + level)/2)
    cover <- numeric(R)
    Z <- numeric(R)
    for (r in 1:R) {
        sim <- simulate_semimartingale(n)
        x <- sim$path
        out <- rv_iq_ci(x, level = level)
        cover[r] <- (sim$QV_true >= out$CI[1] && sim$QV_true <= out$CI[2])
        Z[r] <- sqrt(n) * (out$RV - sim$QV_true)/sqrt(2 * out$IQ_hat)
    }
    list(n = n, R = R, level = level, coverage = mean(cover), Z_mean = mean(Z), Z_sd = sd(Z))
}

res_mc <- mc_check(n = 23 * 60, R = 200, level = 0.95)

cat("\n蒙特卡洛检验：\n")

print(res_mc)
