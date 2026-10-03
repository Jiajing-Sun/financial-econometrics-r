# 正文来源：CH11-风险管理与极值理论.tex，代码块 1；修订稿第 405 行。
# 仅提取章末习题之前的正文；不含习题提示或答案。
# 语法已检查；未宣称全部外部数据与可选分支已执行。
if (!identical(Sys.getenv('FIN_ECON_ENABLE_NETWORK'),'1')) stop('此正文示例可能联网；请设置 FIN_ECON_ENABLE_NETWORK=1 后手动运行。')
pkgs_needed <- c("quantmod", "zoo")

to_install <- setdiff(pkgs_needed, rownames(installed.packages()))

if (length(to_install)) NULL

invisible(lapply(pkgs_needed, library, character.only = TRUE))

set.seed(123)

getSymbols("^GSPC", src = "yahoo", from = "1980-01-01", auto.assign = TRUE, warnings = FALSE)

px <- Cl(GSPC)

ret <- na.omit(100 * diff(log(px)))

colnames(ret) <- "ret"

Y <- -as.numeric(ret)

Tn <- length(Y)

Y_est <- Y

order_desc <- function(x) sort(x, decreasing = TRUE)

hill_once <- function(x, M) {
    x <- x[is.finite(x)]
    xsort <- order_desc(x)
    stopifnot(M >= 1, M < length(xsort), xsort[M + 1L] > 0)
    xm1 <- xsort[M + 1]
    hk <- mean(log(xsort[1:M]/xm1))
    kappa_hat <- 1/hk
    se <- kappa_hat/sqrt(M)
    list(kappa = kappa_hat, se = se, xm1 = xm1, xsort = xsort)
}

dem_once <- function(x, M) {
    x <- x[is.finite(x)]
    xsort <- order_desc(x)
    stopifnot(M >= 1, M < length(xsort), xsort[M + 1L] > 0)
    z <- log(xsort[1:M]) - log(xsort[M + 1])
    H1 <- mean(z)
    H2 <- mean(z^2)
    gamma_hat <- H1 + 1 - 0.5 * (1 - (H1^2)/H2)^(-1)
    kappa_hat <- 1/gamma_hat
    list(kappa = kappa_hat, gamma = gamma_hat)
}

tail_quantile_uncond <- function(alpha, x, M, kappa_hat) {
    x <- x[is.finite(x)]
    xsort <- order_desc(x)
    Tn <- length(xsort)
    stopifnot(M >= 1, M < Tn, xsort[M + 1] > 0, alpha > 0, alpha < M/Tn, kappa_hat > 0)
    xm1 <- xsort[M + 1]
    L_T <- xm1 * (M/Tn)^(1/kappa_hat)
    L_T * alpha^(-1/kappa_hat)
}

M_max <- min(2000L, floor(0.1 * Tn), sum(Y_est > 0) - 1L)

stopifnot(M_max >= 90L)

M_grid <- seq(50L, M_max, by = 10L)

hill_path <- sapply(M_grid, function(M) hill_once(Y_est, M)$kappa)

roll_sd <- zoo::rollapply(hill_path, width = 5, sd, align = "right", fill = NA)

idx_best <- which.min(roll_sd)

M_star <- M_grid[idx_best]

HILL <- hill_once(Y_est, M_star)

DEM <- dem_once(Y_est, M_star)

cat(sprintf("Hill: κ̂=%.3f (se≈%.3f) at M*=%d\n", HILL$kappa, HILL$se, M_star))

cat(sprintf(" DEM: κ̂=%.3f (γ̂=%.3f)      at M*=%d\n", DEM$kappa, DEM$gamma, M_star))

rank_size_reg <- function(x, M, delta = 0.5) {
    xsort <- order_desc(x)
    i <- 1:M
    y <- log(i - delta)
    X <- log(xsort[i])
    fit <- lm(y ~ X)
    b <- -coef(fit)[2]
    list(b = b, fit = fit, i = i, x_top = xsort[i], y = y, X = X)
}

RS <- rank_size_reg(Y_est, M_star, delta = 0.5)

cat(sprintf("Rank–Size(δ=1/2): 斜率 b̂ ≈ %.3f（≈ κ）\n", RS$b))

par(mfrow = c(2, 2))

plot(M_grid, hill_path, type = "l", lwd = 2, xlab = "M（阈值个数）", ylab = "Hill 尾指数估计 κ̂",
    main = "Hill 曲线（选择 M*）")

abline(v = M_star, lty = 2)

abline(h = HILL$kappa, lty = 3)

plot(log(RS$x_top), RS$y, pch = 16, cex = 0.7, xlab = expression(log ~ X[(i)] ~ "(i=1..M*)"),
    ylab = expression(log(i - 1/2)), main = "Rank–Size 回归（δ=1/2）")

abline(RS$fit, lwd = 2, lty = 2)

legend("bottomleft", sprintf("b̂ ≈ %.2f (≈ κ)", RS$b), bty = "n")

Yp <- Y_est[is.finite(Y_est) & Y_est > 0]

Yp <- sort(Yp)

emp_ccdf <- (length(Yp) - seq_along(Yp) + 0.5)/length(Y_est)

plot(log(Yp), log(pmax(emp_ccdf, 1e-08)), type = "l", lwd = 2, xlab = expression(log ~ y),
    ylab = expression(log ~ bar(F)(y)), main = "CCDF 双对数图（幂律应近线性）")

u_grid <- quantile(Yp, probs = seq(0.8, 0.99, by = 0.01), names = FALSE)

e_u <- sapply(u_grid, function(u) {
    exc <- Yp[Yp > u]
    if (length(exc) > 0)
        mean(exc - u)
    else NA
})

plot(u_grid, e_u, type = "b", pch = 16, xlab = "阈值 u（右尾）", ylab = "均超额 e(u)",
    main = "Mean Excess Plot（Pareto 下近线性上升）")

par(mfrow = c(1, 1))

alphas <- 10^seq(log10(min(1/50, 0.9 * M_star/Tn)), log10(1/20000), length.out = 100)

q_emp <- as.numeric(quantile(Y_est, probs = 1 - alphas, type = 7))

q_ext <- sapply(alphas, tail_quantile_uncond, x = Y_est, M = M_star, kappa_hat = HILL$kappa)

plot(alphas, q_emp, log = "xy", type = "l", lwd = 2, xlab = expression(alpha), ylab = expression(q[1 -
    alpha] ~ "(亏损阈值)"), main = "经验分位 vs Pareto 外推")

lines(alphas, q_ext, lwd = 2, lty = 2)

legend("topright", c("经验分位", "Pareto 外推（Hill）"), lwd = c(2, 2), lty = c(1,
    2), bty = "n")

alpha_list <- c(1/100, 1/250, 1/1000, 1/5000)

alpha_list <- alpha_list[alpha_list < M_star/Tn]

q_hat_tab <- sapply(alpha_list, function(a) tail_quantile_uncond(a, x = Y_est, M = M_star,
    kappa_hat = HILL$kappa))

names(q_hat_tab) <- paste0("α=", alpha_list)

cat("\n极端分位（外推，单位=收益%的亏损阈值）\n")

print(round(q_hat_tab, 3))

cat("\n============== 摘要 ==============\n")

cat(sprintf("Hill(M*=%d): κ̂=%.3f,  se≈%.3f\n", M_star, HILL$kappa, HILL$se))

cat(sprintf(" DEM(M*=%d): κ̂=%.3f (γ̂=%.3f)\n", M_star, DEM$kappa, DEM$gamma))

cat(sprintf(" Rank–Size(δ=1/2, M*=%d): 斜率 b̂≈%.3f（≈ κ）\n", M_star, RS$b))

cat(" 诊断：CCDF 双对数近线性 & mean excess 上升 ≈ 幂律尾（正则变动）相容。\n")

cat(" 分位外推：q_{1-α} ~ α^{-1/κ̂}（尾分位外推公式），仅在 α < M/T 的范围内可信。\n")
