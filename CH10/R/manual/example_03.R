# 正文来源：CH10-收益率曲线.tex，代码块 3；修订稿第 386 行。
# 仅提取章末习题之前的正文；不含习题提示或答案。
# 语法已检查；未宣称全部外部数据与可选分支已执行。
if (!identical(Sys.getenv('FIN_ECON_ENABLE_NETWORK'),'1')) stop('此正文示例可能联网；请设置 FIN_ECON_ENABLE_NETWORK=1 后手动运行。')
suppressPackageStartupMessages(library(quantmod))

taus <- c(0.5, 1, 2, 3, 5, 7, 10, 20, 30)

fred <- c("DGS6MO", "DGS1", "DGS2", "DGS3", "DGS5", "DGS7", "DGS10", "DGS20", "DGS30")

getSymbols(fred, src = "FRED", auto.assign = TRUE, warnings = FALSE)

Y <- do.call(merge, lapply(fred, get))

Yc <- Y[stats::complete.cases(Y)]

stopifnot(nrow(Yc) > 0)

last_row <- tail(Yc, 1)

asof <- index(last_row)

y_ann <- as.numeric(last_row)/100

bond_list <- vector("list", length(taus))

names(bond_list) <- paste0("T", taus, "Y")

for (k in seq_along(taus)) {
    Tm <- taus[k]
    cR <- y_ann[k]
    N <- as.integer(round(2 * Tm))
    tps <- (1:N)/2
    cf <- rep(cR/2, N)
    cf[N] <- cf[N] + 1
    price <- 1
    bond_list[[k]] <- list(t = tps, cf = cf, p = price)
}

A_NS <- function(t, theta) {
    b0 <- theta[1]
    b1 <- theta[2]
    b2 <- theta[3]
    tau0 <- theta[4]
    E <- exp(-t/tau0)
    b0 * t + b1 * tau0 * (1 - E) + b2 * (tau0 - (t + tau0) * E)
}

d_NS <- function(t, theta) exp(-A_NS(t, theta))

f_NS <- function(t, theta) {
    b0 <- theta[1]
    b1 <- theta[2]
    b2 <- theta[3]
    tau0 <- theta[4]
    b0 + (b1 + b2 * t/tau0) * exp(-t/tau0)
}

A_NSS <- function(t, theta) {
    b0 <- theta[1]
    b1 <- theta[2]
    b2 <- theta[3]
    tau1 <- theta[4]
    b3 <- theta[5]
    tau2 <- theta[6]
    E1 <- exp(-t/tau1)
    E2 <- exp(-t/tau2)
    b0 * t + b1 * tau1 * (1 - E1) + b2 * (tau1 - (t + tau1) * E1) + b3 * (tau2 - (t + tau2) *
        E2)
}

d_NSS <- function(t, theta) exp(-A_NSS(t, theta))

f_NSS <- function(t, theta) {
    b0 <- theta[1]
    b1 <- theta[2]
    b2 <- theta[3]
    tau1 <- theta[4]
    b3 <- theta[5]
    tau2 <- theta[6]
    b0 + b1 * exp(-t/tau1) + b2 * (t/tau1) * exp(-t/tau1) + b3 * (t/tau2) * exp(-t/tau2)
}

price_error_NS <- function(theta) {
    if (theta[4] <= 1e-06)
        return(1e+12)
    err <- 0
    for (i in seq_along(bond_list)) {
        bi <- bond_list[[i]]
        di <- d_NS(bi$t, theta)
        p_hat <- sum(bi$cf * di)
        err <- err + (p_hat - bi$p)^2
    }
    err
}

price_error_NSS <- function(theta) {
    if (theta[4] <= 1e-06 || theta[6] <= 1e-06)
        return(1e+12)
    err <- 0
    for (i in seq_along(bond_list)) {
        bi <- bond_list[[i]]
        di <- d_NSS(bi$t, theta)
        p_hat <- sum(bi$cf * di)
        err <- err + (p_hat - bi$p)^2
    }
    err
}

theta0_NS <- c(b0 = mean(y_ann), b1 = -0.02, b2 = 0.02, tau0 = 2)

lower_NS <- c(-0.1, -5, -5, 0.001)

upper_NS <- c(0.2, 5, 5, 10)

fit_NS <- optim(par = theta0_NS, fn = price_error_NS, method = "L-BFGS-B", lower = lower_NS,
    upper = upper_NS, control = list(maxit = 2000))

theta0_NSS <- c(b0 = mean(y_ann), b1 = -0.03, b2 = 0.06, tau1 = 2, b3 = -0.02, tau2 = 8)

lower_NSS <- c(-0.1, -5, -5, 0.001, -5, 0.001)

upper_NSS <- c(0.2, 5, 5, 10, 5, 30)

fit_NSS <- optim(par = theta0_NSS, fn = price_error_NSS, method = "L-BFGS-B", lower = lower_NSS,
    upper = upper_NSS, control = list(maxit = 3000))

cat("=== NS 拟合（价格准则）===\nAs of:", as.character(asof), "\n")

print(fit_NS$par)

cat("obj =", fit_NS$value, "\n\n")

cat("=== NSS 拟合（价格准则）===\nAs of:", as.character(asof), "\n")

print(fit_NSS$par)

cat("obj =", fit_NSS$value, "\n\n")

grid <- seq(0.01, max(taus), by = 0.01)

d_ns <- d_NS(grid, fit_NS$par)

y_ns <- -log(d_ns)/grid

f_ns <- f_NS(grid, fit_NS$par)

d_nss <- d_NSS(grid, fit_NSS$par)

y_nss <- -log(d_nss)/grid

f_nss <- f_NSS(grid, fit_NSS$par)

price_fit_tab <- function(d_fun, theta) {
    DF <- data.frame(tau = taus, p_target = sapply(bond_list, function(b) b$p), p_fitted = sapply(bond_list,
        function(b) sum(b$cf * d_fun(b$t, theta))))
    DF$abs_err <- DF$p_fitted - DF$p_target
    DF
}

tab_NS <- price_fit_tab(d_NS, fit_NS$par)

tab_NSS <- price_fit_tab(d_NSS, fit_NSS$par)

cat("NS 回定价误差（价格 - 1）:\n")

print(round(tab_NS, 6), row.names = FALSE)

cat("\nNSS 回定价误差（价格 - 1）:\n")

print(round(tab_NSS, 6), row.names = FALSE)

op <- par(mfrow = c(1, 3), mar = c(4, 4, 2, 1))

plot(grid, d_ns, type = "l", lwd = 2, col = "steelblue", xlab = "t (years)", ylab = "d(t)",
    main = "贴现函数 d(t)")

lines(grid, d_nss, lwd = 2, col = "tomato")

legend("topright", c("NS", "NSS"), lty = 1, col = c("steelblue", "tomato"), bty = "n")

plot(grid, 100 * y_ns, type = "l", lwd = 2, col = "steelblue", xlab = "t (years)", ylab = "y(t) [% p.a., cont.]",
    main = "连续复利收益率 y(t)")

lines(grid, 100 * y_nss, lwd = 2, col = "tomato")

plot(grid, 100 * f_ns, type = "l", lwd = 2, col = "steelblue", xlab = "t (years)", ylab = "f(t) [% p.a.]",
    main = "瞬时远期 f(t)")

lines(grid, 100 * f_nss, lwd = 2, col = "tomato")

par(op)
