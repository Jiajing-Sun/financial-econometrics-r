# 正文来源：CH10-收益率曲线.tex，代码块 4；修订稿第 576 行。
# 仅提取章末习题之前的正文；不含习题提示或答案。
# 语法已检查；未宣称全部外部数据与可选分支已执行。
if (!identical(Sys.getenv('FIN_ECON_ENABLE_NETWORK'),'1')) stop('此正文示例可能联网；请设置 FIN_ECON_ENABLE_NETWORK=1 后手动运行。')
suppressPackageStartupMessages(library(quantmod))

taus <- c(0.5, 1, 2, 3, 5, 7, 10, 20, 30)

fred <- c("DGS6MO", "DGS1", "DGS2", "DGS3", "DGS5", "DGS7", "DGS10", "DGS20", "DGS30")

getSymbols(fred, src = "FRED", auto.assign = TRUE, warnings = FALSE)

Y <- do.call(merge, lapply(fred, get))

Yc <- Y[complete.cases(Y)]

stopifnot(nrow(Yc) > 0)

last_row <- tail(Yc, 1)

asof <- index(last_row)

par_y <- as.numeric(last_row)/100

make_par_bond <- function(Tm, cR) {
    N <- as.integer(round(2 * Tm))
    tps <- (1:N)/2
    cf <- rep(cR/2, N)
    cf[N] <- cf[N] + 1
    list(t = tps, cf = cf, p = 1)
}

bond_list <- mapply(make_par_bond, Tm = taus, cR = par_y, SIMPLIFY = FALSE)

names(bond_list) <- paste0("T", taus, "Y")

discount_from_piecewise_f <- function(t, tau_knots, f_vec) {
    I <- length(f_vec)
    stopifnot(length(tau_knots) == I + 1)
    integ <- sapply(t, function(tt) {
        if (tt <= 0)
            return(0)
        k <- max(which(tau_knots < tt))
        k <- min(k, I)
        full <- 0
        if (k > 1L) {
            idx <- seq_len(k - 1L)
            full <- sum(f_vec[idx] * diff(tau_knots)[idx])
        }
        full <- full + f_vec[k] * (tt - tau_knots[k])
        full
    })
    exp(-integ)
}

solve_f_i <- function(i, f_vec, tau_knots, bond_list, bracket = c(-0.05, 0.2)) {
    bi <- bond_list[[i]]
    obj <- function(fi) {
        f_tmp <- f_vec
        f_tmp[i] <- fi
        d_i <- discount_from_piecewise_f(bi$t, tau_knots[1:(i + 1)], f_tmp[1:i])
        sum(bi$cf * d_i) - bi$p
    }
    a <- bracket[1]
    b <- bracket[2]
    iter <- 0
    max_iter <- 20
    while (iter < max_iter && obj(a) * obj(b) > 0) {
        a <- a - 0.05
        b <- b + 0.05
        iter <- iter + 1
    }
    if (obj(a) * obj(b) > 0)
        stop("无法在合理区间内找到根：请检查样本或调整初值/区间。")
    uniroot(obj, interval = c(a, b), tol = 1e-10)$root
}

I <- length(taus)

tau_knots <- c(0, taus)

f_hat <- rep(NA_real_, I)

for (i in 1:I) {
    if (i == 1) {
        f_prev <- numeric(1)
    }
    else {
        f_prev <- f_hat[1:(i - 1)]
    }
    f_hat[i] <- solve_f_i(i, f_vec = c(f_prev, NA), tau_knots = tau_knots, bond_list = bond_list)
}

grid <- seq(0, max(taus), by = 0.01)

f_grid <- sapply(grid, function(tt) {
    if (tt <= 0)
        return(f_hat[1])
    k <- max(which(tau_knots < tt))
    k <- min(k, I)
    f_hat[k]
})

d_grid <- discount_from_piecewise_f(grid, tau_knots, f_hat)

stopifnot(all(is.finite(d_grid)), all(d_grid > 0))

y_grid <- -log(d_grid)/pmax(grid, 1e-08)
y_grid[grid == 0] <- f_hat[1]

p_fitted <- sapply(seq_along(bond_list), function(i) {
    bi <- bond_list[[i]]
    d_i <- discount_from_piecewise_f(bi$t, tau_knots[1:(i + 1)], f_hat[1:i])
    sum(bi$cf * d_i)
})

fit_tab <- data.frame(tau = taus, p_target = 1, p_fitted = p_fitted, price_error = p_fitted -
    1)

cat("=== Fama–Bliss 分段常数远期（序贯引导）===\n")

cat("As of:", as.character(asof), "\n\n")

print(round(fit_tab, 8), row.names = FALSE)

op <- par(mfrow = c(1, 3), mar = c(4, 4, 2, 1))

plot(grid, d_grid, type = "l", lwd = 2, xlab = "t (years)", ylab = "d(t)", main = "贴现函数 d(t)（Fama–Bliss）")

abline(h = 1, v = 0, col = "grey80", lty = 3)

plot(grid, 100 * y_grid, type = "l", lwd = 2, xlab = "t (years)", ylab = "y(t) [% p.a., cont.]",
    main = "连续复利收益率 y(t)")

plot(grid, 100 * f_grid, type = "s", lwd = 2, xlab = "t (years)", ylab = "f(t) [% p.a.]", main = "分段常数远期 f(t)")

abline(v = taus, col = "grey85", lty = 3)

par(op)
