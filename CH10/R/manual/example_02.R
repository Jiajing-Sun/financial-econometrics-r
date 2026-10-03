# 正文来源：CH10-收益率曲线.tex，代码块 2；修订稿第 216 行。
# 仅提取章末习题之前的正文；不含习题提示或答案。
# 语法已检查；未宣称全部外部数据与可选分支已执行。
if (!identical(Sys.getenv('FIN_ECON_ENABLE_NETWORK'),'1')) stop('此正文示例可能联网；请设置 FIN_ECON_ENABLE_NETWORK=1 后手动运行。')
suppressPackageStartupMessages({
    library(quantmod)
    library(splines)
})

taus <- c(0.5, 1, 2, 3, 5, 7, 10, 20, 30)

fred <- c("DGS6MO", "DGS1", "DGS2", "DGS3", "DGS5", "DGS7", "DGS10", "DGS20", "DGS30")

getSymbols(fred, src = "FRED", auto.assign = TRUE, warnings = FALSE)

Y <- do.call(merge, lapply(fred, get))

Yc <- Y[stats::complete.cases(Y)]

stopifnot(nrow(Yc) > 0)

last_row <- tail(Yc, 1)

asof <- index(last_row)

y_ann_pct <- as.numeric(last_row)

y_ann <- y_ann_pct/100

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

t_max <- max(taus)

n_basis <- 12

knots_inner <- seq(0.5, t_max - 0.5, length.out = max(0, n_basis - 4))

gfun <- function(tt) bs(tt, degree = 3, knots = knots_inner, Boundary.knots = c(0, t_max),
    intercept = TRUE)

p_vec <- sapply(bond_list, function(b) b$p)

X <- matrix(0, nrow = length(bond_list), ncol = ncol(gfun(c(0, t_max))))

colnames(X) <- paste0("g", seq_len(ncol(X)))

for (i in seq_along(bond_list)) {
    bi <- bond_list[[i]]
    G <- gfun(bi$t)
    X[i, ] <- colSums(G * bi$cf)
}

G0 <- gfun(0)

w0 <- 1e+06

X_aug <- rbind(X, sqrt(w0) * G0)

p_aug <- c(p_vec, sqrt(w0) * 1)

D2 <- diff(diag(ncol(X)), differences = 2)

lambda <- 0.01

R <- crossprod(D2)

XtX <- crossprod(X_aug)

Xtp <- crossprod(X_aug, p_aug)

theta_hat <- solve(XtX + lambda * R, Xtp)

grid <- seq(0, t_max, by = 0.01)

G_grid <- gfun(grid)

d_hat <- as.vector(G_grid %*% theta_hat)

if (any(!is.finite(d_hat)) || any(d_hat <= 0)) {
    stop("出现无效贴现因子，请采用正值约束或重新设定基函数。")
}

g_tau <- -log(d_hat)

f_hat <- c(diff(g_tau)/diff(grid), NA_real_)

f_hat[length(f_hat)] <- f_hat[length(f_hat) - 1L]

y_hat <- rep(NA_real_, length(grid))

y_hat[grid > 0] <- g_tau[grid > 0]/grid[grid > 0]

y_hat[1] <- f_hat[1]

p_fitted <- numeric(length(bond_list))

for (i in seq_along(bond_list)) {
    bi <- bond_list[[i]]
    d_i <- approx(grid, d_hat, xout = bi$t, rule = 2)$y
    p_fitted[i] <- sum(bi$cf * d_i)
}

fit_tab <- data.frame(tau = taus, p_target = p_vec, p_fitted = p_fitted, price_error = p_fitted -
    p_vec)

cat("=== FRED par-yield 构造息票债；基函数平滑贴现曲线 ===\n")

cat("As of:", as.character(asof), "\n\n")

print(round(fit_tab, 6), row.names = FALSE)

op <- par(mfrow = c(1, 3), mar = c(4, 4, 2, 1))

plot(grid, d_hat, type = "l", lwd = 2, xlab = "t (years)", ylab = "d(t)", main = "贴现函数 d(t)（B样条 + 平滑惩罚）")

abline(h = 1, v = 0, col = "grey80", lty = 3)

points(0, 1, pch = 19, col = "steelblue")

plot(grid, y_hat * 100, type = "l", lwd = 2, xlab = "t (years)", ylab = "y(t) [% p.a., cont.]",
    main = "连续复利收益率 y(t)")

plot(grid, f_hat * 100, type = "l", lwd = 2, xlab = "t (years)", ylab = "f(t) [% p.a.]", main = "瞬时远期 f(t)")

par(op)
