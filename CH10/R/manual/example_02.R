# 正文来源：CH10-收益率曲线.tex，代码块 2；正文第 265 行。
# 只提取章末习题之前的正文代码；原控制台输出未纳入。
# 手动示例：可能依赖前序代码、外部文件、额外R包；参见本章README与manual/index.csv。
# 已移除自动安装、清空工作空间、保存整个工作空间及本机工作目录切换。
dir.create("results/manual", recursive=TRUE, showWarnings=FALSE)
if (!identical(Sys.getenv('FIN_ECON_ENABLE_NETWORK'), '1')) stop('此正文案例会联网；确认数据口径后设置 FIN_ECON_ENABLE_NETWORK=1 再手动运行。', call.=FALSE)
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

gfun <- function(tt) bs(tt, degree = 3, knots = knots_inner, Boundary.knots = c(0, t_max), intercept = TRUE)

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

d_hat[d_hat <= 1e-10] <- 1e-10

y_hat <- -log(d_hat)/pmax(grid, 1e-08)

g_tau <- grid * y_hat

f_hat <- c(diff(g_tau)/diff(grid), NA)

f_hat[length(f_hat)] <- f_hat[length(f_hat) - 1]

p_fitted <- numeric(length(bond_list))

for (i in seq_along(bond_list)) {
    bi <- bond_list[[i]]
    d_i <- approx(grid, d_hat, xout = bi$t, rule = 2)$y
    p_fitted[i] <- sum(bi$cf * d_i)
}

fit_tab <- data.frame(tau = taus, p_target = p_vec, p_fitted = p_fitted, abs_err = p_fitted - p_vec)

cat("=== FRED par-yield 构造息票债；基函数平滑贴现曲线 ===\n")

cat("As of:", as.character(asof), "\n\n")

print(round(fit_tab, 6), row.names = FALSE)

op <- par(mfrow = c(1, 3), mar = c(4, 4, 2, 1))

plot(grid, d_hat, type = "l", lwd = 2, xlab = "t (years)", ylab = "d(t)", main = "贴现函数 d(t)（B样条 + 平滑惩罚）")

abline(h = 1, v = 0, col = "grey80", lty = 3)

points(0, 1, pch = 19, col = "steelblue")

plot(grid, y_hat * 100, type = "l", lwd = 2, xlab = "t (years)", ylab = "y(t) [% p.a., cont.]", main = "连续复利收益率 y(t)")

points(taus, log(1 + y_ann)/taus * taus * 100/taus, pch = 19, col = "grey40")

plot(grid, f_hat * 100, type = "l", lwd = 2, xlab = "t (years)", ylab = "f(t) [% p.a.]", main = "瞬时远期 f(t)")

par(op)
