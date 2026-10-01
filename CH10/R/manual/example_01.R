# 正文来源：CH10-收益率曲线.tex，代码块 1；正文第 104 行。
# 只提取章末习题之前的正文代码；原控制台输出未纳入。
# 手动示例：可能依赖前序代码、外部文件、额外R包；参见本章README与manual/index.csv。
# 已移除自动安装、清空工作空间、保存整个工作空间及本机工作目录切换。
dir.create("results/manual", recursive=TRUE, showWarnings=FALSE)
if (!identical(Sys.getenv('FIN_ECON_ENABLE_NETWORK'), '1')) stop('此正文案例会联网；确认数据口径后设置 FIN_ECON_ENABLE_NETWORK=1 再手动运行。', call.=FALSE)
suppressPackageStartupMessages(library(quantmod))

taus <- c(0.25, 0.5, 1, 2, 3, 5, 7, 10, 20, 30)

fred <- c("DGS3MO", "DGS6MO", "DGS1", "DGS2", "DGS3", "DGS5", "DGS7", "DGS10", "DGS20", "DGS30")

getSymbols(fred, src = "FRED", auto.assign = TRUE, warnings = FALSE)

Y <- do.call(merge, lapply(fred, get))

Yc <- Y[stats::complete.cases(Y)]

stopifnot(nrow(Yc) > 0)

last_row <- tail(Yc, 1)

asof <- index(last_row)

y_annual_pct <- as.numeric(last_row)

y_annual <- y_annual_pct/100

y_cc <- log(1 + y_annual)

d_tau <- exp(-taus * y_cc)

g_tau <- taus * y_cc

fit <- smooth.spline(x = taus, y = g_tau, spar = NULL)

gprime <- predict(fit, x = taus, deriv = 1)$y

f_tau <- gprime

f_disc <- rep(NA_real_, length(taus) - 1)

for (i in 1:(length(taus) - 1)) {
    f_disc[i] <- d_tau[i]/d_tau[i + 1] - 1
}

res <- data.frame(asof = as.character(asof), tau_year = taus, y_annual = y_annual, y_cc = y_cc, d_tau = d_tau, 
    f_cc = f_tau)

disc_tab <- data.frame(from_tau = taus[-length(taus)], to_tau = taus[-1], f_disc = f_disc)

cat("=== FRED Constant Maturity US Treasury — as of", as.character(asof), "===\n")

num_cols_res <- sapply(res, is.numeric)

res_print <- res

res_print[num_cols_res] <- lapply(res_print[num_cols_res], round, 6)

print(res_print, row.names = FALSE)

cat("\n--- Discrete one-period forwards between adjacent nodes (annual compounding) ---\n")

num_cols_disc <- sapply(disc_tab, is.numeric)

disc_print <- disc_tab

disc_print[num_cols_disc] <- lapply(disc_print[num_cols_disc], round, 6)

print(disc_print, row.names = FALSE)

op <- par(mfrow = c(1, 3), mar = c(4, 4, 2, 1))

plot(taus, y_cc * 100, type = "b", pch = 19, xlab = "Maturity τ (years)", ylab = "Yield y(τ) [% p.a., cont.]", 
    main = "Zero-coupon Yield (cont.)")

plot(taus, d_tau, type = "b", pch = 19, xlab = "Maturity τ (years)", ylab = "Discount d(τ)", main = "Discount Function")

plot(taus, f_tau * 100, type = "b", pch = 19, xlab = "Maturity τ (years)", ylab = "Instantaneous f(τ) [% p.a.]", 
    main = "Instantaneous Forward (cont.)")

par(op)
