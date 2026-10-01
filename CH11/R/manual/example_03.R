# 正文来源：CH11-风险管理与极值理论.tex，代码块 3；正文第 1094 行。
# 只提取章末习题之前的正文代码；原控制台输出未纳入。
# 手动示例：可能依赖前序代码、外部文件、额外R包；参见本章README与manual/index.csv。
# 已移除自动安装、清空工作空间、保存整个工作空间及本机工作目录切换。
dir.create("results/manual", recursive=TRUE, showWarnings=FALSE)
if (!identical(Sys.getenv('FIN_ECON_ENABLE_NETWORK'), '1')) stop('此正文案例会联网；确认数据口径后设置 FIN_ECON_ENABLE_NETWORK=1 再手动运行。', call.=FALSE)
req <- c("quantmod", "VineCopula", "copula", "xts", "zoo")

inst <- setdiff(req, rownames(installed.packages()))

if (length(inst)) stop("此手动示例缺少依赖；请先参照章节README自行安装。", call. = FALSE)

suppressPackageStartupMessages(invisible(lapply(req, library, character.only = TRUE)))

ticker_i <- "^GSPC"

ticker_j <- "^IXIC"

start_date <- "2000-01-01"

alpha <- 0.05

set.seed(1)

x_i <- suppressWarnings(getSymbols(ticker_i, src = "yahoo", from = start_date, auto.assign = FALSE))

x_j <- suppressWarnings(getSymbols(ticker_j, src = "yahoo", from = start_date, auto.assign = FALSE))

px_i <- Ad(x_i)

px_j <- Ad(x_j)

px <- na.omit(merge(px_i, px_j))

colnames(px) <- c("Pi", "Pj")

ret <- na.omit(diff(log(px)))

colnames(ret) <- c("ri", "rj")

stopifnot(NROW(ret) > 500)

pobs_emp <- function(x) rank(x, ties.method = "average")/(length(x) + 1)

Ui <- pobs_emp(as.numeric(ret$ri))

Uj <- pobs_emp(as.numeric(ret$rj))

U <- cbind(Ui, Uj)

fit_gauss <- VineCopula::BiCopEst(u1 = U[, 1], u2 = U[, 2], family = 1, method = "mle")

rho_hat <- fit_gauss$par

cat(sprintf("Gaussian Copula: rho_hat = %.3f\n", rho_hat))

fit_bb7 <- VineCopula::BiCopEst(u1 = U[, 1], u2 = U[, 2], family = 9, method = "mle")

par1 <- fit_bb7$par

par2 <- fit_bb7$par2

td <- VineCopula::BiCopPar2TailDep(family = 9, par = par1, par2 = par2)

lambdaL <- td$lower

lambdaU <- td$upper

cat(sprintf("BB7 (Joe–Clayton): par1=%.3f, par2=%.3f | λ_L=%.3f, λ_U=%.3f\n", par1, par2, lambdaL, lambdaU))

ll_g <- fit_gauss$logLik

k_g <- 1

ll_b <- fit_bb7$logLik

k_b <- 2

n <- nrow(U)

AIC_g <- -2 * ll_g + 2 * k_g

BIC_g <- -2 * ll_g + log(n) * k_g

AIC_b <- -2 * ll_b + 2 * k_b

BIC_b <- -2 * ll_b + log(n) * k_b

cat(sprintf("IC: Gauss AIC=%.1f BIC=%.1f | BB7 AIC=%.1f BIC=%.1f\n", AIC_g, BIC_g, AIC_b, BIC_b))

hinv1 <- function(alpha, u1, family, par, par2 = 0) {
    fn <- get("BiCopHinv1", asNamespace("VineCopula"))
    fml <- names(formals(fn))
    if ("h" %in% fml) 
        return(fn(h = alpha, u1 = u1, family = family, par = par, par2 = par2))
    if ("t" %in% fml) 
        return(fn(t = alpha, u1 = u1, family = family, par = par, par2 = par2))
    fn(alpha, u1, family, par, par2)
}

q_emp <- function(x, u) unname(quantile(x, probs = min(max(u, 1e-06), 1 - 1e-06), type = 7, na.rm = TRUE))

covar_from_copula <- function(family, par, par2 = 0, ri, rj, alpha) {
    VaR_i <- q_emp(ri, alpha)
    ui_star <- alpha
    uj_star <- hinv1(alpha, ui_star, family = family, par = par, par2 = par2)
    CoVaR <- q_emp(rj, uj_star)
    uj_med <- hinv1(alpha, 0.5, family = family, par = par, par2 = par2)
    CoVaR_med <- q_emp(rj, uj_med)
    list(CoVaR = CoVaR, CoVaR_med = CoVaR_med, Delta = CoVaR - CoVaR_med, u_i = ui_star, u_j = uj_star, u_j_med = uj_med, 
        VaR_i = VaR_i)
}

ri <- as.numeric(ret$ri)

rj <- as.numeric(ret$rj)

covar_g_ij <- covar_from_copula(family = 1, par = rho_hat, ri = ri, rj = rj, alpha = alpha)

covar_bb7_ij <- covar_from_copula(family = 9, par = par1, par2 = par2, ri = ri, rj = rj, alpha = alpha)

cat(sprintf("\nCoVaR (i→j), α=%.2f:\n", alpha))

cat(sprintf("  Gaussian:  CoVaR=%.4f, baseline=%.4f, ΔCoVaR=%.4f\n", covar_g_ij$CoVaR, covar_g_ij$CoVaR_med, covar_g_ij$Delta))

cat(sprintf("  BB7 (JC):  CoVaR=%.4f, baseline=%.4f, ΔCoVaR=%.4f\n", covar_bb7_ij$CoVaR, covar_bb7_ij$CoVaR_med, 
    covar_bb7_ij$Delta))

covar_g_ji <- covar_from_copula(family = 1, par = rho_hat, ri = rj, rj = ri, alpha = alpha)

covar_bb7_ji <- covar_from_copula(family = 9, par = par1, par2 = par2, ri = rj, rj = ri, alpha = alpha)

cat(sprintf("\nCoVaR (j→i), α=%.2f:\n", alpha))

cat(sprintf("  Gaussian:  CoVaR=%.4f, baseline=%.4f, ΔCoVaR=%.4f\n", covar_g_ji$CoVaR, covar_g_ji$CoVaR_med, covar_g_ji$Delta))

cat(sprintf("  BB7 (JC):  CoVaR=%.4f, baseline=%.4f, ΔCoVaR=%.4f\n", covar_bb7_ji$CoVaR, covar_bb7_ji$CoVaR_med, 
    covar_bb7_ji$Delta))

op <- par(no.readonly = TRUE)

on.exit(par(op), add = TRUE)

par(mfrow = c(1, 2), mar = c(4, 4, 2, 1))

plot(U[, 1], U[, 2], pch = 16, cex = 0.5, col = rgb(0, 0, 0, 0.25), xlab = expression(U[i]), ylab = expression(U[j]), 
    main = "U 空间（伪观测）")

abline(v = alpha, col = "grey60", lty = 2)

points(alpha, covar_g_ij$u_j, pch = 19, col = "steelblue")

points(alpha, covar_bb7_ij$u_j, pch = 19, col = "tomato")

legend("topleft", c("样本", "Ui=α", "u_j* (Gauss)", "u_j* (BB7)"), pch = c(16, NA, 19, 19), lty = c(NA, 2, NA, 
    NA), col = c(rgb(0, 0, 0, 0.25), "grey60", "steelblue", "tomato"), bty = "n", cex = 0.85)

plot(ri, rj, pch = 16, cex = 0.5, col = rgb(0, 0, 0, 0.25), xlab = expression(X^i), ylab = expression(X^j), main = bquote(CoVaR ~ 
    "(" * i %->% j * ")" ~ ~~alpha == .(alpha)))

abline(v = covar_g_ij$VaR_i, col = "grey60", lty = 2)

abline(h = covar_g_ij$CoVaR, col = "steelblue", lty = 1)

abline(h = covar_bb7_ij$CoVaR, col = "tomato", lty = 1)

legend("bottomright", c(expression(X^i == VaR[alpha]^i), "CoVaR (Gauss)", "CoVaR (BB7)"), lty = c(2, 1, 1), col = c("grey60", 
    "steelblue", "tomato"), bty = "n", cex = 0.9)

par(mfrow = c(1, 1))
