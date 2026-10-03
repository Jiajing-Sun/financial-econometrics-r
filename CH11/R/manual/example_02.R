# 正文来源：CH11-风险管理与极值理论.tex，代码块 2；修订稿第 640 行。
# 仅提取章末习题之前的正文；不含习题提示或答案。
# 语法已检查；未宣称全部外部数据与可选分支已执行。
if (!identical(Sys.getenv('FIN_ECON_ENABLE_NETWORK'),'1')) stop('此正文示例可能联网；请设置 FIN_ECON_ENABLE_NETWORK=1 后手动运行。')
RUN_A <- TRUE

RUN_B <- TRUE

RUN_C <- TRUE

ticker <- "^GSPC"

start_date <- "1950-01-01"

window_days <- 250

forecast.length <- 1000

alpha_set <- c(0.01, 0.05)

set.seed(1)

req <- c("quantmod", "PerformanceAnalytics", "rugarch", "evir", "xts", "zoo")

inst <- req[!req %in% installed.packages()[, "Package"]]

if (length(inst)) NULL

suppressPackageStartupMessages(invisible(lapply(req, library, character.only = TRUE)))

rho_alpha <- function(u, alpha) u * (alpha - (u < 0))

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
    Tn <- length(x)
    stopifnot(length(alpha) == 1L, M == as.integer(M), M >= 1, M < Tn, alpha > 0, alpha < M/Tn,
        is.finite(kappa_hat), kappa_hat > 0)
    xm1 <- sort(x, decreasing = TRUE)[M + 1L]
    stopifnot(xm1 > 0)
    xm1 * (M/(Tn * alpha))^(1/kappa_hat)
}

backtest_basic <- function(x, VaR, alpha) {
    stopifnot(length(x) == length(VaR), length(x) >= 2L, all(is.finite(x)), all(is.finite(VaR)),
        length(alpha) == 1L, alpha > 0, alpha < 1)
    hit <- x < -VaR
    H <- length(hit)
    n <- sum(hit)
    p_hat <- n/H
    LR_uc <- max(0, 2 * (dbinom(n, H, p_hat, log = TRUE) - dbinom(n, H, alpha, log = TRUE)))
    p_uc <- pchisq(LR_uc, df = 1, lower.tail = FALSE)
    h1 <- head(hit, -1)
    h2 <- tail(hit, -1)
    n00 <- sum(!h1 & !h2)
    n01 <- sum(!h1 & h2)
    n10 <- sum(h1 & !h2)
    n11 <- sum(h1 & h2)
    LR_ind <- p_ind <- NA_real_
    if (all(c(n00, n01, n10, n11) > 0)) {
        pi0 <- n01/(n00 + n01)
        pi1 <- n11/(n10 + n11)
        pi <- (n01 + n11)/(H - 1)
        logL1 <- dbinom(n01, n00 + n01, pi0, log = TRUE) + dbinom(n11, n10 + n11, pi1, log = TRUE)
        logL0 <- dbinom(n01, n00 + n01, pi, log = TRUE) + dbinom(n11, n10 + n11, pi, log = TRUE)
        LR_ind <- max(0, 2 * (logL1 - logL0))
        p_ind <- pchisq(LR_ind, df = 1, lower.tail = FALSE)
    }
    list(H = H, n = n, p_hat = p_hat, LR_uc = LR_uc, p_uc = p_uc, p_exact = binom.test(n, H,
        alpha)$p.value, LR_ind = LR_ind, p_ind = p_ind, hit = hit)
}

print_bt <- function(name, res) {
    cat(sprintf("%-18s | 命中率=%.3f (n=%d/H=%d) | Kupiec p=%.3f | 独立性p=%.3f\n", name,
        res$p_hat, res$n, res$H, res$p_uc, res$p_ind))
}

message("Downloading data from Yahoo Finance ...")

x <- suppressWarnings(getSymbols(ticker, from = start_date, auto.assign = FALSE))

px <- Ad(x)

ret <- na.omit(dailyReturn(px, type = "log"))

colnames(ret) <- "ret"

loss <- -ret

stopifnot(NROW(ret) > forecast.length + window_days + 20)

ret_in <- head(ret, -forecast.length)

ret_out <- tail(ret, forecast.length)

loss_in <- -ret_in

loss_out <- -ret_out

ret_all <- ret

y_all <- -as.numeric(ret_all)

T_total <- length(y_all)

T_est <- floor(0.8 * T_total)

ret_est <- ret_all[1:T_est]

ret_eval <- ret_all[(T_est + 1):T_total]

y_est <- y_all[1:T_est]

if (RUN_A) {
    message("A) Rolling VaR: Hist / Gaussian / GARCH-t / EVT-POT ...")
    hist_roll_var <- sapply(1:forecast.length, function(i) {
        idx_end <- NROW(loss_in) + i - 1
        idx_start <- idx_end - window_days + 1
        w <- loss[idx_start:idx_end, 1]
        c(VaR_99 = as.numeric(quantile(w, 0.99, na.rm = TRUE)), VaR_95 = as.numeric(quantile(w,
            0.95, na.rm = TRUE)))
    })
    hist_roll_var <- xts(t(hist_roll_var), order.by = index(ret_out))
    colnames(hist_roll_var) <- c("VaR_99", "VaR_95")
    gauss_roll_var <- sapply(1:forecast.length, function(i) {
        idx_end <- NROW(loss_in) + i - 1
        idx_start <- idx_end - window_days + 1
        w <- loss[idx_start:idx_end, 1]
        mu <- mean(w)
        sig <- sd(w)
        c(VaR_99 = as.numeric(qnorm(0.99, mu, sig)), VaR_95 = as.numeric(qnorm(0.95, mu, sig)))
    })
    gauss_roll_var <- xts(t(gauss_roll_var), order.by = index(ret_out))
    colnames(gauss_roll_var) <- c("VaR_99", "VaR_95")
    spec_t <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), mean.model = list(armaOrder = c(0,
        0), include.mean = TRUE), distribution.model = "std")
    roll <- ugarchroll(spec_t, data = ret_all, n.ahead = 1, forecast.length = forecast.length,
        refit.every = window_days, refit.window = "moving", solver = "hybrid", solver.control = list(trace = 0),
        keep.coef = TRUE)
    dens_df <- as.data.frame(roll@forecast$density)
    nms <- colnames(dens_df)
    idx_mu <- which(nms == "Mu")
    idx_sigma <- which(nms == "Sigma")
    idx_shape <- grep("shape|df|nu", nms, ignore.case = TRUE)
    stopifnot(length(idx_mu) * length(idx_sigma) * length(idx_shape) > 0)
    mu_t <- as.numeric(dens_df[, idx_mu[1]])
    sg_t <- as.numeric(dens_df[, idx_sigma[1]])
    nu_t <- as.numeric(dens_df[, idx_shape[1]])
    q01 <- qdist("std", 0.01, mu = 0, sigma = 1, skew = 1, shape = nu_t)
    q05 <- qdist("std", 0.05, mu = 0, sigma = 1, skew = 1, shape = nu_t)
    garch_t_var_loss <- xts(cbind(VaR_99 = -(mu_t + sg_t * q01), VaR_95 = -(mu_t + sg_t * q05)),
        order.by = index(ret_out))
    u <- as.numeric(quantile(loss_in, 0.95))
    fit_gpd <- evir::gpd(as.numeric(loss_in), u)
    pe <- fit_gpd$par.ests
    nm <- tolower(names(pe))
    xi <- as.numeric(pe[which(nm == "xi")[1]])
    beta <- as.numeric(pe[which(nm %in% c("beta", "scale"))[1]])
    lambda_hat <- mean(as.numeric(loss_in) > u)
    gpd_var_uncond <- function(p, u, xi, beta, lambda) {
        if (abs(xi) < 1e-08)
            u + beta * log(lambda/(1 - p))
        else u + (beta/xi) * ((lambda/(1 - p))^xi - 1)
    }
    evt_99 <- gpd_var_uncond(0.99, u, xi, beta, lambda_hat)
    evt_95 <- gpd_var_uncond(0.95, u, xi, beta, lambda_hat)
    evt_var_loss <- xts(cbind(VaR_99 = rep(evt_99, NROW(ret_out)), VaR_95 = rep(evt_95, NROW(ret_out))),
        order.by = index(ret_out))
    uc <- function(actual, var, a, name) {
        a_num <- as.numeric(actual)
        v_num <- as.numeric(var)
        keep <- is.finite(a_num) & is.finite(v_num)
        res <- backtest_basic(a_num[keep], v_num[keep], a)
        print_bt(name, res)
        invisible(res)
    }
    cat("\n[A] 回测（样本外）\n")
    btA <- list(hist99 = uc(ret_out, hist_roll_var$VaR_99, 0.01, "Hist 99%"), hist95 = uc(ret_out,
        hist_roll_var$VaR_95, 0.05, "Hist 95%"), gau99 = uc(ret_out, gauss_roll_var$VaR_99,
        0.01, "Normal 99%"), gau95 = uc(ret_out, gauss_roll_var$VaR_95, 0.05, "Normal 95%"),
        gar99 = uc(ret_out, garch_t_var_loss$VaR_99, 0.01, "GARCH-t 99%"), gar95 = uc(ret_out,
            garch_t_var_loss$VaR_95, 0.05, "GARCH-t 95%"), evt99 = uc(ret_out, evt_var_loss$VaR_99,
            0.01, "EVT 99%"), evt95 = uc(ret_out, evt_var_loss$VaR_95, 0.05, "EVT 95%"))
    last_99 <- c(Historical = as.numeric(last(hist_roll_var$VaR_99)), Gaussian = as.numeric(last(gauss_roll_var$VaR_99)),
        GARCH_t = as.numeric(last(garch_t_var_loss$VaR_99)), EVT_POT = as.numeric(last(evt_var_loss$VaR_99)))
    VaR_to_pct <- function(v) 100 * (1 - exp(-v))
    cat("\n[A] 最近 1 步 99% VaR（正损失阈值，log-return 单位）\n")
    print(round(last_99, 5))
    cat("\n[A] 约当百分比跌幅（%）\n")
    print(round(VaR_to_pct(last_99), 2))
    message("[A] Plotting last ~1y returns & 99% VaR ...")
    op <- par(no.readonly = TRUE)
    on.exit(par(op), add = TRUE)
    par(mar = c(4, 4, 2, 1))
    plot_ret <- tail(ret_out, 252)
    plot(index(plot_ret), coredata(plot_ret), type = "h", main = paste0(ticker, " Returns & 99% VaR (last ~1y)"),
        xlab = "", ylab = "log-return", col = "grey40")
    lines(index(plot_ret), -coredata(hist_roll_var$VaR_99[index(plot_ret)]), lwd = 2)
    lines(index(plot_ret), -coredata(gauss_roll_var$VaR_99[index(plot_ret)]), lwd = 2, lty = 2)
    lines(index(plot_ret), -coredata(garch_t_var_loss$VaR_99[index(plot_ret)]), lwd = 2, lty = 3)
    lines(index(plot_ret), -coredata(evt_var_loss$VaR_99[index(plot_ret)]), lwd = 2, lty = 4)
    legend("bottomleft", c("Returns", "Hist 99%", "Normal 99%", "GARCH-t 99%", "EVT 99%"),
        lty = c(1, 1, 2, 3, 4), lwd = c(1, 2, 2, 2, 2), bty = "n")
}

if (RUN_B) {
    message("B) Semi-parametric tail thickness: Hill/DEM/diagnostics ...")
    M_max <- min(2000L, floor(0.1 * T_est), sum(y_est > 0) - 1L)
    stopifnot(M_max >= 90L)
    M_grid <- seq(50L, M_max, by = 10L)
    hill_path <- sapply(M_grid, function(M) hill_once(y_est, M)$kappa)
    roll_sd <- zoo::rollapply(hill_path, width = 5, sd, align = "right", fill = NA)
    M_star <- M_grid[which.min(roll_sd)]
    HILL <- hill_once(y_est, M_star)
    DEM <- dem_once(y_est, M_star)
    cat(sprintf("[B] Hill: κ̂=%.3f (se≈%.3f) @ M*=%d\n", HILL$kappa, HILL$se, M_star))
    cat(sprintf("[B]  DEM: κ̂=%.3f (γ̂=%.3f) @ M*=%d\n", DEM$kappa, DEM$gamma, M_star))
    par(mfrow = c(2, 2))
    plot(M_grid, hill_path, type = "l", lwd = 2, xlab = "M", ylab = "Hill κ̂", main = "Hill 曲线（阈值选择）")
    abline(v = M_star, lty = 2)
    abline(h = HILL$kappa, lty = 3)
    xsort <- order_desc(y_est)
    i <- 1:M_star
    delta <- 0.5
    y_rs <- log(i - delta)
    X_rs <- log(xsort[i])
    fit_rs <- lm(y_rs ~ X_rs)
    b_hat <- -coef(fit_rs)[2]
    plot(X_rs, y_rs, pch = 16, cex = 0.7, xlab = expression(log ~ X[(i)]), ylab = expression(log(i -
        1/2)), main = "Rank-Size 回归 (δ=1/2)")
    abline(fit_rs, lwd = 2, lty = 2)
    legend("bottomleft", sprintf("斜率 b̂ ≈ %.2f (≈ κ)", b_hat), bty = "n")
    Yp <- sort(y_est[is.finite(y_est) & y_est > 0])
    emp_ccdf <- (length(Yp) - seq_along(Yp) + 0.5)/length(y_est)
    plot(log(Yp), log(pmax(emp_ccdf, 1e-10)), type = "l", lwd = 2, xlab = expression(log ~
        y), ylab = expression(log ~ bar(F)(y)), main = "CCDF 双对数图")
    u_grid <- quantile(Yp, probs = seq(0.8, 0.99, by = 0.01), names = FALSE)
    e_u <- sapply(u_grid, function(u) {
        exc <- Yp[Yp > u]
        if (length(exc) > 0)
            mean(exc - u)
        else NA
    })
    plot(u_grid, e_u, type = "b", pch = 16, xlab = "阈值 u", ylab = "均超额 e(u)", main = "Mean Excess Plot（Pareto 下上升）")
    par(mfrow = c(1, 1))
    alphas <- c(1/100, 1/250, 1/1000, 1/5000)
    q_hat <- sapply(alphas, tail_quantile_uncond, x = y_est, M = M_star, kappa_hat = HILL$kappa)
    names(q_hat) <- paste0("α=", alphas)
    cat("[B] 外推 q_{1-α}（单位=亏损阈值，% 收益的近似）：\n")
    print(round(q_hat, 3))
}

if (RUN_C) {
    message("C) Dynamic VaR: GARCH-norm / quasi-nonparam / CaViaR (SAV) ...")
    spec_n <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), mean.model = list(armaOrder = c(0,
        0), include.mean = TRUE), distribution.model = "norm")
    fit_n <- ugarchfit(spec_n, ret_est, solver = "hybrid")
    spec_fix <- spec_n
    setfixed(spec_fix) <- as.list(coef(fit_n))
    filt <- ugarchfilter(spec_fix, data = ret_all)
    mu_all <- as.numeric(fitted(filt))
    sig_all <- as.numeric(sigma(filt))
    mu_t <- mu_all[(T_est + 1):T_total]
    sig_t <- sig_all[(T_est + 1):T_total]
    alpha_v <- 0.01
    VaR_garch_norm <- -(mu_t + sig_t * qnorm(alpha_v))
    eps_est <- residuals(fit_n, standardize = TRUE)
    W <- -as.numeric(na.omit(eps_est))
    M_eps <- min(if (RUN_B) M_star else floor(0.1 * length(W)), sum(W > 0) - 1L)
    stopifnot(M_eps >= 10L)
    H_eps <- hill_once(W, M_eps)
    w_alpha_hat <- function(alpha) {
        T_eps <- length(W)
        if (alpha >= 1/T_eps) {
            unname(quantile(-W, probs = alpha, type = 7))
        }
        else {
            -tail_quantile_uncond(alpha, x = W, M = M_eps, kappa_hat = H_eps$kappa)
        }
    }
    VaR_garch_qnpar <- -(mu_t + sig_t * w_alpha_hat(alpha_v))
    x_eval <- as.numeric(ret_eval)
    caviar_fit <- function(x, alpha, VaR0_init) {
        loss <- function(par, x, alpha, VaR0) {
            b0 <- par[1]
            b1 <- par[2]
            b2 <- par[3]
            H <- length(x)
            VaR <- numeric(H)
            VaR[1] <- b0 + b1 * VaR0 + b2 * abs(0)
            for (t in 2:H) VaR[t] <- b0 + b1 * VaR[t - 1] + b2 * abs(x[t - 1])
            sum(rho_alpha(x + VaR, alpha))
        }
        opt <- optim(c(0.1, 0.9, 0.1), loss, x = x, alpha = alpha, VaR0 = VaR0_init, method = "Nelder-Mead",
            control = list(maxit = 5000))
        par <- opt$par
        H <- length(x)
        VaR <- numeric(H)
        VaR[1] <- par[1] + par[2] * VaR0_init + par[3] * abs(0)
        for (t in 2:H) VaR[t] <- par[1] + par[2] * VaR[t - 1] + par[3] * abs(x[t - 1])
        list(VaR = VaR, par = par)
    }
    VaR0_init <- as.numeric(VaR_garch_norm[1])
    cav <- caviar_fit(x_eval, alpha = alpha_v, VaR0_init = VaR0_init)
    VaR_caviar <- cav$VaR
    bt_gn <- backtest_basic(as.numeric(ret_eval), VaR_garch_norm, alpha_v)
    bt_gq <- backtest_basic(as.numeric(ret_eval), VaR_garch_qnpar, alpha_v)
    bt_cav <- backtest_basic(as.numeric(ret_eval), VaR_caviar, alpha_v)
    cat("\n[C] 回测（alpha=", alpha_v, "）\n", sep = "")
    print_bt("GARCH-norm", bt_gn)
    print_bt("GARCH-qnpar", bt_gq)
    print_bt("CaViaR(SAV)", bt_cav)
    op <- par(mar = c(4, 4, 2, 2))
    plot(index(ret_eval), as.numeric(ret_eval), type = "h", main = paste0("评估期收益与 VaR（α=",
        100 * alpha_v, "%）"), xlab = "", ylab = "日收益(%)", col = "grey40")
    lines(index(ret_eval), -VaR_garch_norm, lwd = 2)
    lines(index(ret_eval), -VaR_garch_qnpar, lwd = 2, lty = 2)
    lines(index(ret_eval), -VaR_caviar, lwd = 2, lty = 3)
    abline(h = 0, col = "darkgrey")
    legend("bottomleft", c("收益", "GARCH-norm", "GARCH-qnpar", "CaViaR(SAV)"), lwd = c(1,
        2, 2, 2), lty = c(1, 1, 2, 3), col = c("grey40", "black", "black", "black"), bty = "n")
    par(op)
}

message("=== Done ===")
