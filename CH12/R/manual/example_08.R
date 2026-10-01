# 正文来源：CH12-金融计量经济学与人工智能方法.tex，代码块 8；正文第 1933 行。
# 只提取章末习题之前的正文代码；原控制台输出未纳入。
# 手动示例：可能依赖前序代码、外部文件、额外R包；参见本章README与manual/index.csv。
# 已移除自动安装、清空工作空间、保存整个工作空间及本机工作目录切换。
dir.create("results/manual", recursive=TRUE, showWarnings=FALSE)
if (!identical(Sys.getenv('FIN_ECON_ENABLE_NETWORK'), '1')) stop('此正文案例会联网；确认数据口径后设置 FIN_ECON_ENABLE_NETWORK=1 再手动运行。', call.=FALSE)
start_date <- as.Date("2015-01-01")

end_date <- as.Date("2024-12-31")

tickers <- c("SPY", "QQQ", "IWM", "EFA", "EEM", "TLT", "IEF", "LQD", "HYG", "GLD", "SLV", "USO", "XLK", "XLF", 
    "XLY", "XLP", "XLV", "XLI", "XLE", "XLU", "XLB")

k_factors <- 3

run_ae <- FALSE

alpha <- 0.95

B_boot <- 200

use_ewma <- TRUE

lambda_ewma <- 0.97

vol_target <- TRUE

set.seed(2025)

pkgs <- c("quantmod", "dplyr", "tidyr", "zoo", "ggplot2", "scales", "MASS", "matrixStats")

for (p in pkgs) if (!requireNamespace(p, quietly = TRUE)) stop("此手动示例缺少依赖；请先参照章节README自行安装。", 
    call. = FALSE)

invisible(lapply(pkgs, library, character.only = TRUE))

if (run_ae) {
    if (!requireNamespace("keras", quietly = TRUE)) 
        stop("此手动示例缺少依赖；请先参照章节README自行安装。", call. = FALSE)
    library(keras)
}

options(stringsAsFactors = FALSE, scipen = 99, timeout = max(300, getOption("timeout")))

get_ok <- function(sym) {
    tryCatch({
        suppressWarnings(getSymbols(sym, src = "yahoo", from = start_date - 30, to = end_date, auto.assign = FALSE))
    }, error = function(e) NULL)
}

raw <- lapply(tickers, get_ok)

names(raw) <- tickers

ok <- vapply(raw, Negate(is.null), logical(1))

if (!all(ok)) message("以下代码下载失败并被剔除：", paste(tickers[!ok], collapse = ", "))

tickers <- tickers[ok]

raw <- raw[ok]

Adj <- lapply(raw, Ad)

idx_all <- Reduce(intersect, lapply(Adj, index))

Adj <- lapply(Adj, function(x) x[idx_all])

R <- do.call(cbind, lapply(Adj, function(p) diff(log(p))))

R <- R[complete.cases(R), ]

colnames(R) <- tickers

na_rate <- colMeans(is.na(R))

keep <- na_rate < 0.01

R <- R[, keep, drop = FALSE]

tickers <- colnames(R)

n <- nrow(R)

split_tr <- floor(0.7 * n)

R_tr <- R[1:split_tr, , drop = FALSE]

R_te <- R[(split_tr + 1):n, , drop = FALSE]

mu_tr <- colMeans(R_tr)

sd_tr <- apply(R_tr, 2, sd)

sd_tr[sd_tr == 0 | is.na(sd_tr)] <- 1

scale_apply <- function(X, mu, sd) sweep(sweep(X, 2, mu, "-"), 2, sd, "/")

Xs_tr <- scale_apply(R_tr, mu_tr, sd_tr)

Xs_te <- scale_apply(R_te, mu_tr, sd_tr)

pca <- prcomp(Xs_tr, center = FALSE, scale. = FALSE)

Z_tr_pca <- pca$x[, 1:k_factors, drop = FALSE]

Z_te_pca <- scale(Xs_te, center = FALSE, scale = FALSE) %*% pca$rotation[, 1:k_factors, drop = FALSE]

if (run_ae) {
    p <- ncol(Xs_tr)
    inputs <- layer_input(shape = p)
    x <- layer_dense(layer_dropout(layer_dense(inputs, units = 64, activation = "relu"), rate = 0.1), units = k_factors, 
        activation = "linear", name = "latent")
    y <- layer_dense(layer_dropout(layer_dense(x, units = 64, activation = "relu"), rate = 0.1), units = p, activation = "linear")
    ae <- compile(keras_model(inputs = inputs, outputs = y), optimizer = optimizer_adam(learning_rate = 0.01), 
        loss = "mse")
    fit(ae, x = as.matrix(Xs_tr), y = as.matrix(Xs_tr), validation_split = 0.1, epochs = 80, batch_size = 64, callbacks = list(callback_early_stopping(monitor = "val_loss", 
        patience = 8, restore_best_weights = TRUE)), verbose = 0)
    encoder <- keras_model(inputs = inputs, outputs = get_layer(ae, "latent")$output)
    Z_tr <- as.matrix(encoder %>% predict(as.matrix(Xs_tr)))
    Z_te <- as.matrix(encoder %>% predict(as.matrix(Xs_te)))
} else {
    Z_tr <- Z_tr_pca
    Z_te <- Z_te_pca
}

colnames(Z_tr) <- paste0("F", 1:k_factors)

colnames(Z_te) <- paste0("F", 1:k_factors)

if (is.null(colnames(Z_tr)) || anyDuplicated(colnames(Z_tr)) > 0) {
    colnames(Z_tr) <- paste0("F", seq_len(ncol(Z_tr)))
}

fac_names <- colnames(Z_tr)

assets <- colnames(R_tr)

k <- length(fac_names)

N <- length(assets)

B_mat <- matrix(NA_real_, nrow = k, ncol = N, dimnames = list(fac_names, assets))

A_vec <- setNames(numeric(N), assets)

S_eps <- setNames(numeric(N), assets)

ols_fit <- function(y, Zdf) {
    fit <- lm(y ~ ., data = cbind(y = y, Zdf))
    cf <- coef(fit)
    beta <- cf[-1]
    names(beta) <- gsub("`", "", names(beta))
    alpha <- unname(cf[1])
    yhat <- as.numeric(alpha + as.matrix(Zdf) %*% beta)
    list(alpha = alpha, beta = beta, yhat = yhat)
}

ridge_fit <- function(y, Zdf, lambda = 1e-04) {
    if (!requireNamespace("MASS", quietly = TRUE)) 
        stop("此手动示例缺少依赖；请先参照章节README自行安装。", call. = FALSE)
    X <- as.matrix(Zdf)
    rr <- MASS::lm.ridge(y ~ X, lambda = lambda)
    beta <- setNames(as.numeric(rr$coef), colnames(X))
    yhat <- as.numeric(X %*% beta)
    alpha <- mean(y - yhat)
    yhat <- alpha + yhat
    list(alpha = alpha, beta = beta, yhat = yhat)
}

for (j in seq_along(assets)) {
    y <- R_tr[, j]
    Zdf <- as.data.frame(Z_tr)
    fit <- try(ols_fit(y, Zdf), silent = TRUE)
    if (inherits(fit, "try-error")) 
        fit <- ridge_fit(y, Zdf, lambda = 1e-04)
    beta <- fit$beta
    names(beta) <- gsub("`", "", names(beta))
    common <- intersect(fac_names, names(beta))
    if (length(common) > 0) 
        B_mat[common, j] <- as.numeric(beta[common])
    A_vec[j] <- fit$alpha
    S_eps[j] <- sd(y - fit$yhat)
}

cat("B_mat 维度: ", paste(dim(B_mat), collapse = " x "), "\n")

stopifnot(nrow(B_mat) == k, ncol(B_mat) == N, all(rownames(B_mat) == fac_names), all(colnames(B_mat) == assets))

w <- rep(1/length(assets), length(assets))

names(w) <- assets

b_port <- as.numeric(B_mat %*% w)

if (use_ewma) {
    wts <- lambda_ewma^(rev(seq_len(nrow(Z_tr))) - 1)
    wts <- wts/sum(wts)
    Sigma_F <- cov.wt(Z_tr, wt = wts, center = rep(0, ncol(Z_tr)))$cov
} else {
    Sigma_F <- cov(Z_tr)
}

Sigma_eps <- diag(S_eps^2, nrow = N)

sigma_idio2 <- as.numeric(t(w) %*% Sigma_eps %*% w)

Rte_mat <- as.matrix(R_te)

Zte_mat <- as.matrix(Z_te)

pnl_true_te <- as.numeric(Rte_mat %*% w)

sigma_factor <- sqrt(t(b_port) %*% Sigma_F %*% b_port)

sigma_model <- sqrt(sigma_factor^2 + sigma_idio2)

sigma_test_realized <- sd(pnl_true_te)

sigma_target <- sd(as.numeric(as.matrix(R_tr) %*% w))

print(c(sigma_factor = sigma_factor, sigma_idio = sqrt(sigma_idio2), sigma_model = sigma_model, sigma_test_realized = sigma_test_realized))

VaR_hist <- -quantile(pnl_true_te, probs = 1 - alpha, na.rm = TRUE)

ES_hist <- -mean(pnl_true_te[pnl_true_te <= quantile(pnl_true_te, probs = 1 - alpha, na.rm = TRUE)])

cat(sprintf("历史法 (真实组合)：VaR@%.0f%%=%.4f, ES@%.0f%%=%.4f\n", alpha * 100, VaR_hist, alpha * 100, 
    ES_hist))

mc_sims <- 10000

F_draw <- MASS::mvrnorm(n = mc_sims, mu = rep(0, k), Sigma = Sigma_F)

raw_mc <- as.numeric(F_draw %*% b_port) + rnorm(mc_sims, 0, sqrt(sigma_idio2))

pnl_mc <- if (vol_target) as.numeric(sigma_target/sigma_model) * raw_mc else raw_mc

VaR_mc <- -quantile(pnl_mc, probs = 1 - alpha, na.rm = TRUE)

ES_mc <- -mean(pnl_mc[pnl_mc <= quantile(pnl_mc, probs = 1 - alpha, na.rm = TRUE)])

cat(sprintf("因子MC (模型)：   VaR@%.0f%%=%.4f, ES@%.0f%%=%.4f\n", alpha * 100, VaR_mc, alpha * 100, ES_mc))

boot_expo_safe <- function(R_tr, Z_tr, w, B = 200, alpha = 0.95, lambda_ridge = 1e-04) {
    stopifnot(nrow(R_tr) == nrow(Z_tr))
    if (is.null(colnames(Z_tr)) || anyDuplicated(colnames(Z_tr)) > 0) 
        colnames(Z_tr) <- paste0("F", seq_len(ncol(Z_tr)))
    fac_names <- colnames(Z_tr)
    assets <- colnames(R_tr)
    k <- length(fac_names)
    N <- length(assets)
    VaR_b <- numeric(B)
    ES_b <- numeric(B)
    ols_fit <- function(y, Zdf) {
        fit <- lm(y ~ ., data = cbind(y = y, Zdf))
        cf <- coef(fit)
        alpha <- unname(cf[1])
        beta <- cf[-1]
        names(beta) <- gsub("`", "", names(beta))
        yhat <- as.numeric(alpha + as.matrix(Zdf) %*% beta)
        list(alpha = alpha, beta = beta, yhat = yhat)
    }
    ridge_fit <- function(y, Zdf, lambda = 1e-04) {
        if (!requireNamespace("MASS", quietly = TRUE)) 
            stop("此手动示例缺少依赖；请先参照章节README自行安装。", call. = FALSE)
        X <- as.matrix(Zdf)
        rr <- MASS::lm.ridge(y ~ X, lambda = lambda)
        beta <- setNames(as.numeric(rr$coef), colnames(X))
        yhat <- as.numeric(X %*% beta)
        alpha <- mean(y - yhat)
        yhat <- alpha + yhat
        list(alpha = alpha, beta = beta, yhat = yhat)
    }
    for (b in seq_len(B)) {
        idx <- sample(nrow(R_tr), nrow(R_tr), replace = TRUE)
        Rb <- R_tr[idx, , drop = FALSE]
        Zb <- Z_tr[idx, , drop = FALSE]
        colnames(Zb) <- fac_names
        Bmat_b <- matrix(0, nrow = k, ncol = N, dimnames = list(fac_names, assets))
        Sb2 <- numeric(N)
        for (j in seq_len(N)) {
            y <- Rb[, j]
            Zdf <- as.data.frame(Zb)
            fit <- try(ols_fit(y, Zdf), silent = TRUE)
            if (inherits(fit, "try-error")) 
                fit <- ridge_fit(y, Zdf, lambda = lambda_ridge)
            beta <- fit$beta
            names(beta) <- gsub("`", "", names(beta))
            common <- intersect(fac_names, names(beta))
            if (length(common) > 0) 
                Bmat_b[common, j] <- as.numeric(beta[common])
            Sb2[j] <- stats::sd(y - fit$yhat)^2
        }
        b_port_b <- as.numeric(Bmat_b %*% w)
        mcs <- 5000L
        SigmaF_b <- cov(Zb)
        F_draw <- MASS::mvrnorm(mcs, mu = rep(0, k), Sigma = SigmaF_b)
        eps_sd <- sqrt(as.numeric(t(w) %*% diag(Sb2, nrow = N) %*% w))
        pnl_b <- as.numeric(F_draw %*% b_port_b) + stats::rnorm(mcs, 0, eps_sd)
        VaR_b[b] <- -stats::quantile(pnl_b, probs = 1 - alpha, na.rm = TRUE)
        ES_b[b] <- -mean(pnl_b[pnl_b <= stats::quantile(pnl_b, probs = 1 - alpha, na.rm = TRUE)])
    }
    list(VaR = VaR_b, ES = ES_b)
}

boot_res <- boot_expo_safe(R_tr, Z_tr, w, B = B_boot, alpha = alpha)

VaR_ci <- quantile(boot_res$VaR, probs = c(0.05, 0.5, 0.95))

ES_ci <- quantile(boot_res$ES, probs = c(0.05, 0.5, 0.95))

cat(sprintf("自助 VaR 区间 [5%%,50%%,95%%]：%s\n", paste(round(VaR_ci, 4), collapse = " / ")))

cat(sprintf("自助 ES  区间 [5%%,50%%,95%%]：%s\n", paste(round(ES_ci, 4), collapse = " / ")))

df_fac <- tidyr::pivot_longer(data.frame(date = index(R_te), Z_te), -date, names_to = "Factor", values_to = "Value")

g1 <- ggplot(df_fac, aes(date, Value, color = Factor)) + geom_line() + theme_minimal() + labs(title = "测试期因子时间序列", 
    x = NULL, y = "Factor")

df_exp <- as.data.frame(t(B_mat))

df_exp$Asset <- rownames(df_exp)

df_exp <- tidyr::pivot_longer(df_exp, -Asset, names_to = "Factor", values_to = "Beta")

g2 <- ggplot(df_exp, aes(Factor, Asset, fill = Beta)) + geom_tile() + scale_fill_gradient2(low = "steelblue", high = "firebrick", 
    mid = "white") + theme_minimal() + labs(title = "资产对因子暴露（训练期 OLS）", x = NULL, y = NULL)

df_risk <- data.frame(Method = c("Boot-VaR-50%", "Hist-VaR", "MC-VaR"), Value = c(VaR_ci[2], VaR_hist, VaR_mc))

g3 <- ggplot(df_risk, aes(Method, Value, fill = Method)) + geom_col(width = 0.6) + theme_minimal() + labs(title = sprintf("组合 VaR@%.0f%% 对比", 
    alpha * 100), y = "VaR")

print(g1)

print(g2)

print(g3)

cat("\n=== 小结 ===\n")

cat("* PCA/AE 将高维收益压缩为少数因子；OLS 回归得到资产暴露，因子协方差由训练期估计（本例默认 EWMA）。\n")

cat("* 历史法 VaR/ES 基于测试期真实收益；因子 MC VaR/ES 使用 Σ_F 高斯抽样，并可做波动目标化以匹配当前状态。\n")

cat("* 自助法给出 VaR/ES 的估计不确定性分位区间；这不等同于深度贝叶斯后验。\n")
