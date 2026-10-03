# 正文来源：CH12-金融计量经济学与人工智能方法.tex，代码块 6；修订稿第 1447 行。
# 仅提取章末习题之前的正文；不含习题提示或答案。
# 语法已检查；未宣称全部外部数据与可选分支已执行。
if (!identical(Sys.getenv('FIN_ECON_ENABLE_NETWORK'),'1')) stop('此正文示例可能联网；请设置 FIN_ECON_ENABLE_NETWORK=1 后手动运行。')
start_date <- as.Date("2010-01-01")

end_date <- as.Date("2024-12-31")

set.seed(2025)

run_lstm <- FALSE

use_attention <- FALSE

L_seq <- 20

train_win <- 800

val_frac <- 0.2

epochs <- 40

patience <- 8

retrain_every <- 30

keep_last <- 1200

n_out <- 600

refit_every <- 5

pkgs <- c("quantmod", "rugarch", "dplyr", "tidyr", "lubridate", "ggplot2", "zoo")

for (p in pkgs) if (!requireNamespace(p, quietly = TRUE)) NULL

invisible(lapply(pkgs, library, character.only = TRUE))

if (run_lstm) {
    if (!requireNamespace("keras3", quietly = TRUE))
        stop("请先安装并配置 keras3 及其计算后端。")
    library(keras3)
}

get_ok <- function(sym) {
    tryCatch({
        suppressWarnings(getSymbols(sym, src = "yahoo", from = start_date - 30, to = end_date,
            auto.assign = FALSE))
    }, error = function(e) NULL)
}

spy <- get_ok("SPY")

vix <- get_ok("^VIX")

if (is.null(spy) || is.null(vix)) stop("下载数据失败。")

idx <- sort(intersect(index(spy), index(vix)))

spy <- spy[idx]

vix <- vix[idx]

ret <- diff(log(Ad(spy)))

O <- Op(spy)

H <- Hi(spy)

L <- Lo(spy)

C <- Cl(spy)

GKvar <- 0.5 * (log(H/L))^2 - (2 * log(2) - 1) * (log(C/O))^2

GKvar <- GKvar[index(ret)]

GKvol <- sqrt(pmax(GKvar, 0))

vix_cl <- Cl(vix)[index(GKvol)]

vix_cl <- zoo::na.locf(vix_cl)

df <- drop_na(mutate(drop_na(tibble(date = as.Date(index(GKvol)), r = as.numeric(ret), abs_r = abs(as.numeric(ret)),
    r2 = as.numeric(ret)^2, GKvol = as.numeric(GKvol), VIX = as.numeric(vix_cl))), y_lin = dplyr::lead(GKvol,
    1), y_log = dplyr::lead(log(pmax(GKvol, 1e-08)), 1)))

df_q <- tail(df, keep_last)

n_q <- nrow(df_q)

spec_garch <- rugarch::ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,
    1)), mean.model = list(armaOrder = c(0, 0), include.mean = TRUE), distribution.model = "norm")

roll <- ugarchroll(spec = spec_garch, data = df_q$r, n.ahead = 1, forecast.length = n_out,
    refit.every = refit_every, refit.window = "moving", solver = "hybrid", solver.control = list(trace = 0),
    calculate.VaR = FALSE)

roll_df <- as.data.frame(roll)

pred_garch <- rep(NA_real_, n_q)

garch_idx <- (n_q - n_out + 1):n_q

pred_garch[garch_idx - 1L] <- log(pmax(roll_df$Sigma, 1e-08))

har_df <- drop_na(select(mutate(df_q, RVd = log(pmax(GKvol, 1e-08)), RVw_tmp = zoo::rollapply(GKvol,
    5, mean, align = "right", fill = NA), RVm_tmp = zoo::rollapply(GKvol, 22, mean, align = "right",
    fill = NA), RVw = log(pmax(RVw_tmp, 1e-08)), RVm = log(pmax(RVm_tmp, 1e-08))), date, y_log,
    RVd, RVw, RVm))

pred_har <- rep(NA_real_, nrow(har_df))

start_h <- 600

for (i in start_h:nrow(har_df)) {
    tr <- (i - 500):(i - 1)
    fit <- lm(y_log ~ RVd + RVw + RVm, data = har_df[tr, ])
    pred_har[i] <- as.numeric(predict(fit, newdata = har_df[i, ]))
}

scale_fit <- function(M) list(mu = colMeans(M), sd = pmax(apply(M, 2, sd), 1e-08))

scale_apply <- function(M, s) sweep(sweep(M, 2, s$mu, "-"), 2, s$sd, "/")

make_seq <- function(X, y, L) {
    n <- nrow(X)
    stopifnot(n >= L, length(y) == n)
    ends <- L:n
    a <- array(0, dim = c(length(ends), L, ncol(X)))
    for (j in seq_along(ends)) a[j, , ] <- X[(ends[j] - L + 1):ends[j], , drop = FALSE]
    list(X = a, y = y[ends])
}

build_lstm_model <- function(L, p, use_attention = FALSE) {
    inputs <- keras_input(shape = c(L, p))
    x <- layer_lstm(inputs, units = 16, return_sequences = TRUE)
    if (use_attention) {
        attn <- layer_multi_head_attention(num_heads = 2, key_dim = 8)
        x <- attn(query = x, value = x, key = x, use_causal_mask = TRUE)
    }
    x <- layer_lstm(x, units = 8)
    outputs <- layer_dense(x, units = 1)
    model <- keras_model(inputs, outputs)
    compile(model, optimizer = optimizer_adam(learning_rate = 0.001), loss = "mse")
    model
}

pred_lstm <- rep(NA_real_, n_q)

if (run_lstm) {
    set_random_seed(2026)
    feats <- c("GKvol", "abs_r", "r2", "VIX")
    start_i <- max(train_win + 1, L_seq + 1)
    stopifnot(start_i <= n_q)
    for (i in start_i:n_q) {
        retrain <- (i - start_i)%%retrain_every == 0
        if (retrain) {
            tr <- (i - train_win):(i - 1)
            nv <- max(50, round(length(tr) * val_frac))
            core <- head(tr, -nv)
            val <- tail(tr, nv)
            stat <- scale_fit(as.matrix(df_q[core, feats]))
            xc <- scale_apply(as.matrix(df_q[core, feats]), stat)
            xv <- scale_apply(as.matrix(df_q[val, feats]), stat)
            core_seq <- make_seq(xc, df_q$y_log[core], L_seq)
            xv <- rbind(tail(xc, L_seq - 1), xv)
            yv <- c(tail(df_q$y_log[core], L_seq - 1), df_q$y_log[val])
            val_seq <- make_seq(xv, yv, L_seq)
            model_lstm_q <- build_lstm_model(L_seq, ncol(xc), use_attention)
            fit(model_lstm_q, core_seq$X, core_seq$y, validation_data = list(val_seq$X, val_seq$y),
                epochs = epochs, batch_size = 32, verbose = 0, shuffle = FALSE, callbacks = list(callback_early_stopping(monitor = "val_loss",
                  patience = patience, restore_best_weights = TRUE)))
        }
        xt <- scale_apply(as.matrix(df_q[(i - L_seq + 1):i, feats]), stat)
        xt <- array(xt, dim = c(1, L_seq, ncol(xt)))
        pred_lstm[i] <- as.numeric(predict(model_lstm_q, xt, verbose = 0))
    }
}

oos_df <- drop_na(left_join(left_join(left_join(select(df_q, date, y_true = y_log), tibble(date = df_q$date,
    GARCH = pred_garch), by = "date"), tibble(date = har_df$date, HAR = pred_har), by = "date"),
    tibble(date = df_q$date, LSTM = pred_lstm), by = "date"), y_true)

mdl_cols <- c("GARCH", "HAR", "LSTM")

mdl_cols <- mdl_cols[colSums(!is.na(oos_df[mdl_cols])) > 0]

common <- complete.cases(oos_df[, c("y_true", mdl_cols)])

stopifnot(any(common))

oos_df <- oos_df[common, ]

metric <- function(y, yhat) {
    ok <- !is.na(y) & !is.na(yhat)
    if (!any(ok))
        return(c(RMSE = NA, Corr = NA))
    rmse <- sqrt(mean((y[ok] - yhat[ok])^2))
    corr <- suppressWarnings(cor(y[ok], yhat[ok]))
    c(RMSE = rmse, Corr = corr)
}

res_tab <- do.call(rbind, lapply(mdl_cols, function(m) metric(oos_df$y_true, oos_df[[m]])))

rownames(res_tab) <- mdl_cols

print(round(res_tab, 4))

plt_df <- dplyr::ungroup(tidyr::drop_na(dplyr::group_by(tidyr::pivot_longer(filter(oos_df,
    row_number() >= n() - 500), all_of(c("y_true", mdl_cols)), names_to = "Series", values_to = "Value"),
    Series), Value))

ggplot(plt_df, aes(date, Value, color = Series)) + geom_line(linewidth = 0.8) + labs(title = "日度 log-Vol：真实（GK）与模型预测（最近 500 天）",
    x = NULL, y = "log(Vol)") + theme_minimal() + theme(legend.position = "bottom")
