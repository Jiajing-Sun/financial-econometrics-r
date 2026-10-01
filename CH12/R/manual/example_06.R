# 正文来源：CH12-金融计量经济学与人工智能方法.tex，代码块 6；正文第 1399 行。
# 只提取章末习题之前的正文代码；原控制台输出未纳入。
# 手动示例：可能依赖前序代码、外部文件、额外R包；参见本章README与manual/index.csv。
# 已移除自动安装、清空工作空间、保存整个工作空间及本机工作目录切换。
dir.create("results/manual", recursive=TRUE, showWarnings=FALSE)
if (!identical(Sys.getenv('FIN_ECON_ENABLE_NETWORK'), '1')) stop('此正文案例会联网；确认数据口径后设置 FIN_ECON_ENABLE_NETWORK=1 再手动运行。', call.=FALSE)
start_date <- as.Date("2010-01-01")

end_date <- as.Date("2024-12-31")

set.seed(2025)

run_lstm <- FALSE

run_hybrid <- FALSE

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

for (p in pkgs) if (!requireNamespace(p, quietly = TRUE)) stop("此手动示例缺少依赖；请先参照章节README自行安装。", 
    call. = FALSE)

invisible(lapply(pkgs, library, character.only = TRUE))

if (run_lstm || run_hybrid) {
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

GKvol <- sqrt(pmax(as.numeric(GKvar), 0))

vix_cl <- Cl(vix)[index(GKvol)]

vix_cl <- zoo::na.locf(vix_cl)

df <- drop_na(mutate(drop_na(tibble(date = as.Date(index(GKvol)), r = as.numeric(ret), abs_r = abs(as.numeric(ret)), 
    r2 = as.numeric(ret)^2, GKvol = as.numeric(GKvol), VIX = as.numeric(vix_cl))), y_lin = dplyr::lead(GKvol, 1), 
    y_log = dplyr::lead(log(pmax(GKvol, 1e-08)), 1)))

df_q <- tail(df, keep_last)

n_q <- nrow(df_q)

spec_garch <- rugarch::ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), mean.model = list(armaOrder = c(0, 
    0), include.mean = TRUE), distribution.model = "norm")

roll <- ugarchroll(spec = spec_garch, data = df_q$r, n.ahead = 1, forecast.length = n_out, refit.every = refit_every, 
    refit.window = "moving", solver = "hybrid", solver.control = list(trace = 0), calculate.VaR = FALSE)

roll_df <- as.data.frame(roll)

pred_garch <- rep(NA_real_, n_q)

garch_idx <- (n_q - n_out + 1):n_q

pred_garch[garch_idx] <- log(pmax(roll_df$Sigma, 1e-08))

har_df <- drop_na(select(mutate(df_q, RVd = log(pmax(GKvol, 1e-08)), RVw_tmp = zoo::rollapply(GKvol, 5, mean, align = "right", 
    fill = NA), RVm_tmp = zoo::rollapply(GKvol, 22, mean, align = "right", fill = NA), RVw = log(pmax(dplyr::lag(RVw_tmp), 
    1e-08)), RVm = log(pmax(dplyr::lag(RVm_tmp), 1e-08))), date, y_log, RVd, RVw, RVm))

pred_har <- rep(NA_real_, nrow(har_df))

start_h <- 600

for (i in start_h:(nrow(har_df) - 1)) {
    tr <- (i - 500):(i - 1)
    fit <- lm(y_log ~ RVd + RVw + RVm, data = har_df[tr, ])
    pred_har[i] <- as.numeric(predict(fit, newdata = har_df[i, ]))
}

has_mha <- run_lstm && "layer_multi_head_attention" %in% ls(getNamespace("keras"))

scale_fit <- function(M) list(mu = colMeans(M), sd = pmax(apply(M, 2, sd), 1e-08))

scale_apply <- function(M, stat) sweep(sweep(M, 2, stat$mu, "-"), 2, stat$sd, "/")

make_seq <- function(X, y, L) {
    n <- nrow(X)
    p <- ncol(X)
    if (n <= L) 
        return(NULL)
    Xs <- array(0, dim = c(n - L, L, p))
    ys <- y[(L + 1):n]
    for (k in 1:(n - L)) Xs[k, , ] <- X[k:(k + L - 1), ]
    list(X = Xs, y = ys)
}

build_lstm_model <- function(L, p, use_mha = FALSE) {
    inputs <- layer_input(shape = c(L, p))
    x <- layer_dropout(layer_lstm(inputs, units = 32, return_sequences = TRUE), 0.1)
    if (use_mha) {
        x <- layer_layer_normalization(layer_multi_head_attention(num_heads = 4, key_dim = 16, dropout = 0)(list(x, 
            x, x)))
    }
    x <- layer_dense(layer_dropout(layer_lstm(x, units = 16), 0.1), units = 1)
    model <- keras_model(inputs = inputs, outputs = x)
    compile(model, optimizer = optimizer_adam(learning_rate = 0.005), loss = "mse")
    model
}

pred_lstm <- rep(NA_real_, n_q)

if (run_lstm || run_hybrid) {
    base_feats <- c("GKvol", "abs_r", "r2", "VIX")
    need_train <- TRUE
    start_i <- max(train_win + 1, L_seq + 1)
    for (i in start_i:(n_q - 1)) {
        need_train <- need_train || ((i - start_i)%%retrain_every == 0)
        tr_idx <- (i - train_win):(i - 1)
        X_tr <- as.matrix(df_q[tr_idx, base_feats])
        y_tr <- df_q$y_log[tr_idx]
        if (run_hybrid) {
            fit_h <- try(ugarchfit(spec = spec_garch, data = df_q$r[tr_idx], solver = "hybrid", solver.control = list(trace = 0)), 
                silent = TRUE)
            if (!inherits(fit_h, "try-error")) {
                X_tr <- cbind(X_tr, GARCH = as.numeric(sigma(fit_h)))
            }
        }
        stat <- scale_fit(X_tr)
        Xs_tr <- scale_apply(X_tr, stat)
        n_tr <- nrow(Xs_tr)
        val_n <- max(50, round(n_tr * val_frac))
        core_n <- n_tr - val_n
        X_core <- Xs_tr[1:core_n, , drop = FALSE]
        y_core <- y_tr[1:core_n]
        X_val <- Xs_tr[(core_n + 1):n_tr, , drop = FALSE]
        y_val <- y_tr[(core_n + 1):n_tr]
        seq_core <- make_seq(X_core, y_core, L = L_seq)
        seq_val <- make_seq(rbind(X_core[(nrow(X_core) - L_seq + 1):nrow(X_core), , drop = FALSE], X_val), c(tail(y_core, 
            L_seq), y_val), L = L_seq)
        X_te <- as.matrix(df_q[(i - L_seq):(i - 1), base_feats])
        if (run_hybrid && exists("fit_h") && !inherits(fit_h, "try-error")) {
            gsig_te <- as.numeric(sigma(ugarchforecast(fit_h, n.ahead = 1)))
            X_te <- cbind(X_te, GARCH = c(tail(as.numeric(sigma(fit_h)), L_seq - 1), gsig_te))
        }
        X_te <- scale_apply(X_te, stat)
        X_te <- array(X_te, dim = c(1, L_seq, ncol(X_te)))
        if ((run_lstm || run_hybrid) && !is.null(seq_core) && !is.null(seq_val)) {
            if (need_train || !exists("model_lstm_q")) {
                model_lstm_q <- build_lstm_model(L_seq, ncol(Xs_tr), use_mha = (run_lstm && has_mha))
            }
            fit <- try(fit(model_lstm_q, x = seq_core$X, y = seq_core$y, validation_data = list(seq_val$X, seq_val$y), 
                epochs = epochs, batch_size = 32, verbose = 0, callbacks = list(callback_early_stopping(monitor = "val_loss", 
                  patience = patience, restore_best_weights = TRUE))), silent = TRUE)
            if (!inherits(fit, "try-error")) {
                pred_lstm[i] <- as.numeric(predict(model_lstm_q, X_te))
            }
            need_train <- FALSE
        }
    }
}

oos_df <- drop_na(left_join(left_join(left_join(select(df_q, date, y_true = y_log), tibble(date = df_q$date, GARCH = pred_garch), 
    by = "date"), tibble(date = har_df$date, HAR = pred_har), by = "date"), tibble(date = df_q$date, LSTM = pred_lstm), 
    by = "date"), y_true)

mdl_cols <- c("GARCH", "HAR", "LSTM")

mdl_cols <- mdl_cols[colSums(!is.na(oos_df[mdl_cols])) > 0]

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

plt_df <- dplyr::ungroup(tidyr::drop_na(dplyr::group_by(tidyr::pivot_longer(filter(oos_df, row_number() >= n() - 
    500), all_of(c("y_true", mdl_cols)), names_to = "Series", values_to = "Value"), Series), Value))

ggplot(plt_df, aes(date, Value, color = Series)) + geom_line(linewidth = 0.8) + labs(title = "日度 log-Vol：真实（GK）与模型预测（最近 500 天）", 
    x = NULL, y = "log(Vol)") + theme_minimal() + theme(legend.position = "bottom")
