# 正文来源：CH12-金融计量经济学与人工智能方法.tex，代码块 4；正文第 772 行。
# 只提取章末习题之前的正文代码；原控制台输出未纳入。
# 手动示例：可能依赖前序代码、外部文件、额外R包；参见本章README与manual/index.csv。
# 已移除自动安装、清空工作空间、保存整个工作空间及本机工作目录切换。
dir.create("results/manual", recursive=TRUE, showWarnings=FALSE)
if (!identical(Sys.getenv('FIN_ECON_ENABLE_NETWORK'), '1')) stop('此正文案例会联网；确认数据口径后设置 FIN_ECON_ENABLE_NETWORK=1 再手动运行。', call.=FALSE)
start_date <- as.Date("2010-01-01")

end_date <- as.Date("2024-12-31")

pkgs <- c("quantmod", "TTR", "dplyr", "lubridate", "pROC", "ggplot2", "tidyr", "zoo", "xgboost", "Matrix")

for (p in pkgs) if (!requireNamespace(p, quietly = TRUE)) stop("此手动示例缺少依赖；请先参照章节README自行安装。", 
    call. = FALSE)

invisible(lapply(pkgs, library, character.only = TRUE))

options(stringsAsFactors = FALSE, scipen = 99, timeout = max(300, getOption("timeout")))

set.seed(2025)

symbols <- c("SPY", "^GSPC")

px <- NULL

for (sym in symbols) {
    message("尝试下载: ", sym)
    ok <- try({
        x <- suppressWarnings(getSymbols(sym, src = "yahoo", from = start_date - 400, to = end_date, auto.assign = FALSE))
        if (!is.null(x)) {
            px <- x
            attr(px, "symbol") <- sym
        }
    }, silent = TRUE)
    if (!is.null(px)) 
        break
}

if (is.null(px)) stop("数据下载失败（检查网络或代理）。")

adj <- Ad(px)

adj_m <- to.monthly(adj, indexAt = "lastof", OHLC = FALSE)

adj_m <- adj_m[paste0(format(start_date, "%Y-%m"), "/", format(end_date, "%Y-%m"))]

ret_m <- diff(log(adj_m))

mom6 <- log(adj_m/lag(adj_m, 6))

mom12 <- log(adj_m/lag(adj_m, 12))

vol3 <- runSD(ret_m, n = 3)

sma10 <- SMA(adj_m, n = 10)

sma_ratio <- adj_m/sma10 - 1

rsi14 <- RSI(adj_m, n = 14)

macd <- MACD(adj_m, nFast = 12, nSlow = 26, nSig = 9)

macd_m <- macd[, "macd"]

macd_h <- macd_m - macd[, "signal"]

df <- tidyr::drop_na(select(mutate(arrange(tibble(date = as.Date(index(adj_m)), price = as.numeric(adj_m), r1 = as.numeric(ret_m), 
    mom6 = as.numeric(mom6), mom12 = as.numeric(mom12), vol3 = as.numeric(vol3), sma_ratio = as.numeric(sma_ratio), 
    rsi14 = as.numeric(rsi14), macd = as.numeric(macd_m), macd_hist = as.numeric(macd_h)), date), ret_fwd = dplyr::lead(r1, 
    1), y_fac = factor(ifelse(ret_fwd > 0, "Up", "Down"), levels = c("Down", "Up")), y_bin = as.numeric(y_fac == 
    "Up"), across(c(mom6, mom12, vol3, sma_ratio, rsi14, macd, macd_hist), ~dplyr::lag(.x, 1), .names = "{.col}_lag")), 
    date, y_fac, y_bin, ret_fwd, ends_with("_lag")))

len <- nrow(df)

min_train <- 24

min_oos <- 6

if (len < (min_train + min_oos)) {
    stop(sprintf("有效月数不足：当前%d，至少需要%d（训练≥%d + 样本外≥%d）。", len, min_train + 
        min_oos, min_train, min_oos))
}

train_win <- min(120, max(min_train, len - min_oos))

oos_months <- len - train_win

message(sprintf("可用月数=%d；训练窗=%d；样本外=%d。", len, train_win, oos_months))

features <- names(df)[grepl("_lag$", names(df))]

oos <- list()

last_model <- NULL

for (i in (train_win + 1):nrow(df)) {
    train_df <- df[(i - train_win):(i - 1), ]
    test_df <- df[i, ]
    val_win <- min(max(6, round(0.2 * nrow(train_df))), max(6, nrow(train_df) - 12))
    core_idx <- 1:(nrow(train_df) - val_win)
    val_idx <- (nrow(train_df) - val_win + 1):nrow(train_df)
    X_core <- as.matrix(train_df[core_idx, features])
    y_core <- train_df$y_bin[core_idx]
    X_val <- as.matrix(train_df[val_idx, features])
    y_val <- train_df$y_bin[val_idx]
    X_test <- as.matrix(test_df[, features])
    dtrain <- xgb.DMatrix(data = X_core, label = y_core, missing = NA)
    dval <- xgb.DMatrix(data = X_val, label = y_val, missing = NA)
    dtest <- xgb.DMatrix(data = X_test, missing = NA)
    pos <- max(1L, sum(y_core == 1))
    neg <- max(1L, sum(y_core == 0))
    spw <- as.numeric(neg/pos)
    params <- list(objective = "binary:logistic", eval_metric = "auc", eta = 0.05, max_depth = 3, min_child_weight = 5, 
        subsample = 0.8, colsample_bytree = 0.8, lambda = 1, alpha = 0, gamma = 0, scale_pos_weight = spw, nthread = 1)
    watch <- list(train = dtrain, val = dval)
    bst <- xgb.train(params = params, data = dtrain, nrounds = 2000, watchlist = watch, early_stopping_rounds = 50, 
        verbose = 0)
    last_model <- bst
    p_xgb <- as.numeric(predict(bst, dtest, ntreelimit = bst$best_ntreelimit))
    oos[[length(oos) + 1]] <- tibble(date = test_df$date, ret_fwd = test_df$ret_fwd, p_xgb = p_xgb)
}

oos <- arrange(bind_rows(oos), date)

resp <- factor(ifelse(oos$ret_fwd > 0, "Up", "Down"), levels = c("Down", "Up"))

auc_xgb <- as.numeric(pROC::auc(pROC::roc(resp, oos$p_xgb, quiet = TRUE)))

pred_xgb <- ifelse(oos$p_xgb > 0.5, "Up", "Down")

acc_xgb <- mean(pred_xgb == as.character(resp), na.rm = TRUE)

tc <- 5e-04

sig_xgb <- ifelse(oos$p_xgb > 0.5, 1, 0)

turn_xgb <- c(0, abs(diff(sig_xgb)))

ret_xgb <- sig_xgb * oos$ret_fwd - turn_xgb * tc

ret_bh <- oos$ret_fwd

nav <- tidyr::pivot_longer(tibble(date = oos$date, XGB = cumprod(1 + ret_xgb), BH = cumprod(1 + ret_bh)), -date, 
    names_to = "Strategy", values_to = "NAV")

ann_factor <- 12

stat <- mutate(tibble(Strategy = c("XGB", "BH"), AnnRet = c(prod(1 + ret_xgb)^(ann_factor/length(ret_xgb)) - 1, 
    prod(1 + ret_bh)^(ann_factor/length(ret_bh)) - 1), AnnVol = c(sd(ret_xgb, na.rm = TRUE) * sqrt(ann_factor), 
    sd(ret_bh, na.rm = TRUE) * sqrt(ann_factor))), Sharpe = AnnRet/AnnVol)

stat_rounded <- dplyr::mutate(stat, dplyr::across(where(is.numeric), ~round(.x, 3)))

cat("\n===== 固定区间：", format(start_date), " 至 ", format(end_date), "；标的：", attr(px, "symbol"), 
    " =====\n", sep = "")

cat(sprintf("AUC_XGB = %.3f,  ACC_XGB = %.3f\n\n", auc_xgb, acc_xgb))

cat("年化统计（AnnRet / AnnVol / Sharpe）：\n")

print(stat_rounded)

p1 <- ggplot(nav, aes(date, NAV, color = Strategy)) + geom_line(linewidth = 0.9) + labs(title = paste0("XGBoost 择时 vs 买入并持有（", 
    attr(px, "symbol"), "，月频，滚动样本外；", format(start_date), "—", format(end_date), "）"), 
    x = NULL, y = "净值（起点=1）") + theme_minimal() + theme(legend.position = "bottom")

print(p1)

imp <- xgb.importance(model = last_model, feature_names = features)

imp_df <- as.data.frame(imp, stringsAsFactors = FALSE)

ord <- order(-imp_df$Gain)

top_n <- min(10L, nrow(imp_df))

imp_top <- imp_df[ord[seq_len(top_n)], , drop = FALSE]

print(imp_top)

p2 <- ggplot(imp_top, aes(x = reorder(Feature, Gain), y = Gain)) + geom_col() + coord_flip() + labs(title = "XGBoost 特征重要度（Gain，最后一轮模型）", 
    x = NULL, y = "Gain") + theme_minimal()

print(p2)
