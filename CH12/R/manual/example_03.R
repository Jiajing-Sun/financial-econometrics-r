# 正文来源：CH12-金融计量经济学与人工智能方法.tex，代码块 3；修订稿第 530 行。
# 仅提取章末习题之前的正文；不含习题提示或答案。
# 语法已检查；未宣称全部外部数据与可选分支已执行。
if (!identical(Sys.getenv('FIN_ECON_ENABLE_NETWORK'),'1')) stop('此正文示例可能联网；请设置 FIN_ECON_ENABLE_NETWORK=1 后手动运行。')
start_date <- as.Date("2010-01-01")

end_date <- as.Date("2024-12-31")

pkgs <- c("quantmod", "TTR", "dplyr", "lubridate", "randomForest", "pROC", "ggplot2", "tidyr",
    "zoo")

for (p in pkgs) if (!requireNamespace(p, quietly = TRUE)) NULL

invisible(lapply(pkgs, library, character.only = TRUE))

set.seed(2025)

symbols <- c("SPY", "^GSPC")

px <- NULL

for (sym in symbols) {
    message("尝试下载: ", sym)
    ok <- try({
        x <- suppressWarnings(getSymbols(sym, src = "yahoo", from = start_date - 400, to = end_date,
            auto.assign = FALSE))
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

df <- tidyr::drop_na(select(mutate(arrange(tibble(date = as.Date(index(adj_m)), price = as.numeric(adj_m),
    r1 = as.numeric(ret_m), mom6 = as.numeric(mom6), mom12 = as.numeric(mom12), vol3 = as.numeric(vol3),
    sma_ratio = as.numeric(sma_ratio), rsi14 = as.numeric(rsi14), macd = as.numeric(macd_m),
    macd_hist = as.numeric(macd_h)), date), ret_fwd = dplyr::lead(r1, 1), y = factor(ifelse(ret_fwd >
    0, "Up", "Down"), levels = c("Down", "Up")), across(c(mom6, mom12, vol3, sma_ratio, rsi14,
    macd, macd_hist), ~dplyr::lag(.x, 1), .names = "{.col}_lag")), date, y, ret_fwd, ends_with("_lag")))

len <- nrow(df)

min_train <- 18

min_oos <- 6

if (len < (min_train + min_oos)) {
    stop(sprintf("有效月数不足：当前%d，至少需要%d（最小训练%d + 最小样本外%d）。请适当延长区间。",
        len, min_train + min_oos, min_train, min_oos))
}

train_win <- min(120, max(min_train, len - min_oos))

oos_months <- len - train_win

message(sprintf("可用月数=%d；训练窗=%d；样本外=%d。", len, train_win, oos_months))

features <- names(df)[grepl("_lag$", names(df))]

oos <- list()

for (i in (train_win + 1):nrow(df)) {
    train <- df[(i - train_win):(i - 1), ]
    test <- df[i, ]
    fit_glm <- glm(reformulate(termlabels = features, response = "y"), data = train, family = binomial())
    p_glm <- as.numeric(predict(fit_glm, newdata = test, type = "response"))
    fit_rf <- randomForest(x = train[, features], y = train$y, ntree = 500, mtry = max(1, floor(sqrt(length(features)))),
        nodesize = 5, importance = TRUE)
    p_rf <- as.numeric(predict(fit_rf, newdata = test[, features, drop = FALSE], type = "prob")[,
        "Up"])
    oos[[length(oos) + 1]] <- tibble(date = test$date, ret_fwd = test$ret_fwd, p_glm = p_glm,
        p_rf = p_rf)
}

oos <- arrange(bind_rows(oos), date)

resp <- factor(ifelse(oos$ret_fwd > 0, "Up", "Down"), levels = c("Down", "Up"))

auc_glm <- as.numeric(pROC::auc(pROC::roc(resp, oos$p_glm, quiet = TRUE, direction = "<")))

auc_rf <- as.numeric(pROC::auc(pROC::roc(resp, oos$p_rf, quiet = TRUE, direction = "<")))

pred_glm <- ifelse(oos$p_glm > 0.5, "Up", "Down")

pred_rf <- ifelse(oos$p_rf > 0.5, "Up", "Down")

acc_glm <- mean(pred_glm == as.character(resp), na.rm = TRUE)

acc_rf <- mean(pred_rf == as.character(resp), na.rm = TRUE)

tc <- 5e-04

sig_glm <- ifelse(oos$p_glm > 0.5, 1, 0)

sig_rf <- ifelse(oos$p_rf > 0.5, 1, 0)

turn_glm <- c(0, abs(diff(sig_glm)))

turn_rf <- c(0, abs(diff(sig_rf)))

ret_glm <- sig_glm * oos$ret_fwd - turn_glm * tc

ret_rf <- sig_rf * oos$ret_fwd - turn_rf * tc

ret_bh <- oos$ret_fwd

nav <- tidyr::pivot_longer(tibble(date = oos$date, GLM = cumprod(1 + ret_glm), RF = cumprod(1 +
    ret_rf), BH = cumprod(1 + ret_bh)), -date, names_to = "Strategy", values_to = "NAV")

ann_factor <- 12

stat <- mutate(tibble(Strategy = c("GLM", "RF", "BH"), AnnRet = c(prod(1 + ret_glm)^(ann_factor/length(ret_glm)) -
    1, prod(1 + ret_rf)^(ann_factor/length(ret_rf)) - 1, prod(1 + ret_bh)^(ann_factor/length(ret_bh)) -
    1), AnnVol = c(sd(ret_glm, na.rm = TRUE) * sqrt(ann_factor), sd(ret_rf, na.rm = TRUE) *
    sqrt(ann_factor), sd(ret_bh, na.rm = TRUE) * sqrt(ann_factor))), Sharpe = AnnRet/AnnVol)

stat_rounded <- dplyr::mutate(stat, dplyr::across(where(is.numeric), ~round(.x, 3)))

cat("\n===== 固定区间：", format(start_date), " 至 ", format(end_date), "；标的：",
    attr(px, "symbol"), " =====\n", sep = "")

cat("样本外指标（AUC 与 ACC）：\n")

cat(sprintf("  AUC  (GLM, RF): %.3f, %.3f\n", auc_glm, auc_rf))

cat(sprintf("  ACC  (GLM, RF): %.3f, %.3f\n\n", acc_glm, acc_rf))

cat("年化统计（AnnRet / AnnVol / Sharpe）：\n")

print(stat_rounded)

ggplot(nav, aes(date, NAV, color = Strategy)) + geom_line(linewidth = 0.9) + labs(title = paste0("随机森林 vs 逻辑回归（",
    attr(px, "symbol"), "，月频，滚动样本外；", format(start_date), "—", format(end_date),
    "）"), x = NULL, y = "净值（起点=1）") + theme_minimal() + theme(legend.position = "bottom")
