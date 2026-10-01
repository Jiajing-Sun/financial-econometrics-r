# 正文来源：CH12-金融计量经济学与人工智能方法.tex，代码块 1；正文第 193 行。
# 只提取章末习题之前的正文代码；原控制台输出未纳入。
# 手动示例：可能依赖前序代码、外部文件、额外R包；参见本章README与manual/index.csv。
# 已移除自动安装、清空工作空间、保存整个工作空间及本机工作目录切换。
dir.create("results/manual", recursive=TRUE, showWarnings=FALSE)
library(dplyr)

library(tidyr)

library(ggplot2)

library(rpart)

library(rpart.plot)

library(pROC)

set.seed(123)

n_stocks <- 200

n_months <- 120

dates <- seq(as.Date("2010-01-31"), by = "month", length.out = n_months)

rate_series <- as.numeric(stats::arima.sim(list(ar = 0.8), n = n_months, sd = 0.2))

rate_series <- scale(rate_series)[, 1]

panel <- mutate(as_tibble(expand.grid(id = sprintf("S%03d", 1:n_stocks), date = dates)), rate = rate_series[match(date, 
    dates)], pe = pmax(rnorm(n(), mean = 15, sd = 5), 1), mom = rnorm(n()), vol = exp(rnorm(n(), sd = 0.3)))

panel <- mutate(panel, signal = -0.02 * pe + 0.5 * (-rate) + 0.3 * mom - 0.1 * vol + ifelse(rate < 0 & pe > 18, 
    -0.2, 0), ret_fwd = 0.02 * signal + rnorm(n(), sd = 0.05), y = factor(ifelse(ret_fwd > 0, "Up", "Down")))

uniq_dates <- sort(unique(panel$date))

train_win <- 60

q <- 0.2

tc <- 5e-04

oos_list <- list()

for (t in (train_win + 1):length(uniq_dates)) {
    trng <- uniq_dates[(t - train_win):(t - 1)]
    test <- uniq_dates[t]
    train_df <- filter(panel, date %in% trng)
    test_df <- filter(panel, date == test)
    fit <- rpart(y ~ pe + rate + mom + vol, data = train_df, method = "class", control = rpart.control(cp = 0, 
        maxdepth = 4, minsplit = 200, minbucket = 50))
    test_df$prob <- predict(fit, newdata = test_df, type = "prob")[, "Up"]
    roc_obj <- pROC::roc(response = test_df$y, predictor = test_df$prob, quiet = TRUE)
    auc_val <- as.numeric(pROC::auc(roc_obj))
    thr_long <- quantile(test_df$prob, 1 - q, na.rm = TRUE)
    thr_short <- quantile(test_df$prob, q, na.rm = TRUE)
    long_ret <- mean(test_df$ret_fwd[test_df$prob >= thr_long], na.rm = TRUE)
    short_ret <- mean(test_df$ret_fwd[test_df$prob <= thr_short], na.rm = TRUE)
    ls_ret <- (long_ret - short_ret) - 2 * tc
    oos_list[[length(oos_list) + 1]] <- tibble(date = test, auc = auc_val, long = long_ret, short = short_ret, 
        ls = ls_ret)
}

oos <- mutate(arrange(bind_rows(oos_list), date), nav = cumprod(1 + ls))

ann_factor <- 12

ann_ret <- prod(1 + oos$ls)^(ann_factor/nrow(oos)) - 1

ann_vol <- sd(oos$ls, na.rm = TRUE) * sqrt(ann_factor)

sharpe <- mean(oos$ls, na.rm = TRUE)/sd(oos$ls, na.rm = TRUE) * sqrt(ann_factor)

print(round(c(AnnReturn = ann_ret, AnnVol = ann_vol, Sharpe = sharpe), 3))

ggplot(oos, aes(date, nav)) + geom_line() + labs(title = "多空策略累计净值（含交易成本）", y = "净值", 
    x = NULL)

ggplot(oos, aes(date, auc)) + geom_line() + geom_hline(yintercept = 0.5, linetype = 2) + labs(title = "截面AUC（每月）", 
    y = "AUC", x = NULL)

rpart.plot::rpart.plot(fit, type = 2, extra = 104, under = TRUE, fallen.leaves = TRUE)
