# 正文来源：CH8-金融资产定价模型.tex，代码块 4；正文第 790 行。
# 只提取章末习题之前的正文代码；原控制台输出未纳入。
# 手动示例：可能依赖前序代码、外部文件、额外R包；参见本章README与manual/index.csv。
# 已移除自动安装、清空工作空间、保存整个工作空间及本机工作目录切换。
dir.create("results/manual", recursive=TRUE, showWarnings=FALSE)
set.seed(123)

prices <- matrix(runif(n_stocks * n_periods, 50, 150), nrow = n_stocks, dimnames = list(paste0("Stock_", 1:n_stocks), 
    paste0("Period_", 1:n_periods)))

returns <- apply(prices, 1, function(p) c(NA, diff(p)/head(p, -1)))

returns <- t(returns)

sorting_period <- 12

holding_period <- 1

sort_cols <- (n_periods - sorting_period + 1):n_periods

hold_col <- n_periods - holding_period + 1

sorting_returns <- rowSums(returns[, sort_cols, drop = FALSE], na.rm = TRUE)

k_top <- max(1, round(0.3 * n_stocks))

ord_sort <- order(sorting_returns, decreasing = TRUE)

winners <- rownames(returns)[ord_sort[1:k_top]]

losers <- rownames(returns)[ord_sort[(n_stocks - k_top + 1):n_stocks]]

winner_ret <- mean(returns[winners, hold_col], na.rm = TRUE)

loser_ret <- mean(returns[losers, hold_col], na.rm = TRUE)

UMD <- winner_ret - loser_ret

cat(sprintf("动量因子 UMD（示例）= %.6f\n", UMD))
