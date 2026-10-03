# 正文来源：CH8-金融资产定价模型.tex，代码块 4；修订稿第 794 行。
# 仅提取章末习题之前的正文；不含习题提示或答案。
# 语法已检查；未宣称全部外部数据与可选分支已执行。
set.seed(123)

prices <- matrix(runif(n_stocks * n_periods, 50, 150), nrow = n_stocks, dimnames = list(paste0("Stock_",
    1:n_stocks), paste0("Period_", 1:n_periods)))

returns <- apply(prices, 1, function(p) c(NA, diff(p)/head(p, -1)))

returns <- t(returns)

sorting_period <- 12

holding_period <- 1

hold_col <- n_periods

sort_end <- hold_col - 2L

sort_cols <- (sort_end - sorting_period + 1L):sort_end

stopifnot(min(sort_cols) >= 2L, max(sort_cols) < hold_col)

sorting_returns <- apply(1 + returns[, sort_cols, drop = FALSE], 1, prod) - 1

k_top <- max(1, round(0.3 * n_stocks))

ord_sort <- order(sorting_returns, decreasing = TRUE)

winners <- rownames(returns)[ord_sort[1:k_top]]

losers <- rownames(returns)[ord_sort[(n_stocks - k_top + 1):n_stocks]]

winner_ret <- mean(returns[winners, hold_col], na.rm = TRUE)

loser_ret <- mean(returns[losers, hold_col], na.rm = TRUE)

UMD <- winner_ret - loser_ret

cat(sprintf("动量因子 UMD（示例）= %.6f\n", UMD))
