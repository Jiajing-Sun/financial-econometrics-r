# 正文来源：CH8-金融资产定价模型.tex，代码块 3；正文第 737 行。
# 只提取章末习题之前的正文代码；原控制台输出未纳入。
# 手动示例：可能依赖前序代码、外部文件、额外R包；参见本章README与manual/index.csv。
# 已移除自动安装、清空工作空间、保存整个工作空间及本机工作目录切换。
dir.create("results/manual", recursive=TRUE, showWarnings=FALSE)
set.seed(123)

n_stocks <- 100

n_periods <- 24

market_values <- matrix(runif(n_stocks * n_periods, 100, 1100), nrow = n_stocks)

pb_ratios <- matrix(runif(n_stocks * n_periods, 1, 6), nrow = n_stocks)

returns <- matrix(runif(n_stocks * n_periods, -0.05, 0.05), nrow = n_stocks)

rownames(market_values) <- rownames(pb_ratios) <- rownames(returns) <- paste0("Stock_", 1:n_stocks)

colnames(market_values) <- colnames(pb_ratios) <- colnames(returns) <- paste0("Period_", 1:n_periods)

smb_values <- numeric(n_periods - 1)

hml_values <- numeric(n_periods - 1)

for (period in 2:n_periods) {
    mv <- market_values[, period]
    ord_mv <- order(mv)
    k_half <- floor(length(mv)/2)
    small_ids <- rownames(market_values)[ord_mv[1:k_half]]
    big_ids <- rownames(market_values)[ord_mv[(length(mv) - k_half + 1):length(mv)]]
    small_ret <- mean(returns[small_ids, period], na.rm = TRUE)
    big_ret <- mean(returns[big_ids, period], na.rm = TRUE)
    smb_values[period - 1] <- small_ret - big_ret
    bm <- 1/pb_ratios[, period]
    ord_bm <- order(bm, decreasing = TRUE)
    k_top <- max(1, round(0.3 * length(bm)))
    high_ids <- rownames(pb_ratios)[ord_bm[1:k_top]]
    low_ids <- rownames(pb_ratios)[ord_bm[(length(bm) - k_top + 1):length(bm)]]
    high_ret <- mean(returns[high_ids, period], na.rm = TRUE)
    low_ret <- mean(returns[low_ids, period], na.rm = TRUE)
    hml_values[period - 1] <- high_ret - low_ret
}

names(smb_values) <- paste0("Period_", 2:n_periods)

names(hml_values) <- paste0("Period_", 2:n_periods)

cat("SMB因子（示例）:\n")

print(smb_values)

cat("HML因子（示例）:\n")

print(hml_values)
