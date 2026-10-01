# 正文来源：CH8-金融资产定价模型.tex，代码块 5；正文第 858 行。
# 只提取章末习题之前的正文代码；原控制台输出未纳入。
# 手动示例：可能依赖前序代码、外部文件、额外R包；参见本章README与manual/index.csv。
# 已移除自动安装、清空工作空间、保存整个工作空间及本机工作目录切换。
dir.create("results/manual", recursive=TRUE, showWarnings=FALSE)
set.seed(123)

n_stocks <- 100

n_periods <- 24

roe <- matrix(runif(n_stocks * n_periods, min = -0.1, max = 0.1), nrow = n_stocks, dimnames = list(paste0("Stock_", 
    1:n_stocks), paste0("Period_", 1:n_periods)))

asset_growth <- matrix(runif(n_stocks * n_periods, min = 0, max = 0.5), nrow = n_stocks, dimnames = list(rownames(roe), 
    colnames(roe)))

returns <- matrix(runif(n_stocks * n_periods, min = -0.05, max = 0.05), nrow = n_stocks, dimnames = list(rownames(roe), 
    colnames(roe)))

rmw_values <- numeric(n_periods - 1)

cma_values <- numeric(n_periods - 1)

for (period in 2:n_periods) {
    rvec <- roe[, period]
    ord_roe <- order(rvec)
    k_low <- max(1, floor(0.3 * length(rvec)))
    k_high <- k_low
    weak_ids <- rownames(roe)[ord_roe[1:k_low]]
    robust_ids <- rownames(roe)[ord_roe[(length(rvec) - k_high + 1):length(rvec)]]
    rmw_values[period - 1] <- mean(returns[robust_ids, period], na.rm = TRUE) - mean(returns[weak_ids, period], 
        na.rm = TRUE)
    avec <- asset_growth[, period]
    ord_ag <- order(avec)
    conservative_ids <- rownames(asset_growth)[ord_ag[1:k_low]]
    aggressive_ids <- rownames(asset_growth)[ord_ag[(length(avec) - k_high + 1):length(avec)]]
    cma_values[period - 1] <- mean(returns[conservative_ids, period], na.rm = TRUE) - mean(returns[aggressive_ids, 
        period], na.rm = TRUE)
}

names(rmw_values) <- paste0("Period_", 2:n_periods)

names(cma_values) <- paste0("Period_", 2:n_periods)

cat("RMW因子（示例）：\n")

print(rmw_values)

cat("CMA因子（示例）：\n")

print(cma_values)
