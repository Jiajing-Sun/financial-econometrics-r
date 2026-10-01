# 正文来源：CH7-非参数方法.tex，代码块 4；正文第 602 行。
# 只提取章末习题之前的正文代码；原控制台输出未纳入。
# 手动示例：可能依赖前序代码、外部文件、额外R包；参见本章README与manual/index.csv。
# 已移除自动安装、清空工作空间、保存整个工作空间及本机工作目录切换。
dir.create("results/manual", recursive=TRUE, showWarnings=FALSE)
library(datasets)

library(ks)

data("EuStockMarkets")

ftse_prices <- EuStockMarkets[, "FTSE"]

dax_prices <- EuStockMarkets[, "DAX"]

ftse_returns <- diff(ftse_prices)/lag(ftse_prices, -1) * 100

dax_returns <- diff(dax_prices)/lag(dax_prices, -1) * 100

ftse_returns <- na.omit(ftse_returns)

dax_returns <- na.omit(dax_returns)

market_returns <- data.frame(FTSE_Returns = ftse_returns, DAX_Returns = dax_returns)

data_matrix <- as.matrix(market_returns)

kde_result <- kde(x = data_matrix)

plot(kde_result, display = "filled.contour")

plot(kde_result, display = "persp")
