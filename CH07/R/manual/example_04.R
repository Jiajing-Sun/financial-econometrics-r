# 正文来源：CH7-非参数方法.tex，代码块 4；修订稿第 613 行。
# 仅提取章末习题之前的正文；不含习题提示或答案。
# 语法已检查；未宣称全部外部数据与可选分支已执行。
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
