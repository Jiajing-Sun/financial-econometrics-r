# 正文来源：CH7-非参数方法.tex，代码块 3；修订稿第 235 行。
# 仅提取章末习题之前的正文；不含习题提示或答案。
# 语法已检查；未宣称全部外部数据与可选分支已执行。
library(datasets)

data("EuStockMarkets")

dax_prices <- EuStockMarkets[, "DAX"]

dax_returns <- diff(dax_prices)/lag(dax_prices, -1) * 100

dax_returns <- na.omit(dax_returns)

dax_returns_df <- data.frame(DAX_Returns = dax_returns)

library(ggplot2)

ggplot(dax_returns_df, aes(x = DAX_Returns)) + geom_histogram(aes(y = ..density..), binwidth = 0.5,
    color = "black", fill = "blue") + geom_density(alpha = 0.2, fill = "#FF6666") + labs(title = "Density and Histogram of DAX Index Daily Returns",
    x = "Daily Returns (%) ", y = "Density")
