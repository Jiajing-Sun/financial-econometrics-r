# 正文来源：CH7-非参数方法.tex，代码块 3；正文第 235 行。
# 只提取章末习题之前的正文代码；原控制台输出未纳入。
# 手动示例：可能依赖前序代码、外部文件、额外R包；参见本章README与manual/index.csv。
# 已移除自动安装、清空工作空间、保存整个工作空间及本机工作目录切换。
dir.create("results/manual", recursive=TRUE, showWarnings=FALSE)
library(datasets)

data("EuStockMarkets")

dax_prices <- EuStockMarkets[, "DAX"]

dax_returns <- diff(dax_prices)/lag(dax_prices, -1) * 100

dax_returns <- na.omit(dax_returns)

dax_returns_df <- data.frame(DAX_Returns = dax_returns)

library(ggplot2)

ggplot(dax_returns_df, aes(x = DAX_Returns)) + geom_histogram(aes(y = ..density..), binwidth = 0.5, color = "black", 
    fill = "blue") + geom_density(alpha = 0.2, fill = "#FF6666") + labs(title = "Density and Histogram of DAX Index Daily Returns", 
    x = "Daily Returns (%) ", y = "Density")
