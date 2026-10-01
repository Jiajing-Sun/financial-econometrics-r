# 正文来源：CH3-回归模型及其应用.tex，代码块 2；正文第 857 行。
# 只提取章末习题之前的正文代码；原控制台输出未纳入。
# 手动示例：可能依赖前序代码、外部文件、额外R包；参见本章README与manual/index.csv。
# 已移除自动安装、清空工作空间、保存整个工作空间及本机工作目录切换。
dir.create("results/manual", recursive=TRUE, showWarnings=FALSE)
library(ggplot2)

library(zoo)

stock_data <- read.csv("data/stock_data.csv")

head(stock_data)

stock_data$Date <- as.Date(stock_data$Date, format = "%Y-%m-%d")

stock_data$stock_price <- na.approx(stock_data$stock_price)

stock_data$market_index <- na.approx(stock_data$market_index)

stock_data$risk_free_rate <- na.approx(stock_data$risk_free_rate)

n <- length(stock_data$Date)

stock_data$stock_return <- c(NA, diff(log(stock_data$stock_price)))

stock_data$market_return <- c(NA, diff(log(stock_data$market_index)))

stock_data$risk_free_daily <- stock_data$risk_free_rate/100/365

stock_data$stock_excess_return <- stock_data$stock_return - stock_data$risk_free_daily

stock_data$market_excess_return <- stock_data$market_return - stock_data$risk_free_daily

stock_data <- stock_data[-1, ]

capm_model <- lm(stock_excess_return ~ market_excess_return, data = stock_data)

summary(capm_model)

p <- ggplot(stock_data, aes(x = market_excess_return, y = stock_excess_return)) + geom_point(alpha = 0.75, size = 1.8) + 
    geom_smooth(method = "lm", se = FALSE, color = "blue", linewidth = 0.8) + labs(x = "市场超额收益率", 
    y = "A公司股票超额收益率") + theme_minimal(base_size = 12)

print(p)

ggsave("results/manual/capm_stock_plot.png", plot = p, width = 6, height = 4, dpi = 300)
