# 正文来源：CH3-回归模型及其应用.tex，代码块 2；修订稿第 543 行。
# 仅提取章末习题之前的正文；不含习题提示或答案。
# 语法已检查；未宣称全部外部数据与可选分支已执行。
library(ggplot2)

library(zoo)

stock_data <- read.csv("stock_data.csv")

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

p <- ggplot(stock_data, aes(x = market_excess_return, y = stock_excess_return)) + geom_point(alpha = 0.75,
    size = 1.8) + geom_smooth(method = "lm", se = FALSE, color = "blue", linewidth = 0.8) +
    labs(x = "市场超额收益率", y = "A公司股票超额收益率") + theme_minimal(base_size = 12)

print(p)

ggsave("../figures/capm_stock_plot.png", plot = p, width = 6, height = 4, dpi = 300)
