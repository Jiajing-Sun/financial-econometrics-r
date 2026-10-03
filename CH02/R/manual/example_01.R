# 正文来源：CH2-引言和背景.tex，代码块 1；修订稿第 326 行。
# 仅提取章末习题之前的正文；不含习题提示或答案。
# 语法已检查；未宣称全部外部数据与可选分支已执行。
if (!identical(Sys.getenv('FIN_ECON_ENABLE_NETWORK'),'1')) stop('此正文示例可能联网；请设置 FIN_ECON_ENABLE_NETWORK=1 后手动运行。')
if (!require("quantmod")) NULL

if (!require("ggplot2")) NULL

library(quantmod)

library(ggplot2)

symbols <- c("^GSPC", "^N225", "^FTSE", "^GDAXI")

stock_data <- lapply(symbols, function(sym) {
    getSymbols(sym, src = "yahoo", from = "2020-01-01", to = Sys.Date(), auto.assign = FALSE)
})

names(stock_data) <- c("S&P 500", "Nikkei 225", "FTSE 100", "DAX")

log_returns <- lapply(stock_data, function(x) {
    x <- na.omit(x)
    Delt(Cl(x), type = "log")
})

log_returns_df <- do.call(merge, log_returns)

names(log_returns_df) <- c("S&P 500", "Nikkei 225", "FTSE 100", "DAX")

log_returns_df <- data.frame(date = index(log_returns_df), coredata(log_returns_df))

log_returns_long <- na.omit(log_returns_long)

ggplot(log_returns_long, aes(x = date, y = `Log Return`, color = Index)) + geom_line() + theme_minimal() +
    labs(title = "Logarithmic Return of Major Stock Indices", x = "Date", y = "Logarithmic Return",
        color = "Stock Index") + theme(legend.position = "bottom")
