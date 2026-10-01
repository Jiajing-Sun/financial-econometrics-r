# 正文来源：CH2-引言和背景.tex，代码块 1；正文第 385 行。
# 只提取章末习题之前的正文代码；原控制台输出未纳入。
# 手动示例：可能依赖前序代码、外部文件、额外R包；参见本章README与manual/index.csv。
# 已移除自动安装、清空工作空间、保存整个工作空间及本机工作目录切换。
dir.create("results/manual", recursive=TRUE, showWarnings=FALSE)
if (!identical(Sys.getenv('FIN_ECON_ENABLE_NETWORK'), '1')) stop('此正文案例会联网；确认数据口径后设置 FIN_ECON_ENABLE_NETWORK=1 再手动运行。', call.=FALSE)
if (!require("quantmod")) stop("此手动示例缺少依赖；请先参照章节README自行安装。", call. = FALSE)

if (!require("ggplot2")) stop("此手动示例缺少依赖；请先参照章节README自行安装。", call. = FALSE)

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

ggplot(log_returns_long, aes(x = date, y = `Log Return`, color = Index)) + geom_line() + theme_minimal() + labs(title = "Logarithmic Return of Major Stock Indices", 
    x = "Date", y = "Logarithmic Return", color = "Stock Index") + theme(legend.position = "bottom")
