# 正文来源：CH2-引言和背景.tex，代码块 2；修订稿第 471 行。
# 仅提取章末习题之前的正文；不含习题提示或答案。
# 语法已检查；未宣称全部外部数据与可选分支已执行。
if (!identical(Sys.getenv('FIN_ECON_ENABLE_NETWORK'),'1')) stop('此正文示例可能联网；请设置 FIN_ECON_ENABLE_NETWORK=1 后手动运行。')
library(quantmod)

start_date <- "2023-01-01"

end_date <- "2023-12-31"

getSymbols("000001.SS", src = "yahoo", from = start_date, to = end_date)

returns <- dailyReturn(Cl(get("000001.SS")))

r_bar <- mean(returns, na.rm = TRUE)

s_squared <- mean((returns - r_bar)^2, na.rm = TRUE)

T <- length(na.omit(returns))

kappa_3 <- sum((returns - r_bar)^3, na.rm = TRUE)/(T * s_squared^(3/2))

kappa_4 <- sum((returns - r_bar)^4, na.rm = TRUE)/(T * s_squared^2)

cat("平均收益率: ", r_bar, "\n")

cat("方差: ", s_squared, "\n")

cat("偏度: ", kappa_3, "\n")

cat("峰度: ", kappa_4, "\n")
