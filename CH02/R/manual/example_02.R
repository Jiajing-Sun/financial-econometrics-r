# 正文来源：CH2-引言和背景.tex，代码块 2；正文第 534 行。
# 只提取章末习题之前的正文代码；原控制台输出未纳入。
# 手动示例：可能依赖前序代码、外部文件、额外R包；参见本章README与manual/index.csv。
# 已移除自动安装、清空工作空间、保存整个工作空间及本机工作目录切换。
dir.create("results/manual", recursive=TRUE, showWarnings=FALSE)
if (!identical(Sys.getenv('FIN_ECON_ENABLE_NETWORK'), '1')) stop('此正文案例会联网；确认数据口径后设置 FIN_ECON_ENABLE_NETWORK=1 再手动运行。', call.=FALSE)
library(quantmod)

start_date <- "2023-01-01"

end_date <- "2023-12-31"

getSymbols("000001.SS", src = "yahoo", from = start_date, to = end_date)

returns <- dailyReturn(Cl(get("000001.SS")))

r_bar <- mean(returns, na.rm = TRUE)

s_squared <- var(returns, na.rm = TRUE)

T <- length(na.omit(returns))

kappa_3 <- sum((returns - r_bar)^3, na.rm = TRUE)/(T * s_squared^(3/2))

kappa_4 <- sum((returns - r_bar)^4, na.rm = TRUE)/(T * s_squared^2)

cat("平均收益率: ", r_bar, "\n")

cat("方差: ", s_squared, "\n")

cat("偏度: ", kappa_3, "\n")

cat("峰度: ", kappa_4, "\n")
