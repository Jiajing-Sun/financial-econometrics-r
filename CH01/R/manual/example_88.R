# 正文来源：CH1-R语言概述.tex，代码块 88；修订稿第 2464 行。
# 仅提取章末习题之前的正文；不含习题提示或答案。
# 语法已检查；未宣称全部外部数据与可选分支已执行。
if (!identical(Sys.getenv('FIN_ECON_ENABLE_NETWORK'),'1')) stop('此正文示例可能联网；请设置 FIN_ECON_ENABLE_NETWORK=1 后手动运行。')
library(fredr)

fredr_set_key(Sys.getenv("FRED_API_KEY"))

cpi <- fredr(series_id = "CPIAUCSL", observation_start = as.Date("2000-01-01"))
