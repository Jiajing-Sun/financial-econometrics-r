# 正文来源：CH1-R语言概述.tex，代码块 88；正文第 2489 行。
# 只提取章末习题之前的正文代码；原控制台输出未纳入。
# 手动示例：可能依赖前序代码、外部文件、额外R包；参见本章README与manual/index.csv。
# 已移除自动安装、清空工作空间、保存整个工作空间及本机工作目录切换。
dir.create("results/manual", recursive=TRUE, showWarnings=FALSE)
if (!identical(Sys.getenv('FIN_ECON_ENABLE_NETWORK'), '1')) stop('此正文案例会联网；确认数据口径后设置 FIN_ECON_ENABLE_NETWORK=1 再手动运行。', call.=FALSE)
library(fredr)

fredr_set_key(Sys.getenv("FRED_API_KEY"))

cpi <- fredr(series_id = "CPIAUCSL", observation_start = as.Date("2000-01-01"))
