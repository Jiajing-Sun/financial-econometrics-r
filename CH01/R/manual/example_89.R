# 正文来源：CH1-R语言概述.tex，代码块 89；修订稿第 2476 行。
# 仅提取章末习题之前的正文；不含习题提示或答案。
# 语法已检查；未宣称全部外部数据与可选分支已执行。
library(OECD)

meta <- get_datasets()

cli <- get_dataset("MEI_CLI", start_time = 2015)
