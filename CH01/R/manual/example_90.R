# 正文来源：CH1-R语言概述.tex，代码块 90；修订稿第 2488 行。
# 仅提取章末习题之前的正文；不含习题提示或答案。
# 语法已检查；未宣称全部外部数据与可选分支已执行。
library(imfr)

df_imf <- imf_data(database_id = "IFS", indicator = "PCPI_IX", country = c("CN", "GB"), start = 2015,
    end = 2024)
