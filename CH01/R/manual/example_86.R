# 正文来源：CH1-R语言概述.tex，代码块 86；修订稿第 2328 行。
# 仅提取章末习题之前的正文；不含习题提示或答案。
# 语法已检查；未宣称全部外部数据与可选分支已执行。
if (!identical(Sys.getenv('FIN_ECON_ENABLE_NETWORK'),'1')) stop('此正文示例可能联网；请设置 FIN_ECON_ENABLE_NETWORK=1 后手动运行。')
library(WDI)

library(dplyr)

library(ggplot2)

head(WDIsearch("gdp per capita"))

wb <- WDI(country = c("CN", "GB", "US"), indicator = c(gdppc = "NY.GDP.PCAP.KD", pop = "SP.POP.TOTL"),
    start = 1990, end = 2023, extra = TRUE)

wb <- arrange(as_tibble(wb), country, year)

ggplot(wb, aes(x = year, y = gdppc, color = country)) + geom_line() + labs(title = "人均GDP（不变价美元，2015基年）",
    x = "年份", y = "人均GDP（常量美元）", color = "国家")
