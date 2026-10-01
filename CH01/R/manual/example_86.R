# 正文来源：CH1-R语言概述.tex，代码块 86；正文第 2355 行。
# 只提取章末习题之前的正文代码；原控制台输出未纳入。
# 手动示例：可能依赖前序代码、外部文件、额外R包；参见本章README与manual/index.csv。
# 已移除自动安装、清空工作空间、保存整个工作空间及本机工作目录切换。
dir.create("results/manual", recursive=TRUE, showWarnings=FALSE)
if (!identical(Sys.getenv('FIN_ECON_ENABLE_NETWORK'), '1')) stop('此正文案例会联网；确认数据口径后设置 FIN_ECON_ENABLE_NETWORK=1 再手动运行。', call.=FALSE)
library(WDI)

library(dplyr)

library(ggplot2)

head(WDIsearch("gdp per capita"))

wb <- WDI(country = c("CN", "GB", "US"), indicator = c(gdppc = "NY.GDP.PCAP.KD", pop = "SP.POP.TOTL"), start = 1990, 
    end = 2023, extra = TRUE)

wb <- arrange(as_tibble(wb), country, year)

ggplot(wb, aes(x = year, y = gdppc, color = country)) + geom_line() + labs(title = "人均GDP（不变价美元，2015基年）", 
    x = "年份", y = "人均GDP（常量美元）", color = "国家")
