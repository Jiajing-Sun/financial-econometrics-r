# 正文来源：CH1-R语言概述.tex，代码块 51；修订稿第 1401 行。
# 仅提取章末习题之前的正文；不含习题提示或答案。
# 语法已检查；未宣称全部外部数据与可选分支已执行。
library(dplyr)

starwars %>% filter(species == "Human") %>% select(name, height, mass) %>% arrange(desc(height)) %>%
    head(5)
