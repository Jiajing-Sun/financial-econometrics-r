# 正文来源：CH1-R语言概述.tex，代码块 53；修订稿第 1433 行。
# 仅提取章末习题之前的正文；不含习题提示或答案。
# 语法已检查；未宣称全部外部数据与可选分支已执行。
starwars %>% arrange(desc(height))

starwars %>% select(name, height, mass) %>% mutate(height_m = height/100)

starwars %>% group_by(species, sex) %>% summarize(average_height = mean(height, na.rm = TRUE),
    average_mass = mean(mass, na.rm = TRUE))
