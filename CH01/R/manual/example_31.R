# 正文来源：CH1-R语言概述.tex，代码块 31；修订稿第 824 行。
# 仅提取章末习题之前的正文；不含习题提示或答案。
# 语法已检查；未宣称全部外部数据与可选分支已执行。
library(VIM)

data_set <- data.frame(var1 = c(1, 2, NA, 4, 5), var2 = c(6, NA, 8, 9, 10))

imputed_data <- kNN(data_set, k = 1)

cat("k-近邻插补后的数据（kNN 可能会附加标记列 .imp/.na）：\n")

print(imputed_data)
