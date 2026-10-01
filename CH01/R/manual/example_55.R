# 正文来源：CH1-R语言概述.tex，代码块 55；正文第 1546 行。
# 只提取章末习题之前的正文代码；原控制台输出未纳入。
# 手动示例：可能依赖前序代码、外部文件、额外R包；参见本章README与manual/index.csv。
# 已移除自动安装、清空工作空间、保存整个工作空间及本机工作目录切换。
dir.create("results/manual", recursive=TRUE, showWarnings=FALSE)
library(tidyr)

n = 10

tidy_dataframe = data.frame(S.No = c(1:n), Group.1 = c(23, 345, 76, 212, 88, 199, 72, 35, 90, 265), Group.2 = c(117, 
    89, 66, 334, 90, 101, 178, 233, 45, 200), Group.3 = c(29, 101, 239, 289, 176, 320, 89, 109, 199, 56))

tidy_dataframe
