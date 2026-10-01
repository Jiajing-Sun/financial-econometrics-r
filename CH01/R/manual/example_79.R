# 正文来源：CH1-R语言概述.tex，代码块 79；正文第 2102 行。
# 只提取章末习题之前的正文代码；原控制台输出未纳入。
# 手动示例：可能依赖前序代码、外部文件、额外R包；参见本章README与manual/index.csv。
# 已移除自动安装、清空工作空间、保存整个工作空间及本机工作目录切换。
dir.create("results/manual", recursive=TRUE, showWarnings=FALSE)
p + facet_grid(. ~ cyl) + labs(title = "Fuel Efficiency vs Engine Power by Cylinder Count")
