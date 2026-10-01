# 正文来源：CH1-R语言概述.tex，代码块 77；正文第 2079 行。
# 只提取章末习题之前的正文代码；原控制台输出未纳入。
# 手动示例：可能依赖前序代码、外部文件、额外R包；参见本章README与manual/index.csv。
# 已移除自动安装、清空工作空间、保存整个工作空间及本机工作目录切换。
dir.create("results/manual", recursive=TRUE, showWarnings=FALSE)
ggplot(data = mtcars, aes(x = hp)) + geom_histogram(binwidth = 10, fill = "skyblue", color = "black") + labs(title = "Distribution of Horsepower", 
    x = "Engine Power (HP) ", y = "Frequency")
