# 正文来源：CH6-收益可预测性与有效市场假说.tex，代码块 1；正文第 538 行。
# 只提取章末习题之前的正文代码；原控制台输出未纳入。
# 手动示例：可能依赖前序代码、外部文件、额外R包；参见本章README与manual/index.csv。
# 已移除自动安装、清空工作空间、保存整个工作空间及本机工作目录切换。
dir.create("results/manual", recursive=TRUE, showWarnings=FALSE)
invisible(NULL)

invisible(NULL)

library(tseries)

library(ggplot2)

library(vrtest)

Moutaidata <- read.csv("data/user/Moutai.csv")

Moutaidata <- data.frame(Date = Moutaidata$date, MoutaiPrice = Moutaidata$GuizhouMoutai_600519_ClosePrice_Adj)

Moutaidata$Date <- as.Date(Moutaidata$Date)

Moutaidata$DiffPrice <- c(NA, diff(Moutaidata$MoutaiPrice))

Moutaidata$LogReturns <- c(NA, diff(log(Moutaidata$MoutaiPrice)))

Moutaidata <- Moutaidata[-1, ]

Box_Pierce_result <- Box.test(Moutaidata$DiffPrice, lag = 10, type = "Box-Pierce")

print(Box_Pierce_result)

Box_Ljung_result <- Box.test(Moutaidata$DiffPrice, lag = 10, type = "Ljung-Box")

print(Box_Ljung_result)

AutoBoot_result <- AutoBoot.test(Moutaidata$LogReturns, nboot = 500, wild = "Normal")

print(AutoBoot_result)
