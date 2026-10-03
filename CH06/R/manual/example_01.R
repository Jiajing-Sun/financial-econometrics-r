# 正文来源：CH6-收益可预测性与有效市场假说.tex，代码块 1；修订稿第 530 行。
# 仅提取章末习题之前的正文；不含习题提示或答案。
# 语法已检查；未宣称全部外部数据与可选分支已执行。
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
}

library(tseries)

library(ggplot2)

library(vrtest)

Moutaidata <- read.csv("Moutai.csv")

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
