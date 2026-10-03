# 正文来源：CH5-波动率模型.tex，代码块 7；修订稿第 1043 行。
# 仅提取章末习题之前的正文；不含习题提示或答案。
# 语法已检查；未宣称全部外部数据与可选分支已执行。
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
}

library(readxl)

library(forecast)

library(vars)

library(mgarchBEKK)

dat <- read_excel("沪深300股指期现货收盘价.xlsx")

dat <- na.omit(dat)

date <- dat$日期

LnS <- log(dat$沪深300股指收盘价)

LnF <- log(dat$沪深300股指期货当月收盘价)

n <- length(LnS)

date.ret <- date[-1]

RS <- LnS[-1] - LnS[-n]

RF <- LnF[-1] - LnF[-n]

RS.tSeries <- data.frame(time = date.ret, RS)

RF.tSeries <- data.frame(time = date.ret, RF)

dat.var <- cbind(RF, RS)

VARselect(dat.var, lag.max = 12, type = "const")

var.2c <- VAR(dat.var, p = 3, type = "const")

ut <- na.omit(resid(var.2c))

estimated <- BEKK(ut)

diagnoseBEKK(estimated)
