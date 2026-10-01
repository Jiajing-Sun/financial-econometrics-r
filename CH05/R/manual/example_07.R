# 正文来源：CH5-波动率模型.tex，代码块 7；正文第 1059 行。
# 只提取章末习题之前的正文代码；原控制台输出未纳入。
# 手动示例：可能依赖前序代码、外部文件、额外R包；参见本章README与manual/index.csv。
# 已移除自动安装、清空工作空间、保存整个工作空间及本机工作目录切换。
dir.create("results/manual", recursive=TRUE, showWarnings=FALSE)
invisible(NULL)

invisible(NULL)

library(readxl)

library(forecast)

library(vars)

library(mgarchBEKK)

dat <- read_excel("data/user/沪深300股指期现货收盘价.xlsx")

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
