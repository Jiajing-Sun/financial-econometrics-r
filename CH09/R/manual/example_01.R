# 正文来源：CH9-连续金融模型.tex，代码块 1；正文第 91 行。
# 只提取章末习题之前的正文代码；原控制台输出未纳入。
# 手动示例：可能依赖前序代码、外部文件、额外R包；参见本章README与manual/index.csv。
# 已移除自动安装、清空工作空间、保存整个工作空间及本机工作目录切换。
dir.create("results/manual", recursive=TRUE, showWarnings=FALSE)
set.seed(123)

n <- 1000

T <- 1

dt <- T/n

t <- seq(0, T, length.out = n + 1)

dW <- rnorm(n, mean = 0, sd = sqrt(dt))

W <- c(0, cumsum(dW))

plot(t, W, type = "l", main = "标准布朗运动", xlab = "时间", ylab = "W (t) ", col = "blue", lwd = 2)
