# 正文来源：CH7-非参数方法.tex，代码块 1；正文第 91 行。
# 只提取章末习题之前的正文代码；原控制台输出未纳入。
# 手动示例：可能依赖前序代码、外部文件、额外R包；参见本章README与manual/index.csv。
# 已移除自动安装、清空工作空间、保存整个工作空间及本机工作目录切换。
dir.create("results/manual", recursive=TRUE, showWarnings=FALSE)
func_uniform <- function(x) {
    y <- numeric(length(x))
    idx <- abs(x) <= 1
    y[idx] <- 1/2
    return(y)
}

func_gaussian <- function(x) {
    y <- (1/sqrt(2 * pi)) * exp(-0.5 * x^2)
    return(y)
}

func_e <- function(x) {
    y <- numeric(length(x))
    idx <- abs(x) <= 1
    y[idx] <- (3/4) * (1 - x[idx]^2)
    return(y)
}

func_quartic <- function(x) {
    y <- numeric(length(x))
    idx <- abs(x) <= 1
    y[idx] <- (15/16) * (1 - x[idx]^2)^2
    return(y)
}
