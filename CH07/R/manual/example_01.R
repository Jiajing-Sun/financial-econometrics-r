# 正文来源：CH7-非参数方法.tex，代码块 1；修订稿第 91 行。
# 仅提取章末习题之前的正文；不含习题提示或答案。
# 语法已检查；未宣称全部外部数据与可选分支已执行。
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
