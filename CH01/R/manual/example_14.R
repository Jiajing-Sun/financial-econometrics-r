# 正文来源：CH1-R语言概述.tex，代码块 14；正文第 433 行。
# 只提取章末习题之前的正文代码；原控制台输出未纳入。
# 手动示例：可能依赖前序代码、外部文件、额外R包；参见本章README与manual/index.csv。
# 已移除自动安装、清空工作空间、保存整个工作空间及本机工作目录切换。
dir.create("results/manual", recursive=TRUE, showWarnings=FALSE)
e_fcn_1 <- function(n = 2000) {
    (1 + 1/n)^n
}

e_fcn_1(1)

e_fcn_1(100)

e_fcn_1(10000)

exp(1)

e_fcn_2 <- function(n = 2000) {
    e <- 0
    for (i in 0:n) {
        e <- e + 1/factorial(i)
    }
    return(e)
}

e_fcn_2(1)

e_fcn_2(10)

e_fcn_3 <- function(n = 2000) {
    e <- sum(1/factorial(0:n))
    return(e)
}

e_fcn_3(1)

e_fcn_3(10)
