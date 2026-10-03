# 正文来源：CH1-R语言概述.tex，代码块 14；修订稿第 435 行。
# 仅提取章末习题之前的正文；不含习题提示或答案。
# 语法已检查；未宣称全部外部数据与可选分支已执行。
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
