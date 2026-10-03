# 正文来源：CH9-连续金融模型.tex，代码块 5；修订稿第 1148 行。
# 仅提取章末习题之前的正文；不含习题提示或答案。
# 语法已检查；未宣称全部外部数据与可选分支已执行。
TSRV <- function(price, K, annual_factor = 1) {
    stopifnot(is.numeric(price), all(is.finite(price)), all(price > 0), length(K) == 1, K ==
        as.integer(K), length(annual_factor) == 1, annual_factor > 0)
    y <- log(price)
    n <- length(y) - 1L
    stopifnot(K > 1, K <= n)
    rv_sub <- m <- numeric(K)
    for (j in 0:(K - 1L)) {
        z <- y[seq.int(j + 1L, n + 1L, by = K)]
        m[j + 1L] <- length(z) - 1L
        rv_sub[j + 1L] <- sum(diff(z)^2)
    }
    ratio <- mean(m)/n
    estimate <- (mean(rv_sub) - ratio * sum(diff(y)^2))/(1 - ratio)
    annual_factor * estimate
}

raw <- readxl::read_excel("AA_daily20120103.xlsx", col_names = FALSE)

price <- as.numeric(raw[[5]])

K <- max(2L, floor((length(price) - 1L)^(2/3)))

TSRV(price, K)
