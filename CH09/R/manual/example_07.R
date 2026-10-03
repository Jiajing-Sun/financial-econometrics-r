# 正文来源：CH9-连续金融模型.tex，代码块 7；修订稿第 1414 行。
# 仅提取章末习题之前的正文；不含习题提示或答案。
# 语法已检查；未宣称全部外部数据与可选分支已执行。
TSRCov <- function(price, slows, fasts = 1L, annual_factor = 1) {
    price <- as.matrix(price)
    stopifnot(is.numeric(price), all(is.finite(price)), all(price > 0))
    y <- log(price)
    n <- nrow(y) - 1L
    stopifnot(length(slows) == 1, length(fasts) == 1, slows == as.integer(slows), fasts ==
        as.integer(fasts), 1 <= fasts, fasts < slows, slows <= n, length(annual_factor) ==
        1, annual_factor > 0)
    rv <- function(k) {
        idx <- seq_len(n + 1L - k)
        z <- y[idx + k, , drop = FALSE] - y[idx, , drop = FALSE]
        crossprod(z)/k
    }
    ratio <- ((n - slows + 1)/slows)/((n - fasts + 1)/fasts)
    annual_factor * (rv(slows) - ratio * rv(fasts))/(1 - ratio)
}
