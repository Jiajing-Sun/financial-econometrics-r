# 正文来源：CH9-连续金融模型.tex，代码块 9；修订稿第 1605 行。
# 仅提取章末习题之前的正文；不含习题提示或答案。
# 语法已检查；未宣称全部外部数据与可选分支已执行。
PAV <- function(price, theta = 0.8, annual_factor = 1) {
    price <- as.matrix(price)
    stopifnot(is.numeric(price), all(is.finite(price)), all(price > 0), length(theta) == 1,
        theta > 0, length(annual_factor) == 1, annual_factor > 0)
    r <- diff(log(price))
    N <- nrow(r)
    k <- floor(theta * sqrt(N))
    stopifnot(k >= 2L, k <= N)
    g <- function(x) pmin(x, 1 - x)
    w <- g((1:(k - 1L))/k)
    psi1 <- k * sum(diff(g((0:k)/k))^2)
    psi2 <- sum(w^2)/k
    z <- matrix(0, N - k + 2L, ncol(r))
    for (i in 0:(N - k + 1L)) {
        z[i + 1L, ] <- as.numeric(crossprod(w, r[i + seq_len(k - 1L), , drop = FALSE]))
    }
    signal <- N/(N - k + 2) * crossprod(z)/(psi2 * k)
    correction <- psi1 * crossprod(r)/(2 * psi2 * k^2)
    annual_factor * (signal - correction)
}
