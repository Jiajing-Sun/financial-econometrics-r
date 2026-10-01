# 正文来源：CH9-连续金融模型.tex，代码块 8；正文第 1558 行。
# 只提取章末习题之前的正文代码；原控制台输出未纳入。
# 手动示例：可能依赖前序代码、外部文件、额外R包；参见本章README与manual/index.csv。
# 已移除自动安装、清空工作空间、保存整个工作空间及本机工作目录切换。
dir.create("results/manual", recursive=TRUE, showWarnings=FALSE)
RK <- function(priceX, slows, fasts) {
    lprice <- log(priceX)
    bin11 <- floor(nrow(lprice)/slows)
    bin22 <- floor(nrow(lprice)/fasts)
    rv10m1 <- matrix(0, nrow = slows, ncol = ncol(lprice))
    for (j in 1:slows) {
        cut <- seq(j + nrow(lprice)%%slows, length.out = bin11, by = slows)
        cut1 <- lprice[cut, , drop = FALSE]
        rv10m1[j, ] <- colSums(diff(cut1)^2)
    }
    rv10m <- colMeans(rv10m1)
    rv30s1 <- matrix(0, nrow = fasts, ncol = ncol(lprice))
    for (j in 1:fasts) {
        cut <- seq(j + nrow(lprice)%%fasts, length.out = bin22, by = fasts)
        cut1 <- lprice[cut, , drop = FALSE]
        rv30s1[j, ] <- colSums(diff(cut1)^2)
    }
    rv30s <- colMeans(rv30s1)
    noisev <- rv30s/((bin22 - 1) * 2)
    h <- round(mean(3.51 * (noisev/rv10m)^0.4 * (nrow(lprice) - 1)^0.6))
    r <- diff(lprice)
    RK <- t(r) %*% r
    for (hh in 1:h) {
        x <- (hh - 1)/h
        if (x <= 0.5) {
            k <- 1 - 6 * x^2 + 6 * x^3
        }
        else {
            k <- 2 * (1 - x)^3
        }
        RK <- RK + k * (t(r[(hh + 1):nrow(r), , drop = FALSE]) %*% r[1:(nrow(r) - hh), , drop = FALSE] + t(r[1:(nrow(r) - 
            hh), , drop = FALSE]) %*% r[(hh + 1):nrow(r), , drop = FALSE])
    }
    return(RK * 252)
}
