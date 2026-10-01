# 正文来源：CH9-连续金融模型.tex，代码块 9；正文第 1673 行。
# 只提取章末习题之前的正文代码；原控制台输出未纳入。
# 手动示例：可能依赖前序代码、外部文件、额外R包；参见本章README与manual/index.csv。
# 已移除自动安装、清空工作空间、保存整个工作空间及本机工作目录切换。
dir.create("results/manual", recursive=TRUE, showWarnings=FALSE)
PAV <- function(priceX, theta) {
    lprice <- log(priceX)
    delta_n <- 1/nrow(lprice)
    K_n <- round(theta/sqrt(delta_n)/2) * 2
    psi1 <- 1
    psi2 <- 1/12
    psi_K <- (1 + 2/K_n^2)/12
    Z <- matrix(0, ncol = ncol(lprice), nrow = ncol(lprice))
    for (j in 1:(nrow(lprice) - K_n + 1)) {
        r <- (1/K_n) * colSums(lprice[(j + K_n/2):(j + K_n - 1), ] - lprice[j:(j + K_n/2 - 1), ])
        Z <- Z + r %*% t(r)
    }
    CX <- sqrt(delta_n)/(theta * psi2) * Z - psi1 * delta_n/(2 * theta^2 * psi2) * t(diff(lprice)) %*% diff(lprice)
    return(CX * 252)
}
