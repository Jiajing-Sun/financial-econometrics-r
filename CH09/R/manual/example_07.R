# 正文来源：CH9-连续金融模型.tex，代码块 7；正文第 1445 行。
# 只提取章末习题之前的正文代码；原控制台输出未纳入。
# 手动示例：可能依赖前序代码、外部文件、额外R包；参见本章README与manual/index.csv。
# 已移除自动安装、清空工作空间、保存整个工作空间及本机工作目录切换。
dir.create("results/manual", recursive=TRUE, showWarnings=FALSE)
TSRV <- function(priceX, slows, fasts) {
    lprice <- log(priceX)
    bin11 <- floor(nrow(lprice)/slows)
    bin22 <- floor(nrow(lprice)/fasts)
    rv5m1 <- matrix(0, ncol = ncol(lprice), nrow = ncol(lprice))
    for (j in 1:slows) {
        cut <- seq(j + nrow(lprice)%%slows, by = slows, length.out = bin11)
        cut1 <- lprice[cut, ]
        diff_cut1 <- diff(cut1)
        rv5m1 <- rv5m1 + t(diff_cut1) %*% diff_cut1
    }
    rv5m <- rv5m1/slows
    rv30s1 <- matrix(0, ncol = ncol(lprice), nrow = ncol(lprice))
    for (j in 1:fasts) {
        cut <- seq(j + nrow(lprice)%%fasts, by = fasts, length.out = bin22)
        cut1 <- lprice[cut, ]
        diff_cut1 <- diff(cut1)
        rv30s1 <- rv30s1 + t(diff_cut1) %*% diff_cut1
    }
    rv30s <- rv30s1/fasts
    TSRV <- rv5m - (fasts/slows) * rv30s
    return(TSRV * 252)
}
