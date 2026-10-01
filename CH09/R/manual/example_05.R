# 正文来源：CH9-连续金融模型.tex，代码块 5；正文第 1167 行。
# 只提取章末习题之前的正文代码；原控制台输出未纳入。
# 手动示例：可能依赖前序代码、外部文件、额外R包；参见本章README与manual/index.csv。
# 已移除自动安装、清空工作空间、保存整个工作空间及本机工作目录切换。
dir.create("results/manual", recursive=TRUE, showWarnings=FALSE)
invisible(NULL)

set.seed(123)

invisible(NULL)

library(readxl)

aa <- read_excel("data/user/AA_daily20120103.xlsx", col_names = FALSE)

TSRV <- function(priceX, K, m, n) {
    lprice <- log(priceX)
    RV_sub <- numeric(K)
    for (j in 1:K) {
        sub_sample <- lprice[seq(j, by = K, length.out = m + 1)]
        returns <- diff(sub_sample)
        RV_sub[j] <- sum(returns^2)
    }
    returns_full <- diff(lprice)
    RV_n <- sum(returns_full^2)
    TSRV_estimator <- (1/K) * sum(RV_sub) - (m/n) * RV_n
    return(TSRV_estimator)
}

priceX <- aa$...5

K <- 5

m <- 100

n <- length(priceX)

TSRV_estimate <- TSRV(priceX, K, m, n)

TSRV_estimate
