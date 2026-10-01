# 正文来源：CH9-连续金融模型.tex，代码块 10；正文第 1715 行。
# 只提取章末习题之前的正文代码；原控制台输出未纳入。
# 手动示例：可能依赖前序代码、外部文件、额外R包；参见本章README与manual/index.csv。
# 已移除自动安装、清空工作空间、保存整个工作空间及本机工作目录切换。
dir.create("results/manual", recursive=TRUE, showWarnings=FALSE)
compute_mvp_weights_simple <- function(cov_matrix) {
    n <- nrow(cov_matrix)
    ones <- rep(1, n)
    inv_cov_matrix <- solve(cov_matrix)
    num <- inv_cov_matrix %*% ones
    den <- t(ones) %*% inv_cov_matrix %*% ones
    weights <- num/as.numeric(den)
    return(weights)
}
