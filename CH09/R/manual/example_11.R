# 正文来源：CH9-连续金融模型.tex，代码块 11；正文第 1739 行。
# 只提取章末习题之前的正文代码；原控制台输出未纳入。
# 手动示例：可能依赖前序代码、外部文件、额外R包；参见本章README与manual/index.csv。
# 已移除自动安装、清空工作空间、保存整个工作空间及本机工作目录切换。
dir.create("results/manual", recursive=TRUE, showWarnings=FALSE)
compute_mvp_weights <- function(cov_matrix) {
    n <- nrow(cov_matrix)
    Dmat <- 2 * cov_matrix
    dvec <- rep(0, n)
    Amat <- cbind(rep(1, n), diag(n))
    bvec <- c(1, rep(0, n))
    meq <- 1
    solution <- solve.QP(Dmat, dvec, Amat, bvec, meq)
    return(solution$solution)
}
