# 正文来源：CH9-连续金融模型.tex，代码块 11；修订稿第 1665 行。
# 仅提取章末习题之前的正文；不含习题提示或答案。
# 语法已检查；未宣称全部外部数据与可选分支已执行。
compute_mvp_weights <- function(cov_matrix) {
    n <- nrow(cov_matrix)
    Dmat <- 2 * cov_matrix
    dvec <- rep(0, n)
    Amat <- cbind(rep(1, n), diag(n))
    bvec <- c(1, rep(0, n))
    meq <- 1
    solution <- quadprog::solve.QP(Dmat, dvec, Amat, bvec, meq)
    return(solution$solution)
}
