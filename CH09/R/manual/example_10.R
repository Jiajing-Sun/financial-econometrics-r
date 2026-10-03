# 正文来源：CH9-连续金融模型.tex，代码块 10；修订稿第 1640 行。
# 仅提取章末习题之前的正文；不含习题提示或答案。
# 语法已检查；未宣称全部外部数据与可选分支已执行。
compute_mvp_weights_simple <- function(cov_matrix) {
    n <- nrow(cov_matrix)
    ones <- rep(1, n)
    inv_cov_matrix <- solve(cov_matrix)
    num <- inv_cov_matrix %*% ones
    den <- t(ones) %*% inv_cov_matrix %*% ones
    weights <- num/as.numeric(den)
    return(weights)
}
