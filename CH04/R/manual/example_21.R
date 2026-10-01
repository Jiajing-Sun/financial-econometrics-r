# 正文来源：CH4-自回归移动平均模型.tex，代码块 21；正文第 3047 行。
# 最新SVAR部分采用已识别AB限制；全章片段仅手动执行，见manual/index.csv。
dir.create("results/manual", recursive=TRUE, showWarnings=FALSE)
set.seed(123)

stopifnot(requireNamespace("vars", quietly = TRUE))

K <- 3L

n <- 500L

Phi <- matrix(c(0.25, 0.05, 0.2, 0.1, 0.28, 0.22, 0.65, 0.45, 0.28), K, byrow = TRUE)

stopifnot(max(Mod(eigen(Phi)$values)) < 1)

A_true <- diag(K)

A_true[lower.tri(A_true)] <- c(-0.1, -0.06, 0.25)

B_true <- diag(c(1, 0.8, 1.2))

P_true <- solve(A_true, B_true)

Y <- matrix(0, n + 200L, K)

for (t in 2:nrow(Y)) {
    Y[t, ] <- Phi %*% Y[t - 1L, ] + P_true %*% rnorm(K)
}

Y <- ts(tail(Y, n))

colnames(Y) <- paste0("Series", 1:K)

fit_var <- vars::VAR(Y, p = 1, type = "none")

A_restr <- diag(K)

A_restr[lower.tri(A_restr)] <- NA_real_

B_restr <- matrix(0, K, K)

diag(B_restr) <- NA_real_

fit_ab <- vars::SVAR(fit_var, Amat = A_restr, Bmat = B_restr, estmethod = "scoring", max.iter = 1000)

sgn <- sign(diag(fit_ab$B))

stopifnot(all(sgn != 0))

fit_ab$B <- sweep(fit_ab$B, 2, sgn, "*")

P_hat <- solve(fit_ab$A, fit_ab$B)

print(fit_ab$A)

print(fit_ab$B)

print(fit_ab$Ase)

print(fit_ab$Bse)

stopifnot(all(diag(fit_ab$B) > 0), max(abs(fit_ab$A[upper.tri(fit_ab$A)])) < 1e-10, max(abs(fit_ab$B[row(fit_ab$B) != 
    col(fit_ab$B)])) < 1e-10)
