# 正文来源：CH4-自回归移动平均模型.tex，代码块 11；正文第 1280 行。
# 最新SVAR部分采用已识别AB限制；全章片段仅手动执行，见manual/index.csv。
dir.create("results/manual", recursive=TRUE, showWarnings=FALSE)
set.seed(123)

n <- 500

data <- arima.sim(n = n, model = list(ar = 0.5, ma = 0.4))

loglik_arma11 <- function(par, data) {
    phi <- par[1]
    theta <- par[2]
    sigma2 <- par[3]^2
    n <- length(data)
    eps <- rep(0, n)
    for (t in 2:n) {
        eps[t] <- data[t] - phi * data[t - 1] - theta * eps[t - 1]
    }
    sum_ll <- -n/2 * log(2 * pi * sigma2) - sum(eps^2)/(2 * sigma2)
    return(-sum_ll)
}

start_values <- c(0.5, 0.5, 1)

result <- optim(par = start_values, fn = loglik_arma11, data = data, method = "BFGS", hessian = TRUE)

cat("估计的参数:", result$par, "\n")

cat("估计值处的对数似然值:", -result$value, "\n")
