# 正文来源：CH9-连续金融模型.tex，代码块 3；正文第 737 行。
# 只提取章末习题之前的正文代码；原控制台输出未纳入。
# 手动示例：可能依赖前序代码、外部文件、额外R包；参见本章README与manual/index.csv。
# 已移除自动安装、清空工作空间、保存整个工作空间及本机工作目录切换。
dir.create("results/manual", recursive=TRUE, showWarnings=FALSE)
if (!identical(Sys.getenv('FIN_ECON_ENABLE_NETWORK'), '1')) stop('此正文案例会联网；确认数据口径后设置 FIN_ECON_ENABLE_NETWORK=1 再手动运行。', call.=FALSE)
suppressPackageStartupMessages({
    library(MLEMVD)
    library(nloptr)
    library(MASS)
    library(numDeriv)
    library(quantmod)
})

if (!exists("price")) {
    getSymbols("SPY", src = "yahoo", from = "2015-01-01")
    price <- as.numeric(Ad(SPY))
}

n <- length(price)

del <- 1/252

scale_factor <- 1000

price_s <- price/scale_factor

ModelU6_with_args <- function(x, x0, del, param, args = NULL) MLEMVD::ModelU6(x, x0, del, param)

ModelU4_with_args <- function(x, x0, del, param, args = NULL) MLEMVD::ModelU4(x, x0, del, param)

total_loglik <- function(logdensity_fun, param, x, del) {
    s <- 0
    for (i in 1:(length(x) - 1)) s <- s + logdensity_fun(x[i + 1], x[i], del, param)$llk
    s
}

safe_vcov_from_I <- function(I, n) {
    I <- 0.5 * (I + t(I))
    I <- I + 1e-10 * diag(nrow(I))
    tryCatch(solve(I)/n, error = function(e) MASS::ginv(I)/n)
}

make_args <- function(param0, f_upper = 50, maxeval = 1500, method = c("LBFGS", "SBPLX"), deoptim_iter = 0, print_level = 0) {
    method <- match.arg(method)
    p <- length(param0)
    l <- rep(-1, p)
    u <- rep(1, p)
    l[p] <- 1e-08
    u[p] <- f_upper
    algo <- if (method == "LBFGS") 
        "NLOPT_LD_LBFGS"
    else "NLOPT_LN_SBPLX"
    list(mode = "direct", nloptr = list(method = algo, maxeval = maxeval, xtol_rel = 1e-08, ftol_rel = 1e-10, ftol_abs = 0, 
        print_level = print_level, l = l, u = u), DEoptim = list(maxiter = deoptim_iter, population = 80, strategy = 2), 
        eval_g_ineq = NULL, eval_jac_g_ineq = NULL)
}

series_ll_finite <- function(logdensity_fun, x, del, par) {
    s <- 0
    for (i in 1:(length(x) - 1)) {
        li <- try(logdensity_fun(x[i + 1], x[i], del, par)$llk, silent = TRUE)
        if (!is.numeric(li) || !is.finite(li)) 
            return(FALSE)
        s <- s + li
    }
    is.finite(s)
}

start_u6 <- c(a = 0, b = 0, c = 0, d = 0, f = 0.05)

args_u6 <- make_args(start_u6, f_upper = 50, maxeval = 1500, method = "LBFGS", deoptim_iter = 0)

cat("\n=== U6（近似 MLE，三次漂移，常数扩散）===\n")

fit_u6 <- mle(logdensity = ModelU6_with_args, x = price_s, del = del, param0 = start_u6, args = args_u6)

theta_u6 <- setNames(as.numeric(fit_u6$solution), names(start_u6))

print(theta_u6)

I_u6 <- as.matrix(logdensity2info(logdensity = ModelU6_with_args, x = price_s, del = del, param = theta_u6))

vcov_u6 <- safe_vcov_from_I(I_u6, n)

se_u6 <- sqrt(diag(vcov_u6))

if (any(!is.finite(se_u6))) {
    cat("（提示）U6 信息矩阵条件数差，改用数值 Hessian。\n")
    H_u6 <- numDeriv::hessian(function(p) -total_loglik(ModelU6_with_args, p, price_s, del), theta_u6)
    vcov_u6 <- safe_vcov_from_I(H_u6, n)
    se_u6 <- sqrt(diag(vcov_u6))
}

cat("\nU6 估计与标准误：\n")

print(round(cbind(Estimate = theta_u6, Std.Error = se_u6), 6))

set.seed(123)

best_par <- NULL

best_ll <- -Inf

for (k in 1:500) {
    a <- runif(1, -0.8, 0.8)
    b <- runif(1, -0.8, 0.8)
    c <- runif(1, -0.8, 0.8)
    f <- exp(runif(1, log(0.001), log(50)))
    par <- c(a = a, b = b, c = c, f = f)
    if (series_ll_finite(ModelU4_with_args, price_s, del, par)) {
        ll <- total_loglik(ModelU4_with_args, par, price_s, del)
        if (ll > best_ll) {
            best_ll <- ll
            best_par <- par
        }
    }
}

if (is.null(best_par)) stop("未能找到 U4 的可行起点：可进一步增大 scale_factor 或放宽边界。")

start_u4 <- best_par

message("U4 可行起点：", paste(round(start_u4, 6), collapse = ", "))

args_u4 <- make_args(start_u4, f_upper = 100, maxeval = 2500, method = "SBPLX", deoptim_iter = 0, print_level = 1)

cat("\n=== U4（稳健近似 MLE，二次漂移，常数扩散）===\n")

fit_u4 <- mle(logdensity = ModelU4_with_args, x = price_s, del = del, param0 = start_u4, args = args_u4)

theta_u4 <- setNames(as.numeric(fit_u4$solution), c("a", "b", "c", "f"))

print(theta_u4)

I_u4_try <- try(as.matrix(logdensity2info(logdensity = ModelU4_with_args, x = price_s, del = del, param = theta_u4)), 
    silent = TRUE)

if (inherits(I_u4_try, "try-error") || any(!is.finite(I_u4_try))) {
    cat("（提示）U4 信息矩阵不可用/不稳，改用数值 Hessian。\n")
    H_u4 <- numDeriv::hessian(function(p) -total_loglik(ModelU4_with_args, p, price_s, del), theta_u4)
    vcov_u4 <- safe_vcov_from_I(H_u4, n)
} else {
    vcov_u4 <- safe_vcov_from_I(I_u4_try, n)
}

se_u4 <- sqrt(diag(vcov_u4))

cat("\nU4 估计与标准误：\n")

print(round(cbind(Estimate = theta_u4, Std.Error = se_u4), 6))

cat("\n完成。\n")
