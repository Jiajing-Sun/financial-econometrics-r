# 正文来源：CH11-风险管理与极值理论.tex，代码块 4；正文第 1368 行。
# 只提取章末习题之前的正文代码；原控制台输出未纳入。
# 手动示例：可能依赖前序代码、外部文件、额外R包；参见本章README与manual/index.csv。
# 已移除自动安装、清空工作空间、保存整个工作空间及本机工作目录切换。
dir.create("results/manual", recursive=TRUE, showWarnings=FALSE)
if (!identical(Sys.getenv('FIN_ECON_ENABLE_NETWORK'), '1')) stop('此正文案例会联网；确认数据口径后设置 FIN_ECON_ENABLE_NETWORK=1 再手动运行。', call.=FALSE)
req <- c("quantmod", "xts", "zoo", "rugarch")

inst <- setdiff(req, rownames(installed.packages()))

if (length(inst)) stop("此手动示例缺少依赖；请先参照章节README自行安装。", call. = FALSE)

suppressPackageStartupMessages(invisible(lapply(req, library, character.only = TRUE)))

set.seed(1)

ticker <- "^GSPC"

start_date <- "1990-01-01"

alpha_set <- c(0.01, 0.05)

roll_win <- 250

x <- suppressWarnings(getSymbols(ticker, src = "yahoo", from = start_date, auto.assign = FALSE))

px <- Ad(x)

ret <- na.omit(diff(log(px)))

colnames(ret) <- "r"

stopifnot(NROW(ret) > roll_win + 20)

r_num <- as.numeric(ret)

es_empirical <- function(x, alpha) {
    q <- unname(quantile(x, probs = alpha, type = 7, na.rm = TRUE))
    es1 <- -mean(x[x <= q])
    es2 <- -(mean(x * (x <= q))/alpha)
    c(VaR = -q, ES_via_cond_mean = es1, ES_via_tail_avg = es2)
}

cat("== 无条件 ES：整体样本 ==\n")

for (a in alpha_set) {
    out <- es_empirical(r_num, a)
    cat(sprintf("alpha=%.2f -> VaR(loss)=%.4f, ES1=%.4f, ES2=%.4f\n", a, out["VaR"], out["ES_via_cond_mean"], out["ES_via_tail_avg"]))
}

roll_es <- function(x, alpha, win) {
    n <- length(x)
    res <- matrix(NA_real_, n, 3)
    colnames(res) <- c("VaR", "ES1", "ES2")
    for (t in seq_len(n)) {
        if (t < win) 
            next
        w <- x[(t - win + 1):t]
        tmp <- es_empirical(w, alpha)
        res[t, ] <- tmp
    }
    xts(res, order.by = index(ret))
}

roll_res_list <- lapply(alpha_set, function(a) roll_es(r_num, a, roll_win))

names(roll_res_list) <- paste0("alpha_", alpha_set)

spec_norm <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), mean.model = list(armaOrder = c(0, 
    0), include.mean = TRUE), distribution.model = "norm")

fit_norm <- ugarchfit(spec_norm, ret, solver = "hybrid")

spec_t <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), mean.model = list(armaOrder = c(0, 
    0), include.mean = TRUE), distribution.model = "std")

fit_t <- ugarchfit(spec_t, ret, solver = "hybrid")

mu_n <- as.numeric(fitted(fit_norm))

sig_n <- as.numeric(sigma(fit_norm))

mu_t <- as.numeric(fitted(fit_t))

sig_t <- as.numeric(sigma(fit_t))

coef_t <- coef(fit_t)

nu <- as.numeric(coef_t[grep("shape", names(coef_t), ignore.case = TRUE)])

if (!is.finite(nu)) stop("未能从 GARCH-t 中识别自由度参数 shape/df。")

m_alpha_norm <- function(alpha) {
    z <- qnorm(alpha)
    -dnorm(z)/alpha
}

m_alpha_std <- function(alpha, nu) {
    q <- qdist("std", alpha, mu = 0, sigma = 1, skew = 1, shape = nu)
    f <- ddist("std", q, mu = 0, sigma = 1, skew = 1, shape = nu)
    -((nu + q^2)/(nu - 1)) * f/alpha
}

cond_paths <- lapply(alpha_set, function(a) {
    z <- qnorm(a)
    mn <- m_alpha_norm(a)
    VaR_loss_norm <- -(mu_n + sig_n * z)
    ES_loss_norm <- -(mu_n + sig_n * mn)
    qt_ <- qdist("std", a, mu = 0, sigma = 1, skew = 1, shape = nu)
    mt_ <- m_alpha_std(a, nu)
    VaR_loss_t <- -(mu_t + sig_t * qt_)
    ES_loss_t <- -(mu_t + sig_t * mt_)
    xts(cbind(VaR_norm = VaR_loss_norm, ES_norm = ES_loss_norm, VaR_t = VaR_loss_t, ES_t = ES_loss_t), order.by = index(ret))
})

names(cond_paths) <- paste0("alpha_", alpha_set)

cat("\n== 条件 ES（GARCH）示例：最近一个交易日 ==\n")

last_row <- function(X) as.numeric(tail(X, 1))

for (nm in names(cond_paths)) {
    a <- sub("alpha_", "", nm)
    v <- cond_paths[[nm]]
    vals <- round(last_row(v), 6)
    names(vals) <- colnames(v)
    cat(sprintf("alpha=%s  ->  %s\n", a, paste(sprintf("%s=%g", names(vals), vals), collapse = ", ")))
}

cat("\n== 滚动历史法 ES 示例：最近一个交易日 ==\n")

for (nm in names(roll_res_list)) {
    a <- sub("alpha_", "", nm)
    v <- roll_res_list[[nm]]
    vals <- round(as.numeric(tail(v, 1)), 6)
    names(vals) <- colnames(v)
    cat(sprintf("alpha=%s  ->  %s\n", a, paste(sprintf("%s=%g", names(vals), vals), collapse = ", ")))
}

a_plot <- "alpha_0.01"

if (a_plot %in% names(cond_paths)) {
    series_c <- cond_paths[[a_plot]]
    series_h <- roll_res_list[[a_plot]]
    one_year <- 252
    r_tail <- tail(ret, one_year)
    c_tail <- tail(series_c, one_year)
    h_tail <- tail(series_h, one_year)
    op <- par(no.readonly = TRUE)
    on.exit(par(op), add = TRUE)
    par(mfrow = c(1, 2), mar = c(4, 4, 2, 1))
    plot(index(r_tail), as.numeric(r_tail), type = "h", main = "历史法（滚动窗口）VaR/ES（alpha=1%）", 
        xlab = "", ylab = "日对数收益", col = "grey40")
    lines(index(h_tail), -h_tail$VaR, lwd = 2)
    lines(index(h_tail), -h_tail$ES1, lwd = 2, lty = 2)
    legend("bottomleft", c("收益", "-VaR (loss)", "-ES (loss)"), lty = c(1, 1, 2), lwd = c(1, 2, 2), col = c("grey40", 
        "black", "black"), bty = "n")
    plot(index(r_tail), as.numeric(r_tail), type = "h", main = "条件 GARCH-t VaR/ES（alpha=1%）", xlab = "", 
        ylab = "日对数收益", col = "grey40")
    lines(index(c_tail), -c_tail$VaR_t, lwd = 2)
    lines(index(c_tail), -c_tail$ES_t, lwd = 2, lty = 2)
    legend("bottomleft", c("收益", "-VaR_t (loss)", "-ES_t (loss)"), lty = c(1, 1, 2), lwd = c(1, 2, 2), col = c("grey40", 
        "black", "black"), bty = "n")
    par(mfrow = c(1, 1))
}
