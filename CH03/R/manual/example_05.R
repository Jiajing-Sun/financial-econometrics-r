# 正文来源：CH3-回归模型及其应用.tex，代码块 5；修订稿第 685 行。
# 仅提取章末习题之前的正文；不含习题提示或答案。
# 语法已检查；未宣称全部外部数据与可选分支已执行。
if (!identical(Sys.getenv('FIN_ECON_ENABLE_NETWORK'),'1')) stop('此正文示例可能联网；请设置 FIN_ECON_ENABLE_NETWORK=1 后手动运行。')
from_date <- "2005-01-01"

to_date <- "2024-12-31"

lag_nw <- 6

ym_first_day <- function(xDate) {
    y <- as.integer(format(as.Date(xDate), "%Y"))
    m <- as.integer(format(as.Date(xDate), "%m"))
    as.Date(sprintf("%04d-%02d-01", y, m))
}

as_ym01_from_YYYYMM <- function(YYYYMM) {
    s <- sprintf("%06d", as.integer(YYYYMM))
    as.Date(paste0(substr(s, 1, 4), "-", substr(s, 5, 6), "-01"))
}

nw_cov <- function(X, u, lag = 0) {
    n <- nrow(X)
    k <- ncol(X)
    S <- matrix(0, k, k)
    for (t in 1:n) {
        xt <- X[t, , drop = FALSE]
        S <- S + t(xt) %*% xt * (u[t]^2)
    }
    if (lag > 0) {
        for (L in 1:lag) {
            wL <- 1 - L/(lag + 1)
            S_L <- matrix(0, k, k)
            for (t in (L + 1):n) {
                xt <- X[t, , drop = FALSE]
                xtL <- X[t - L, , drop = FALSE]
                S_L <- S_L + t(xt) %*% xtL * (u[t] * u[t - L])
            }
            S <- S + wL * (S_L + t(S_L))
        }
    }
    XtX_inv <- solve(t(X) %*% X)
    V <- XtX_inv %*% S %*% XtX_inv
    V
}

print_lm_with_nw <- function(mod, lag = 6, title = "") {
    cat("\n==============================\n", title, "\n", sep = "")
    s <- summary(mod)
    co <- coef(mod)
    X <- model.matrix(mod)
    u <- residuals(mod)
    Vnw <- nw_cov(X, u, lag = lag)
    se_nw <- sqrt(diag(Vnw))
    est <- as.numeric(co)
    tval <- est/se_nw
    df <- nrow(X) - ncol(X)
    pval <- 2 * pt(abs(tval), df = df, lower.tail = FALSE)
    out <- cbind(Estimate = est, NW_Std.Err = se_nw, `t(NW)` = tval, `Pr(>|t|)` = pval)
    rownames(out) <- names(co)
    print(round(out, 6))
    cat(sprintf("\nR-squared: %.3f   Adj R-squared: %.3f   RSE: %.4f   N: %d\n", s$r.squared,
        s$adj.r.squared, s$sigma, s$df[1] + s$df[2]))
}

fetch_yahoo_chart_monthly <- function(symbol, from_date, to_date) {
    p1 <- as.integer(as.POSIXct(as.Date(from_date), tz = "UTC"))
    p2 <- as.integer(as.POSIXct(as.Date(to_date), tz = "UTC"))
    url <- paste0("https://query1.finance.yahoo.com/v8/finance/chart/", symbol, "?period1=",
        p1, "&period2=", p2, "&interval=1mo&events=div%2Csplit")
    tf <- tempfile(fileext = ".json")
    utils::download.file(url, tf, quiet = TRUE, mode = "wb")
    js <- paste(readLines(tf, warn = FALSE), collapse = "")
    ts_pat <- "\"timestamp\"\\s*:\\s*\\[([^\\]]+)\\]"
    ts_m <- regexpr(ts_pat, js, perl = TRUE)
    if (ts_m[1] == -1)
        stop("未找到 timestamp 数组：可能是符号无数据或网络受限。")
    ts_txt <- regmatches(js, ts_m)
    ts_inside <- sub("^\"timestamp\"\\s*:\\s*\\[", "", sub("\\]$", "", ts_txt))
    ts_vals <- as.numeric(unlist(strsplit(ts_inside, ",")))
    dates <- as.POSIXct(ts_vals, origin = "1970-01-01", tz = "UTC")
    months <- as.Date(format(dates, "%Y-%m-01"))
    ac_pat <- "\"adjclose\"\\s*:\\s*\\[\\s*\\{\\s*\"adjclose\"\\s*:\\s*\\[([^\\]]+)\\]"
    ac_m <- regexpr(ac_pat, js, perl = TRUE)
    if (ac_m[1] == -1)
        stop("未找到 adjclose 数组：结构变化或无数据。")
    ac_txt <- regmatches(js, ac_m)
    ac_inside <- sub("^\"adjclose\"\\s*:\\s*\\[\\s*\\{\\s*\"adjclose\"\\s*:\\s*\\[", "", sub("\\]$",
        "", ac_txt))
    ac_inside <- gsub("null", "NA", ac_inside, fixed = TRUE)
    adj <- as.numeric(unlist(strsplit(ac_inside, ",")))
    n <- min(length(months), length(adj))
    months <- months[seq_len(n)]
    adj <- adj[seq_len(n)]
    ok <- !is.na(months) & !is.na(adj)
    months <- months[ok]
    adj <- adj[ok]
    ret <- c(NA, adj[-1]/adj[-length(adj)] - 1)
    out <- data.frame(month = months, ret = ret, stringsAsFactors = FALSE)
    out <- out[!is.na(out$ret), ]
    out
}

aapl <- fetch_yahoo_chart_monthly("AAPL", from_date, to_date)

spy <- fetch_yahoo_chart_monthly("SPY", from_date, to_date)

rets <- merge(aapl, spy, by = "month", all = FALSE, suffixes = c("_AAPL", "_SPY"))

names(rets) <- c("month", "AAPL", "SPY")

read_ff_zip_csv <- function(zip_url, inner_name_pattern, skip_lines) {
    tf <- tempfile(fileext = ".zip")
    utils::download.file(zip_url, tf, mode = "wb", quiet = TRUE)
    lst <- utils::unzip(tf, list = TRUE)$Name
    target <- lst[grep(inner_name_pattern, lst, ignore.case = TRUE)][1]
    csv_path <- utils::unzip(tf, files = target, exdir = tempdir(), overwrite = TRUE)[1]
    dat <- utils::read.csv(csv_path, skip = skip_lines, header = TRUE, check.names = FALSE,
        stringsAsFactors = FALSE)
    colnames(dat)[1] <- "Date"
    ok <- !is.na(suppressWarnings(as.integer(dat$Date)))
    dat <- dat[ok, , drop = FALSE]
    dat
}

ff5_url <- "https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_5_Factors_2x3_CSV.zip"

ff5_raw <- read_ff_zip_csv(ff5_url, "F-F_Research_Data_5_Factors_2x3.CSV", skip_lines = 3)

ff5 <- data.frame(month = as_ym01_from_YYYYMM(ff5_raw$Date), Mkt_RF = as.numeric(ff5_raw[["Mkt-RF"]]),
    SMB = as.numeric(ff5_raw[["SMB"]]), HML = as.numeric(ff5_raw[["HML"]]), RMW = as.numeric(ff5_raw[["RMW"]]),
    CMA = as.numeric(ff5_raw[["CMA"]]), RF = as.numeric(ff5_raw[["RF"]]), stringsAsFactors = FALSE)

ff5 <- ff5[!is.na(ff5$month), ]

ff5 <- ff5[!duplicated(ff5$month), ]

ff5 <- ff5[ff5$month >= as.Date(from_date) & ff5$month <= as.Date(to_date), ]

mom_url <- "https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Momentum_Factor_CSV.zip"

mom_raw <- read_ff_zip_csv(mom_url, "F-F_Momentum_Factor.CSV", skip_lines = 13)

mom <- data.frame(month = as_ym01_from_YYYYMM(mom_raw$Date), UMD = as.numeric(mom_raw[["Mom"]]),
    stringsAsFactors = FALSE)

mom <- mom[!is.na(mom$month), ]

mom <- mom[!duplicated(mom$month), ]

mom <- mom[mom$month >= as.Date(from_date) & mom$month <= as.Date(to_date), ]

ff_factors <- merge(ff5, mom, by = "month", all.x = TRUE, all.y = FALSE)

ff_factors <- ff_factors[order(ff_factors$month), ]

dat <- merge(rets, ff_factors, by = "month", all = FALSE)

dat$Ri_excess_pct <- 100 * dat$AAPL - dat$RF

dat$Rm_excess_pct <- 100 * dat$SPY - dat$RF

dat <- dat[complete.cases(dat), ]

capm_fit <- lm(Ri_excess_pct ~ Rm_excess_pct, data = dat)

ff4_fit <- lm(Ri_excess_pct ~ Rm_excess_pct + SMB + HML + UMD, data = dat)

ff6_fit <- lm(Ri_excess_pct ~ Rm_excess_pct + SMB + HML + RMW + CMA + UMD, data = dat)

print_lm_with_nw(capm_fit, lag = lag_nw, title = "CAPM（AAPL 月度超额收益）")

print_lm_with_nw(ff4_fit, lag = lag_nw, title = "Fama–French 4 因子（含 UMD）")

print_lm_with_nw(ff6_fit, lag = lag_nw, title = "Fama–French 6 因子（5 因子 + UMD）")

nw4

nw6
