# 正文来源：CH10-收益率曲线.tex，代码块 1；修订稿第 105 行。
# 仅提取章末习题之前的正文；不含习题提示或答案。
# 语法已检查；未宣称全部外部数据与可选分支已执行。
if (!identical(Sys.getenv('FIN_ECON_ENABLE_NETWORK'),'1')) stop('此正文示例可能联网；请设置 FIN_ECON_ENABLE_NETWORK=1 后手动运行。')
file <- "data/feds200628.csv"

url <- paste0("https://www.federalreserve.gov/data/", "yield-curve-tables/feds200628.csv")

if (!file.exists(file)) {
    download.file(url, file, mode = "wb")
}

txt <- readLines(file, warn = FALSE)

header <- grep("^\"?Date\"?,", txt)[1]

if (is.na(header)) stop("未找到 Date 表头，请检查是否下载了正确的 CSV。")

data <- read.csv(file, skip = header - 1L, na.strings = c("NA", "N/A", ""), check.names = FALSE)

taus <- c(1, 2, 3, 5, 7, 10, 20, 30)

cols <- sprintf("SVENY%02d", taus)

stopifnot(all(c("Date", cols) %in% names(data)))

dates <- as.Date(data$Date, format = "%Y-%m-%d")

if (all(is.na(dates))) dates <- as.Date(data$Date, format = "%d-%m-%Y")

valid <- !is.na(dates) & complete.cases(data[, cols])

stopifnot(any(valid))

i <- which(valid)[which.max(dates[valid])]

y_cc <- as.numeric(unlist(data[i, cols], use.names = FALSE))/100

stopifnot(all(is.finite(y_cc)))

d_tau <- exp(-taus * y_cc)

gfit <- splinefun(c(0, taus), c(0, taus * y_cc), method = "natural")

f_cc <- gfit(taus, deriv = 1)

period_return <- head(d_tau, -1)/tail(d_tau, -1) - 1

forward_simple_annual <- period_return/diff(taus)

print(data.frame(date = dates[i], tau_years = taus, y_cc = y_cc, discount = d_tau, f_cc = f_cc))

print(data.frame(from = head(taus, -1), to = tail(taus, -1), period_return, forward_simple_annual))

op <- par(mfrow = c(1, 3))

plot(taus, 100 * y_cc, type = "b", xlab = "Years", ylab = "% p.a.", main = "Zero-coupon yield")

plot(taus, d_tau, type = "b", xlab = "Years", ylab = "Discount factor")

plot(taus, 100 * f_cc, type = "b", xlab = "Years", ylab = "% p.a.", main = "Interpolated forward")

par(op)
