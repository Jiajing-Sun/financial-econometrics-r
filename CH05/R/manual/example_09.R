# 正文来源：CH5-波动率模型.tex，代码块 9；正文第 1242 行。
# 只提取章末习题之前的正文代码；原控制台输出未纳入。
# 手动示例：可能依赖前序代码、外部文件、额外R包；参见本章README与manual/index.csv。
# 已移除自动安装、清空工作空间、保存整个工作空间及本机工作目录切换。
dir.create("results/manual", recursive=TRUE, showWarnings=FALSE)
invisible(NULL)

invisible(NULL)

if (!requireNamespace("imputeTS", quietly=TRUE)) stop("请手动安装imputeTS；正文缺失值插值使用该包。")

library(rmgarch)

library(ggplot2)

price2return <- function(p) {
    N <- length(p)
    return.series <- (log(p[2:N]) - log(p[1:(N - 1)])) * 100
    return.series <- imputeTS::na_interpolation(return.series)
    return(return.series)
}

temp.dat <- read.csv("data/user/stock_indices.csv")

dat <- temp.dat[setdiff((1:dim(temp.dat)[1]), which(temp.dat$weekdays == "Saturday" | temp.dat$weekdays == "Sunday")), 
    ]

Dow_Jones <- dat$Dow_Jones_Index_Industrial_Average

SP <- dat$S.P.TSX_Composite_Index

Dow_Jones.return <- price2return(Dow_Jones)

SP.return <- price2return(SP)

return_series <- cbind(Dow_Jones.return, SP.return)

arima_dj <- auto.arima(Dow_Jones.return)

arima_sp <- auto.arima(SP.return)

residuals_dj <- residuals(arima_dj)

residuals_sp <- residuals(arima_sp)

ut <- cbind(residuals_dj, residuals_sp)

garch11.spec = ugarchspec(mean.model = list(armaOrder = c(0, 0), include.mean = FALSE), variance.model = list(garchOrder = c(1, 
    1), model = "sGARCH"), distribution.model = "norm")

dcc.garch11.spec = dccspec(uspec = multispec(replicate(2, garch11.spec)), dccOrder = c(1, 1), distribution = "mvnorm")

dcc.fit = dccfit(dcc.garch11.spec, data = ut)

dcc.fit

dynamic_correlation <- rcor(dcc.fit)

off_diagonal_correlation <- dynamic_correlation[2, 1, ]

dynamic_corr_df <- data.frame(time = as.Date(dimnames(dynamic_correlation)[[3]]), correlation = off_diagonal_correlation)

ggplot(dynamic_corr_df, aes(x = time, y = correlation)) + geom_line(color = "blue") + labs(title = "Dynamic Conditional Correlation (Off-Diagonal) ", 
    x = "Time", y = "Correlation") + theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
