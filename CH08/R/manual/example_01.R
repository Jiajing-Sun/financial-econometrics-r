# 正文来源：CH8-金融资产定价模型.tex，代码块 1；正文第 568 行。
# 只提取章末习题之前的正文代码；原控制台输出未纳入。
# 手动示例：可能依赖前序代码、外部文件、额外R包；参见本章README与manual/index.csv。
# 已移除自动安装、清空工作空间、保存整个工作空间及本机工作目录切换。
dir.create("results/manual", recursive=TRUE, showWarnings=FALSE)
if (!identical(Sys.getenv('FIN_ECON_ENABLE_NETWORK'), '1')) stop('此正文案例会联网；确认数据口径后设置 FIN_ECON_ENABLE_NETWORK=1 再手动运行。', call.=FALSE)
invisible(NULL)

invisible(NULL)

library(tidyverse)

library(scales)

library(quantmod)

library(PerformanceAnalytics)

start_date <- ymd("2013-01-01")

end_date <- ymd("2023-12-31")

library(frenchdata)

factors_ff3_daily_raw <- download_french_data("Fama/French 3 Factors [Daily]")

factors_ff3_daily <- filter(rename(rename_with(mutate(factors_ff3_daily_raw$subsets$data[[1]], date = ymd(date), 
    across(c(RF, `Mkt-RF`, SMB, HML), ~as.numeric(.)/100), .keep = "none"), str_to_lower), mkt_excess = `mkt-rf`), 
    date >= start_date & date <= end_date)

write.csv(factors_ff3_daily, "results/manual/factors_ff3_daily.csv")

tickers <- c("NEE", "ENPH", "SEDG", "FSLR", "BEP", "PLUG", "TSLA", "VWDRY")

stock_data <- list()

for (ticker in tickers) {
    stock_data[[ticker]] <- getSymbols(ticker, src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
    stock_data[[ticker]] <- dailyReturn(Cl(stock_data[[ticker]]))
    write.csv(stock_data[[ticker]], file.path("results/manual", paste0(ticker, "_stock_data.csv")))
}

factors_ff3_daily_xts <- xts(factors_ff3_daily[, -1], order.by = factors_ff3_daily$date)

results <- lapply(stock_data, function(stock) {
    merged_data <- merge(stock, factors_ff3_daily_xts, join = "inner")
    fit <- lm(daily.returns ~ mkt_excess + smb + hml, data = merged_data)
    return(summary(fit))
})

names(results) <- names(stock_data)

results$NEE

results$ENPH

results$SEDG

results$FSLR

results$BEP

results$PLUG

results$TSLA

results$VWDRY

for (ticker in tickers) {
    merged_data <- merge(stock_data[[ticker]], factors_ff3_daily_xts, join = "inner")
    fit <- lm(daily.returns ~ mkt_excess + smb + hml, data = merged_data)
    write.csv(merged_data, file.path("results/manual", paste0(ticker, "_combined_data.csv")))
    fit_summary <- capture.output(summary(fit))
    write(fit_summary, file = paste0(ticker, "_regression_output.txt"))
}
