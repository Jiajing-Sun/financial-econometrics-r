# 正文来源：CH10-收益率曲线.tex，代码块 6；修订稿第 1101 行。
# 仅提取章末习题之前的正文；不含习题提示或答案。
# 语法已检查；未宣称全部外部数据与可选分支已执行。
if (!identical(Sys.getenv('FIN_ECON_ENABLE_NETWORK'),'1')) stop('此正文示例可能联网；请设置 FIN_ECON_ENABLE_NETWORK=1 后手动运行。')
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
}

suppressPackageStartupMessages({
    library(quantmod)
    library(zoo)
    library(moments)
    library(ggplot2)
})

mats_month <- c(1, 3, 6, 12, 24, 36, 60, 120, 240, 360)

fred_codes <- c("DGS1MO", "DGS3MO", "DGS6MO", "DGS1", "DGS2", "DGS3", "DGS5", "DGS10", "DGS20",
    "DGS30")

from <- as.Date("2000-01-01")

to <- as.Date("2024-12-31")

getSymbols(fred_codes, src = "FRED", from = from, to = to, auto.assign = TRUE)

Y <- do.call(merge, mget(fred_codes))

colnames(Y) <- paste0(mats_month, "M")

Ynum <- Y/100

stats_one <- function(x) {
    x <- na.omit(as.numeric(x))
    if (length(x) < 10)
        return(c(m = NA, s = NA, k3 = NA, k4 = NA))
    c(m = mean(x, na.rm = TRUE) * 100, s = sd(x, na.rm = TRUE) * 100, k3 = skewness(x, na.rm = TRUE),
        k4 = kurtosis(x, na.rm = TRUE) - 3)
}

tab1 <- t(apply(Ynum, 2, stats_one))

tab1 <- as.data.frame(tab1)

tab1$Maturity <- rownames(tab1)

tab1 <- tab1[, c("Maturity", "m", "s", "k3", "k4")]

rownames(tab1) <- NULL

acf_one <- function(x, maxlag = 5) {
    x <- diff(na.omit(as.numeric(x)))
    if (length(x) < (maxlag + 10))
        return(rep(NA, maxlag))
    acf(x, plot = FALSE, na.action = na.pass)$acf[2:(maxlag + 1)]
}

acf_mat <- t(apply(Ynum, 2, acf_one, maxlag = 5))

colnames(acf_mat) <- paste0("rho_D(", 1:5, ")")

tab2 <- as.data.frame(acf_mat)

tab2$Maturity <- rownames(acf_mat)

tab2 <- tab2[, c("Maturity", colnames(acf_mat))]

rownames(tab2) <- NULL

cat("=== Table 13.1  日频收益率的描述统计（2000-2024） ===\n")

print(within(tab1, {
    m = round(m, 4)
    s = round(s, 4)
    k3 = round(k3, 4)
    k4 = round(k4, 4)
}), row.names = FALSE)

cat("\n=== Table 13.2  差分收益率的自相关（1-5阶，2000-2024） ===\n")

print(within(tab2, {
    `rho_D(1)` = round(`rho_D(1)`, 4)
    `rho_D(2)` = round(`rho_D(2)`, 4)
    `rho_D(3)` = round(`rho_D(3)`, 4)
    `rho_D(4)` = round(`rho_D(4)`, 4)
    `rho_D(5)` = round(`rho_D(5)`, 4)
}), row.names = FALSE)

Ywin <- window(Y, start = from, end = to)

theme_book <- function() {
    theme_bw(base_size = 22) + theme(plot.title = element_text(hjust = 0.5, size = 25, face = "bold"),
        axis.title = element_text(size = 23), axis.text = element_text(size = 19, colour = "black"),
        legend.position = "bottom", legend.title = element_blank(), legend.text = element_text(size = 20),
        legend.key.width = unit(2.5, "cm"), panel.grid.minor = element_blank(), panel.grid.major = element_line(colour = "grey88",
            linewidth = 0.35), panel.border = element_rect(colour = "black", linewidth = 0.55))
}

save_book_plot <- function(plot, filename) {
    ggsave(filename, plot = plot, width = 9.2, height = 5.4, dpi = 300, device = ragg::agg_png,
        bg = "white")
}

yield_plot_data <- rbind(data.frame(date = index(Ywin), yield = as.numeric(Ywin[, "1M"]), maturity = "1个月期"),
    data.frame(date = index(Ywin), yield = as.numeric(Ywin[, "120M"]), maturity = "120个月期"))

yield_plot_data <- yield_plot_data[is.finite(yield_plot_data$yield), ]

yield_plot_data$maturity <- factor(yield_plot_data$maturity, levels = c("1个月期", "120个月期"))

p_yield <- ggplot(yield_plot_data, aes(x = date, y = yield, colour = maturity, linetype = maturity)) +
    geom_hline(yintercept = 0, colour = "grey75", linetype = "dotted", linewidth = 0.45) +
    geom_line(linewidth = 0.85) + scale_colour_manual(values = c(`1个月期` = "#2C6BB2",
    `120个月期` = "#D65F2E")) + scale_linetype_manual(values = c(`1个月期` = "solid",
    `120个月期` = "longdash")) + scale_x_date(breaks = seq(as.Date("2000-01-01"), as.Date("2025-01-01"),
    by = "5 years"), date_labels = "%Y", expand = expansion(mult = c(0.01, 0.01))) + labs(title = "1个月期与120个月期收益率（2000—2024年）",
    x = "年份", y = "收益率/%") + guides(colour = guide_legend(nrow = 1, byrow = TRUE),
    linetype = guide_legend(nrow = 1, byrow = TRUE)) + theme_book()

save_book_plot(p_yield, "fig_yields_1M_120M_2000_2024.png")

Dy1 <- diff(Ywin[, "1M"])

dy_plot_data <- data.frame(date = index(Dy1), dy = as.numeric(Dy1))

dy_plot_data <- dy_plot_data[is.finite(dy_plot_data$dy), ]

p_dy <- ggplot(dy_plot_data, aes(x = date, y = dy)) + geom_hline(yintercept = 0, colour = "grey70",
    linetype = "dotted", linewidth = 0.45) + geom_col(fill = "#2C6BB2", colour = "#2C6BB2",
    width = 1, alpha = 0.85) + scale_x_date(breaks = seq(as.Date("2000-01-01"), as.Date("2025-01-01"),
    by = "5 years"), date_labels = "%Y", expand = expansion(mult = c(0.01, 0.01))) + labs(title = "1个月期收益率日变化（2000—2024年）",
    x = "年份", y = "日变化/百分点") + theme_book() + theme(legend.position = "none")

save_book_plot(p_dy, "fig_dyield_1M_2000_2024.png")
