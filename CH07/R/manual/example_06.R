# 正文来源：CH7-非参数方法.tex，代码块 6；修订稿第 786 行。
# 仅提取章末习题之前的正文；不含习题提示或答案。
# 语法已检查；未宣称全部外部数据与可选分支已执行。
library(KernSmooth)

library(ggplot2)

library(ragg)

library(showtext)

library(sysfonts)

font_add(family = "ArialCN", regular = "/Library/Fonts/Arial Unicode.ttf")

showtext_auto(enable = TRUE)

data(mtcars)

x <- mtcars$hp * 0.7457

y <- mtcars$mpg * 1.609344/3.785411784

bw <- dpill(x, y)

fit <- locpoly(x, y, bandwidth = bw, degree = 0, kernel = "normal", gridsize = 100)

df <- data.frame(power_kw = x, fuel_eff_km_l = y)

fit_df <- data.frame(power_kw = fit$x, fuel_eff_km_l = fit$y)

p <- ggplot(df, aes(x = power_kw, y = fuel_eff_km_l)) + geom_point(size = 2.6, color = "black") +
    geom_line(data = fit_df, color = "blue", linewidth = 1.2) + labs(title = "Nadaraya-Watson估计",
    x = "功率（kW）", y = "燃油效率（千米/升）") + theme_bw(base_family = "ArialCN",
    base_size = 22) + theme(plot.title = element_text(hjust = 0.5, size = 25, face = "bold"),
    axis.title = element_text(size = 23, color = "black"), axis.text = element_text(size = 19,
        color = "black"), panel.grid.minor = element_blank(), panel.grid.major = element_line(color = "grey88",
        linewidth = 0.35), plot.margin = margin(10, 16, 8, 12))

print(p)

ggsave("Nadaraya_Watson_Estimation.png", p, width = 7.3, height = 4.8, dpi = 220, device = ragg::agg_png)
