# 正文来源：CH7-非参数方法.tex，代码块 2；修订稿第 123 行。
# 仅提取章末习题之前的正文；不含习题提示或答案。
# 语法已检查；未宣称全部外部数据与可选分支已执行。
library(ggplot2)

library(dplyr)

library(ragg)

library(showtext)

library(sysfonts)

font_add(family = "ArialCN", regular = "/Library/Fonts/Arial Unicode.ttf")

showtext_auto(enable = TRUE)

x_vals <- seq(-3, 3, by = 0.01)

df_uniform <- data.frame(x = x_vals, y = func_uniform(x_vals), kernel = "均匀核")

df_gaussian <- data.frame(x = x_vals, y = func_gaussian(x_vals), kernel = "高斯核")

df_e <- data.frame(x = x_vals, y = func_e(x_vals), kernel = "Epanechnikov 核")

df_quartic <- data.frame(x = x_vals, y = func_quartic(x_vals), kernel = "四次方核")

df_kernels <- bind_rows(df_uniform, df_gaussian, df_e, df_quartic)

df_kernels$kernel <- factor(df_kernels$kernel, levels = c("均匀核", "高斯核", "Epanechnikov 核",
    "四次方核"))

p <- ggplot(df_kernels, aes(x = x, y = y, color = kernel, linetype = kernel)) + geom_line(linewidth = 1.15) +
    labs(title = "常见核函数", x = "自变量 u", y = "密度值 K(u)", color = "核函数",
        linetype = "核函数") + scale_color_manual(values = c(均匀核 = "blue", 高斯核 = "red",
    `Epanechnikov 核` = "darkgreen", 四次方核 = "purple")) + scale_linetype_manual(values = c(均匀核 = "solid",
    高斯核 = "longdash", `Epanechnikov 核` = "dotdash", 四次方核 = "dashed")) + coord_cartesian(xlim = c(-3,
    3), ylim = c(0, 0.98), expand = FALSE) + theme_bw(base_family = "ArialCN", base_size = 22) +
    theme(plot.title = element_text(hjust = 0.5, size = 25, face = "bold"), axis.title = element_text(size = 23,
        color = "black"), axis.text = element_text(size = 19, color = "black"), legend.position = "bottom",
        legend.title = element_text(size = 20, color = "black"), legend.text = element_text(size = 19,
            color = "black"), panel.grid.minor = element_blank()) + guides(color = guide_legend(nrow = 2,
    byrow = TRUE), linetype = guide_legend(nrow = 2, byrow = TRUE))

print(p)

ggsave("kernel-functions.png", p, width = 8.2, height = 4.8, dpi = 220, device = ragg::agg_png)
