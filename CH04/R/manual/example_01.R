# 正文来源：CH4-自回归移动平均模型.tex，代码块 1；修订稿第 178 行。
# 仅提取章末习题之前的正文；不含习题提示或答案。
# 语法已检查；未宣称全部外部数据与可选分支已执行。
library(ggplot2)

library(ragg)

if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
}

set.seed(1386)

T <- 1000

N <- 50

t_point <- 100

Y <- numeric(T)

for (t in 2:T) {
    Y[t] <- Y[t - 1] + rnorm(1)
}

mu_hat <- mean(Y)

sigma_hat_sq <- var(Y)

single_df <- data.frame(time = seq_len(T), value = Y)

random_walks <- matrix(nrow = N, ncol = T)

for (i in 1:N) {
    epsilon <- rnorm(T)
    random_walks[i, ] <- cumsum(epsilon)
}

multi_df <- data.frame(time = rep(seq_len(T), times = N), path = factor(rep(seq_len(N), each = T)),
    value = as.vector(t(random_walks)))

theme_book <- function() {
    theme_bw(base_size = 18, base_family = "Arial Unicode MS") + theme(plot.title = element_blank(),
        panel.grid.minor = element_blank(), axis.title = element_text(size = 20), axis.text = element_text(size = 17,
            colour = "black"))
}

p_single <- ggplot(single_df, aes(x = time, y = value)) + geom_line(colour = "#1f77b4", linewidth = 0.7) +
    labs(x = "时间", y = "随机游走取值") + theme_book()

ggsave("single_realization.png", p_single, width = 6, height = 4, dpi = 300, device = ragg::agg_png)

p_multi <- ggplot(multi_df, aes(x = time, y = value, group = path, colour = path)) + geom_line(linewidth = 0.35,
    alpha = 0.65, show.legend = FALSE) + scale_colour_manual(values = hcl.colors(N, palette = "Dark 3")) +
    labs(x = "时间", y = "随机游走取值") + theme_book()

ggsave("50_random_walks_plot.png", p_multi, width = 6, height = 4, dpi = 300, device = ragg::agg_png)

ensemble_average_t100 <- mean(random_walks[, t_point])

ensemble_variance_t100 <- var(random_walks[, t_point])

cat(sprintf("mu_hat: %.3f\n", mu_hat))

cat(sprintf("sigma_hat_sq: %.3f\n", sigma_hat_sq))

cat(sprintf("ensemble_average_t100: %.3f\n", ensemble_average_t100))

cat(sprintf("ensemble_variance_t100: %.3f\n", ensemble_variance_t100))
