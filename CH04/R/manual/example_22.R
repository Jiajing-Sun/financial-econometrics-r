# 正文来源：CH4-自回归移动平均模型.tex，代码块 22；修订稿第 3060 行。
# 仅提取章末习题之前的正文；不含习题提示或答案。
# 语法已检查；未宣称全部外部数据与可选分支已执行。
set.seed(124)

irf_ab <- vars::irf(fit_ab, n.ahead = 10, boot = TRUE, runs = 200, ci = 0.95)

plot(irf_ab)
