# 正文来源：CH4-自回归移动平均模型.tex，代码块 22；正文第 3093 行。
# 最新SVAR部分采用已识别AB限制；全章片段仅手动执行，见manual/index.csv。
dir.create("results/manual", recursive=TRUE, showWarnings=FALSE)
set.seed(124)

irf_ab <- vars::irf(fit_ab, n.ahead = 10, boot = TRUE, runs = 200, ci = 0.95)

plot(irf_ab)
