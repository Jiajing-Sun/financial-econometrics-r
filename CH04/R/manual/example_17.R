# 正文来源：CH4-自回归移动平均模型.tex，代码块 17；正文第 1944 行。
# 最新SVAR部分采用已识别AB限制；全章片段仅手动执行，见manual/index.csv。
dir.create("results/manual", recursive=TRUE, showWarnings=FALSE)
for (t in 2:n_samples) {
    y[t] <- y[t - 1] + epsilon[t]
}
