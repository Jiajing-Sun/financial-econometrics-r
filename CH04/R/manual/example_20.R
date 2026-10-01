# 正文来源：CH4-自回归移动平均模型.tex，代码块 20；正文第 2477 行。
# 最新SVAR部分采用已识别AB限制；全章片段仅手动执行，见manual/index.csv。
dir.create("results/manual", recursive=TRUE, showWarnings=FALSE)
mean <- mean(sse$Log_Returns)

ss_total <- sum((test_data - mean_train)^2)

ss_residual <- sum((test_data - forecasts$mean)^2)

R_OOS_2 <- 1 - (ss_residual/ss_total)

print(paste("Out-of-sample R^2:", R_OOS_2))
