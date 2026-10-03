# 正文来源：CH4-自回归移动平均模型.tex，代码块 20；修订稿第 2450 行。
# 仅提取章末习题之前的正文；不含习题提示或答案。
# 语法已检查；未宣称全部外部数据与可选分支已执行。
mean_train <- mean(train_data, na.rm = TRUE)

ss_total <- sum((test_data - mean_train)^2)

ss_residual <- sum((test_data - forecasts$mean)^2)

R_OOS_2 <- 1 - (ss_residual/ss_total)

print(paste("Out-of-sample R^2:", R_OOS_2))
