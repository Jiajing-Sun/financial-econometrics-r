# 正文来源：CH7-非参数方法.tex，代码块 5；正文第 671 行。
# 只提取章末习题之前的正文代码；原控制台输出未纳入。
# 手动示例：可能依赖前序代码、外部文件、额外R包；参见本章README与manual/index.csv。
# 已移除自动安装、清空工作空间、保存整个工作空间及本机工作目录切换。
dir.create("results/manual", recursive=TRUE, showWarnings=FALSE)
library(ggplot2)

library(ggpubr)

set.seed(123)

x_vals <- seq(-3, 3, length.out = 100)

y_vals <- sin(x_vals) + rnorm(length(x_vals), sd = 0.3)

data <- data.frame(x = x_vals, y = y_vals)

linear_model <- lm(y ~ x, data = data)

loess_model <- loess(y ~ x, data = data)

data$y_pred_linear <- predict(linear_model, newdata = data)

data$y_pred_loess <- predict(loess_model, newdata = data)

data$residuals_linear <- data$y - data$y_pred_linear

data$residuals_loess <- data$y - data$y_pred_loess

plot1 <- ggplot(data, aes(x = x)) + geom_point(aes(y = y), color = "black") + geom_line(aes(y = y_pred_linear), 
    color = "blue", size = 1) + geom_line(aes(y = y_pred_loess), color = "red", size = 1) + labs(title = "Fitted Values", 
    x = "x", y = "y") + theme_minimal() + theme(legend.title = element_blank())

plot2 <- ggplot(data, aes(x = x)) + geom_point(aes(y = residuals_linear), color = "blue") + labs(title = "Residuals of Linear Regression", 
    x = "x", y = "Residuals") + theme_minimal()

plot3 <- ggplot(data, aes(x = x)) + geom_point(aes(y = residuals_loess), color = "red") + labs(title = "Residuals of LOESS", 
    x = "x", y = "Residuals") + theme_minimal()

ggarrange(plot1, plot2, plot3, ncol = 3, nrow = 1)
