# 正文来源：CH4-自回归移动平均模型.tex，代码块 18；修订稿第 2305 行。
# 仅提取章末习题之前的正文；不含习题提示或答案。
# 语法已检查；未宣称全部外部数据与可选分支已执行。
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable() && !is.null(rstudioapi::getActiveDocumentContext()$path)) {
}

if (!requireNamespace("ggplot2", quietly = TRUE)) NULL

if (!requireNamespace("forecast", quietly = TRUE)) NULL

if (!requireNamespace("tseries", quietly = TRUE)) NULL

if (!requireNamespace("dplyr", quietly = TRUE)) NULL

library(ggplot2)

library(forecast)

library(tseries)

library(dplyr)

sse <- read.csv("sse.csv", stringsAsFactors = FALSE)

sse$Date <- as.Date(sse$Date)

sse <- sse[order(sse$Date), ]

sse_plot <- ggplot(sse, aes(x = Date, y = SSE)) + geom_line(color = "steelblue", linewidth = 0.5) +
    labs(title = "SSE Index Over Time", x = "Date", y = "SSE Index") + theme_minimal(base_size = 12)

print(sse_plot)

ggsave("sse_index_plot.png", plot = sse_plot, width = 8, height = 4.5, dpi = 300)

cat("指数折线图已保存：", normalizePath("sse_index_plot.png"), "\n")

sse$Log_Returns <- c(NA, diff(log(sse$SSE)))

sse <- na.omit(sse)

split_index <- floor(0.95 * nrow(sse))

train_data <- sse$Log_Returns[1:split_index]

test_data <- sse$Log_Returns[(split_index + 1):nrow(sse)]

train_date <- sse$Date[1:split_index]

test_date <- sse$Date[(split_index + 1):nrow(sse)]

png("sse_acf_pacf.png", width = 8, height = 4.5, units = "in", res = 300)

par(mfrow = c(1, 2), mar = c(4, 4, 3, 1) + 0.1)

Acf(train_data, main = "ACF of SSE Log Returns (Train)", lag.max = 40)

Pacf(train_data, main = "PACF of SSE Log Returns (Train)", lag.max = 40)

dev.off()

cat("ACF/PACF 图已保存：", normalizePath("sse_acf_pacf.png"), "\n")

set.seed(123)

model <- auto.arima(train_data)

cat("\n===== ARIMA(Train) 摘要 =====\n")

print(summary(model))

forecasts <- forecast(model, h = length(test_data))

train_df <- data.frame(Date = train_date, Value = as.numeric(train_data))

fc_df <- data.frame(Date = test_date, Forecast = as.numeric(forecasts$mean), Lower = as.numeric(forecasts$lower[,
    "80%"]), Upper = as.numeric(forecasts$upper[, "80%"]))

actual_df <- data.frame(Date = test_date, Actual = as.numeric(test_data))

final_plot <- ggplot() + geom_line(data = train_df, aes(x = Date, y = Value), color = "black",
    linewidth = 0.5) + geom_line(data = fc_df, aes(x = Date, y = Forecast), color = "steelblue",
    linewidth = 0.6) + geom_ribbon(data = fc_df, aes(x = Date, ymin = Lower, ymax = Upper),
    fill = "steelblue", alpha = 0.2) + geom_point(data = actual_df, aes(x = Date, y = Actual),
    color = "red", size = 1.2) + scale_x_date(date_breaks = "3 month", date_labels = "%Y-%m") +
    labs(title = "SSE Log Returns: Train, Forecast and Actual", x = "Date", y = "Log Returns") +
    theme_minimal(base_size = 12)

print(final_plot)

ggsave("sse_forecast_plot.png", plot = final_plot, width = 8, height = 4.5, dpi = 300)

cat("预测对比图已保存：", normalizePath("sse_forecast_plot.png"), "\n")
