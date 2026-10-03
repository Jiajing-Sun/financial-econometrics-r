# 正文来源：CH5-波动率模型.tex，代码块 5；修订稿第 726 行。
# 仅提取章末习题之前的正文；不含习题提示或答案。
# 语法已检查；未宣称全部外部数据与可选分支已执行。
set.seed(123)

if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
}

library(rugarch)

library(forecast)

library(zoo)

sse_data <- read.csv("sse.csv", stringsAsFactors = FALSE)

sse_data$Date <- as.Date(sse_data$Date)

sse_data$LogReturns <- c(NA, diff(log(sse_data$SSE), lag = 1))

sse_data <- sse_data[-1, ]

arma_fit <- auto.arima(sse_data$LogReturns, seasonal = FALSE, stepwise = FALSE, approximation = FALSE)

summary(arma_fit)

spec_garch <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), mean.model = list(armaOrder = c(0,
    0), include.mean = TRUE))

garch_fit <- ugarchfit(spec = spec_garch, data = sse_data$LogReturns)

spec_egarch <- ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1, 1)), mean.model = list(armaOrder = c(0,
    0), include.mean = TRUE))

egarch_fit <- ugarchfit(spec = spec_egarch, data = sse_data$LogReturns)

spec_gjrgarch <- ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder = c(1, 1)),
    mean.model = list(armaOrder = c(0, 0), include.mean = TRUE))

gjrgarch_fit <- ugarchfit(spec = spec_gjrgarch, data = sse_data$LogReturns)

tgarch_spec <- ugarchspec(variance.model = list(model = "fGARCH", submodel = "TGARCH", garchOrder = c(1,
    1)), mean.model = list(armaOrder = c(0, 0), include.mean = TRUE))

tarch_fit <- ugarchfit(spec = tgarch_spec, data = sse_data$LogReturns)

library(MSGARCH)

r_ms <- as.numeric(na.omit(sse_data$LogReturns))

r_ms <- r_ms - mean(r_ms)

spec_ms <- CreateSpec(variance.spec = list(model = c("sGARCH", "sGARCH")), distribution.spec = list(distribution = c("norm",
    "norm")), switch.spec = list(do.mix = FALSE))

set.seed(123)

model_ms_garch <- FitML(spec = spec_ms, data = r_ms)

garch_fit

egarch_fit

gjrgarch_fit

tarch_fit

model_ms_garch
