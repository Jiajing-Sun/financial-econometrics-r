# 正文来源：CH5-波动率模型.tex，代码块 5；正文第 729 行。
# 只提取章末习题之前的正文代码；原控制台输出未纳入。
# 手动示例：可能依赖前序代码、外部文件、额外R包；参见本章README与manual/index.csv。
# 已移除自动安装、清空工作空间、保存整个工作空间及本机工作目录切换。
dir.create("results/manual", recursive=TRUE, showWarnings=FALSE)
set.seed(123)

invisible(NULL)

library(rugarch)

library(forecast)

library(zoo)

sse_data <- read.csv("data/user/sse.csv", stringsAsFactors = FALSE)

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

spec_gjrgarch <- ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder = c(1, 1)), mean.model = list(armaOrder = c(0, 
    0), include.mean = TRUE))

gjrgarch_fit <- ugarchfit(spec = spec_gjrgarch, data = sse_data$LogReturns)

tgarch_spec <- ugarchspec(variance.model = list(model = "fGARCH", submodel = "TGARCH", garchOrder = c(1, 1)), mean.model = list(armaOrder = c(0, 
    0), include.mean = TRUE))

library(MSwM)

data_df <- data.frame(log_returns = sse_data$LogReturns)

formula <- log_returns ~ 1

model_ms_garch <- msmFit(formula, data = data_df, k = 2, sw = c(TRUE, TRUE))

garch_fit

egarch_fit

gjrgarch_fit

tarch_fit

model_ms_garch
