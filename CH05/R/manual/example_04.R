# 正文来源：CH5-波动率模型.tex，代码块 4；修订稿第 602 行。
# 仅提取章末习题之前的正文；不含习题提示或答案。
# 语法已检查；未宣称全部外部数据与可选分支已执行。
spec <- ugarchspec(variance.model = list(model = "iGARCH", garchOrder = c(1, 1)), mean.model = list(armaOrder = c(0,
    0)), distribution.model = "norm")

fit <- ugarchfit(spec, data)
