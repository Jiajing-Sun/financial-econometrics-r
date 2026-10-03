# 正文来源：CH3-回归模型及其应用.tex，代码块 1；修订稿第 511 行。
# 仅提取章末习题之前的正文；不含习题提示或答案。
# 语法已检查；未宣称全部外部数据与可选分支已执行。
data(mtcars)

mtcars_metric <- transform(mtcars, weight_t = wt * 0.45359237, fuel_eff_km_l = mpg * 1.609344/3.785411784)

model <- lm(fuel_eff_km_l ~ weight_t, data = mtcars_metric)

summary(model)

anova(model)
