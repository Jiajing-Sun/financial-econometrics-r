# 正文来源：CH3-回归模型及其应用.tex，代码块 1；正文第 825 行。
# 只提取章末习题之前的正文代码；原控制台输出未纳入。
# 手动示例：可能依赖前序代码、外部文件、额外R包；参见本章README与manual/index.csv。
# 已移除自动安装、清空工作空间、保存整个工作空间及本机工作目录切换。
dir.create("results/manual", recursive=TRUE, showWarnings=FALSE)
data(mtcars)

mtcars_metric <- transform(mtcars, weight_t = wt * 0.45359237, fuel_eff_km_l = mpg * 1.609344/3.785411784)

model <- lm(fuel_eff_km_l ~ weight_t, data = mtcars_metric)

summary(model)

anova(model)
