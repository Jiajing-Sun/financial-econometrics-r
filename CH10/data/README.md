# 数据说明

默认参数仅用于展示正文NS函数的形状，没有声称估计实际收益率曲线。FRED DGS系列的原始单位为年化百分数，正文转换为小数；期限、折现因子及复利口径需统一。联网代码只在manual目录。

CSV字段见 input_schema.csv。默认生成的模拟样本在 results/reference/ 中明确带 SIMULATED 标签，输入参数和随机种子保存在代码中。data/user/ 是被Git忽略的自备数据入口。
