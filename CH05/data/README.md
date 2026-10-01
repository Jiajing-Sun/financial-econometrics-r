# 数据说明

默认全部为模拟。正文的 rugarch、mgarchBEKK、rmgarch 等模型估计依赖额外包。sse.csv、stock_indices.csv 等行情须自行按列名与频率整理，不能用模拟序列替代后宣称实证结论。

CSV字段见 input_schema.csv。默认生成的模拟样本在 results/reference/ 中明确带 SIMULATED 标签，输入参数和随机种子保存在代码中。data/user/ 是被Git忽略的自备数据入口。
