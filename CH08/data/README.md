# 数据说明

默认为100只股票、24期的模拟输入。以t-1期市值/PB分组，持有t期，落实正文事前信息要求。这里是正文简化等权说明，不等同于官方Fama–French交叉排序/市值加权因子。Ken French 数据库与股票回归示例需手动联网。

CSV字段见 input_schema.csv。默认生成的模拟样本在 results/reference/ 中明确带 SIMULATED 标签，输入参数和随机种子保存在代码中。data/user/ 是被Git忽略的自备数据入口。
