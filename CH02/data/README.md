# 数据说明

默认是确定性计算，没有真实证券行情。正文股票/债券行情代码需要 quantmod 等依赖及可用的数据服务。

CSV字段见 input_schema.csv。默认生成的模拟样本在 results/reference/ 中明确带 SIMULATED 标签，输入参数和随机种子保存在代码中。data/user/ 是被Git忽略的自备数据入口。
