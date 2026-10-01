# 数据说明

EuStockMarkets 来自R datasets，包含1991—1998年欧洲指数的公开历史样本；不是最新行情。内置对象缺少逐行真实交易日期，所以导出文件以observation为索引，不伪造日期。

CSV字段见 input_schema.csv。默认生成的模拟样本在 results/reference/ 中明确带 SIMULATED 标签，输入参数和随机种子保存在代码中。data/user/ 是被Git忽略的自备数据入口。
