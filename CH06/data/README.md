# 数据说明

默认不会生成虚构的贵州茅台结果。请提供日期唯一、价格为正的date,close CSV，注明复权方式、来源与截止日；正文原片段使用Moutai.csv及原供应商列名，独立入口统一为date,close。有vrtest时额外运行自动方差比bootstrap。未拒绝自相关限制不等于证明弱式有效。

CSV字段见 input_schema.csv。默认生成的模拟样本在 results/reference/ 中明确带 SIMULATED 标签，输入参数和随机种子保存在代码中。data/user/ 是被Git忽略的自备数据入口。
