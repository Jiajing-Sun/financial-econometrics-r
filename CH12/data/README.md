# 数据说明

默认将正文模拟缩小为40只股票、42个月，滚动训练24个月；该示例只演示按时间留出。其他正文RF/XGBoost/LSTM、信用分类与投资组合代码需要额外包、外部数据或运行时环境，未在默认入口中执行。

CSV字段见 input_schema.csv。默认生成的模拟样本在 results/reference/ 中明确带 SIMULATED 标签，输入参数和随机种子保存在代码中。data/user/ 是被Git忽略的自备数据入口。
