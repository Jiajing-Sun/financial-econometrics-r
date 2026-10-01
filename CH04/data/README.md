# 数据说明

默认全部为模拟。手动 SVAR 代码已同步最新修订：A为单位下三角、B为对角矩阵，并进行正对角符号规范。SSE案例需要用户提供原始复权指数CSV。

CSV字段见 input_schema.csv。默认生成的模拟样本在 results/reference/ 中明确带 SIMULATED 标签，输入参数和随机种子保存在代码中。data/user/ 是被Git忽略的自备数据入口。
