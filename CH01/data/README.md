# 数据说明

mtcars 来自 R datasets；原始 mpg 为英里/美制加仑，wt 为1000磅。正文 WDI/FRED/OECD/IMF 接口仅放在手动示例，需分别核对可用指标及频率。FRED 使用用户环境变量 FRED_API_KEY，不在仓库保存密钥。

CSV字段见 input_schema.csv。默认生成的模拟样本在 results/reference/ 中明确带 SIMULATED 标签，输入参数和随机种子保存在代码中。data/user/ 是被Git忽略的自备数据入口。
