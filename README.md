# 金融计量经济学：正文 R 示例

《金融计量经济学：理论、案例与R语言》（孙佳婧、洪永淼、Oliver Linton）的正文配套代码。每章分别保存 R 代码、数据说明和正文演示结果。

**公开范围仅限正文示例及其数据。习题解答代码、hints、教师答案、书稿、课件和答案PDF不在本仓库。**

## 快速运行

需要R（正文部分示例使用 `|>`，建议R 4.1或更高）。默认入口只运行明确标注的离线正文演示，不下载数据或自动安装包。

```bash
Rscript CH03/run.R
Rscript run_all.R
Rscript run_all.R --chapters=1,3,7
```

第12章默认例子使用R推荐包 `rpart`。缺少时请在R中自行运行 `install.packages("rpart")`。其他完整正文片段按各章README安装所需包，不必为快速入口安装全部依赖。

第6章没有预置贵州茅台行情，默认只检查接口并清楚记录未运行实证。实际检验入口：

```bash
Rscript CH06/run.R --data=CH06/data/user/Moutai.csv
```

## 目录

每章包含：

- `R/body_demo.R` 与 `run.R`：已执行核验的独立正文演示。
- `R/manual/`：仅来自章末习题之前的完整正文片段。`index.csv`列原章节、代码块与前置条件。全部做语法检查，未宣称逐段运行或全面验证。
- `data/`：公开正文输入、字段与来源；`data/user/`用于自备文件且被Git忽略。
- `results/reference/`：本仓库正文演示实际生成的结果；模拟与历史样本明确区分。
- `results/current/`、`results/user/`：用户运行输出，被Git忽略。

| 章节 | 默认演示 |
|---|---|
| [01 R语言概述](CH01/README.md) | 循环、函数、e 的数值近似及 mtcars 基础图形 |
| [02 引言和背景](CH02/README.md) | 正文圣彼得堡奖金约定下的截断期望与对数效用 |
| [03 回归模型及其应用](CH03/README.md) | mtcars 公制回归及出版社匿名 A 公司 CAPM |
| [04 自回归移动平均模型](CH04/README.md) | AR(2) 模拟与 Yule–Walker 估计 |
| [05 波动率模型](CH05/README.md) | GARCH(1,1) 条件方差递推模拟 |
| [06 收益可预测性与有效市场假说](CH06/README.md) | 正文价格检验的数据接口；无输入时明确记录待提供数据 |
| [07 非参数方法](CH07/README.md) | 核函数与 R 内置 DAX 历史收益率密度 |
| [08 金融资产定价模型](CH08/README.md) | 正文简化等权 SMB/HML 分组流程的模拟 |
| [09 连续金融模型](CH09/README.md) | 正文标准布朗运动路径模拟 |
| [10 收益率曲线](CH10/README.md) | 正文 Nelson–Siegel 函数的给定参数曲线 |
| [11 风险管理与极值理论](CH11/README.md) | 正文 Hill 尾部估计函数的 Pareto 模拟演示 |
| [12 金融计量经济学与人工智能方法](CH12/README.md) | 正文滚动树模型选股示例的缩小模拟 |

## 数据与复现边界

公开输入包括出版社匿名化 `stock_data.csv`、R内置 `mtcars` 与 `EuStockMarkets`。历史样本不是最新行情；匿名案例不标注真实公司身份。默认模拟均保留种子和参数。没有原始输入时不会用模拟数字冒充市场实证。

WDI、FRED、Ken French、Yahoo、UCI等正文在线案例保留在手动目录。联网片段要求显式设置 `FIN_ECON_ENABLE_NETWORK=1`；数据服务、历史区间、版本、字段和授权条款以实际来源为准。FRED密钥如需使用，只从环境变量读取，禁止写入版本库。

语法和运行记录见 [verification/README.md](verification/README.md)。`manual/index.csv`保留提取定位；正文中的控制台输出及安装指令不作为可执行例子。

## 许可

公开存放不改变原作者及第三方权利。原课件的许可声明与正文代码的许可范围分别说明，详见 [版权与来源说明](LICENSE-NOTICE.txt)；数据与软件各循其来源条款。
