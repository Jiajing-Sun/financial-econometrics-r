# CH08 金融资产定价模型

本目录只公开教材正文代码与对应数据，不包括习题解答、学生提示或教师答案。

## 运行独立正文演示

从仓库根目录执行：

```bash
Rscript CH08/run.R
```

演示内容：正文简化等权 SMB/HML 分组流程的模拟。默认依赖：R自带包。输出写入 `results/current/`，不会联网或自动安装软件。

如需换数据，请先阅读 `R/body_demo.R` 的输入约定。

## 数据与口径

默认为100只股票、24期的模拟输入。以t-1期市值/PB分组，持有t期，落实正文事前信息要求。这里是正文简化等权说明，不等同于官方Fama–French交叉排序/市值加权因子。Ken French 数据库与股票回归示例需手动联网。

字段见 [data/input_schema.csv](data/input_schema.csv)。用户自行提供的文件放在 `data/user/`；该目录及生成的个人分析结果均被Git忽略。

## 完整正文片段

`R/manual/` 收录 4 个正文代码块，顺序、原章节标题、原正文位置和静态识别的依赖见 [R/manual/index.csv](R/manual/index.csv)。这些片段经过语法检查，但没有全部执行；部分依赖前序对象、模型输出、真实数据和指定频率，不能当作彼此独立的脚本批量运行。控制台输出、安装指令块和章末习题未混入代码。

手动示例的工作目录应设为本章目录。相对输入文件已归入 `data/` 或 `data/user/`，图形输出归入 `results/manual/`。联网片段要求先显式设置 `FIN_ECON_ENABLE_NETWORK=1`，然后逐段运行；应先核对原数据服务、代码中的起止日期与变量单位。

以下为静态识别到的手动示例包，动态构造的包列表仍应以对应脚本为准。只在需要运行相关示例时由用户自行安装；仓库不会执行安装：

```r
install.packages(c('PerformanceAnalytics', 'frenchdata', 'quantmod', 'scales', 'tidyverse'))
```

## 核验范围

本章 `R/body_demo.R` 是从正文相关代码/公式整理的独立入口，新增的路径、输出与确定性检查仅用于复现。结果性质见 `results/reference/scope.txt`（第6章是接口检查）。全仓库语法和离线入口核验见根目录 `verification/`。正文中的完整在线实证、外部软件接口与重型模型未被默认核验覆盖。
