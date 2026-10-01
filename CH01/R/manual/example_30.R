# 正文来源：CH1-R语言概述.tex，代码块 30；正文第 779 行。
# 只提取章末习题之前的正文代码；原控制台输出未纳入。
# 手动示例：可能依赖前序代码、外部文件、额外R包；参见本章README与manual/index.csv。
# 已移除自动安装、清空工作空间、保存整个工作空间及本机工作目录切换。
dir.create("results/manual", recursive=TRUE, showWarnings=FALSE)
data_set <- data.frame(var1 = c(1, 2, NA, 4, 5), var2 = c(6, NA, 8, 9, 10))

for (col in names(data_set)) {
    if (is.numeric(data_set[[col]])) {
        m <- mean(data_set[[col]], na.rm = TRUE)
        data_set[[col]][is.na(data_set[[col]])] <- m
    }
}

cat("均值插补后的数据：\n")

print(data_set)

data_set <- data.frame(var1 = c(1, 2, NA, 4, 5), var2 = c(6, NA, 8, 9, 10))

for (col in names(data_set)) {
    if (is.numeric(data_set[[col]])) {
        med <- median(data_set[[col]], na.rm = TRUE)
        data_set[[col]][is.na(data_set[[col]])] <- med
    }
}

cat("中位数插补后的数据：\n")

print(data_set)
