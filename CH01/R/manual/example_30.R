# 正文来源：CH1-R语言概述.tex，代码块 30；修订稿第 785 行。
# 仅提取章末习题之前的正文；不含习题提示或答案。
# 语法已检查；未宣称全部外部数据与可选分支已执行。
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
