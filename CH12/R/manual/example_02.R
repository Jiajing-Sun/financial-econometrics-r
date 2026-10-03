# 正文来源：CH12-金融计量经济学与人工智能方法.tex，代码块 2；修订稿第 274 行。
# 仅提取章末习题之前的正文；不含习题提示或答案。
# 语法已检查；未宣称全部外部数据与可选分支已执行。
library(ISLR)

library(dplyr)

library(ggplot2)

library(rpart)

library(rpart.plot)

library(pROC)

data(Default)

df <- mutate(as_tibble(Default), default = factor(default, levels = c("No", "Yes")))

set.seed(42)

idx <- sample.int(nrow(df), size = 0.7 * nrow(df))

train <- df[idx, ]

test <- df[-idx, ]

fit <- rpart(default ~ balance + income + student, data = train, method = "class", control = rpart.control(cp = 0,
    maxdepth = 4, minsplit = 100, minbucket = 50))

rpart.plot(fit, type = 2, extra = 104, under = TRUE)

test$prob <- predict(fit, newdata = test, type = "prob")[, "Yes"]

roc_obj <- pROC::roc(response = test$default, predictor = test$prob, levels = c("No", "Yes"),
    direction = "<", quiet = TRUE)

auc_val <- as.numeric(pROC::auc(roc_obj))

print(auc_val)

roc_df <- tibble(tpr = roc_obj$sensitivities, fpr = 1 - roc_obj$specificities)

ks_val <- max(roc_df$tpr - roc_df$fpr, na.rm = TRUE)

print(ks_val)

coords_best <- pROC::coords(roc_obj, "best", best.method = "youden", ret = c("threshold", "sensitivity",
    "specificity"))

thr1 <- as.numeric(coords_best["threshold"])

test <- mutate(test, pred = factor(ifelse(prob >= thr1, "Yes", "No"), levels = c("No", "Yes")))

print(table(Predicted = test$pred, Actual = test$default))

cal <- summarise(group_by(mutate(test, bin = ntile(prob, 10)), bin), pred = mean(prob), obs = mean(default ==
    "Yes"), .groups = "drop")

ggplot(cal, aes(pred, obs)) + geom_point() + geom_line() + geom_abline(slope = 1, intercept = 0,
    linetype = 2) + labs(title = "概率校准曲线（十分位）", x = "平均预测违约概率",
    y = "实际违约率")

profit_good <- 100

loss_bad <- -500

grid <- mutate(tibble(thr = seq(0, 1, by = 0.01)), exp_profit = sapply(thr, function(th) {
    approve <- test$prob < th
    p <- test$prob[approve]
    if (length(p) == 0)
        return(-Inf)
    mean((1 - p) * profit_good + p * loss_bad)
}))

thr2 <- grid$thr[which.max(grid$exp_profit)]

print(thr2)

approve2 <- test$prob < thr2

cat("放款率：", mean(approve2), "\n", "单笔期望利润：", mean((1 - test$prob[approve2]) *
    profit_good + test$prob[approve2] * loss_bad), "\n")
