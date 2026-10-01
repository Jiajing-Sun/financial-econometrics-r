# 正文来源：CH12-金融计量经济学与人工智能方法.tex，代码块 7；正文第 1698 行。
# 只提取章末习题之前的正文代码；原控制台输出未纳入。
# 手动示例：可能依赖前序代码、外部文件、额外R包；参见本章README与manual/index.csv。
# 已移除自动安装、清空工作空间、保存整个工作空间及本机工作目录切换。
dir.create("results/manual", recursive=TRUE, showWarnings=FALSE)
if (!identical(Sys.getenv('FIN_ECON_ENABLE_NETWORK'), '1')) stop('此正文案例会联网；确认数据口径后设置 FIN_ECON_ENABLE_NETWORK=1 再手动运行。', call.=FALSE)
pkgs <- c("data.table", "dplyr", "tidyr", "caret", "pROC", "PRROC", "ranger", "nnet", "ggplot2")

for (p in pkgs) if (!requireNamespace(p, quietly = TRUE)) stop("此手动示例缺少依赖；请先参照章节README自行安装。", 
    call. = FALSE)

invisible(lapply(pkgs, library, character.only = TRUE))

set.seed(2025)

uci_url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/statlog/german/german.data"

tmp <- tempfile(fileext = ".data")

uci_ok <- try(utils::download.file(uci_url, tmp, quiet = TRUE), silent = TRUE)

if (!inherits(uci_ok, "try-error")) {
    message("使用 UCI German Credit（german.data）")
    cols <- c("chk_acc", "duration", "cred_hist", "purpose", "credit_amt", "savings", "emp_since", "install_rate", 
        "pers_status_sex", "debtors", "residence_since", "property", "age", "inst_plans", "housing", "num_credits", 
        "job", "num_people_maint", "telephone", "foreign_worker", "class")
    df <- data.table::fread(tmp, header = FALSE, col.names = cols, data.table = FALSE)
    df$bad <- ifelse(df$class == 2, 1L, 0L)
    df$class <- NULL
    num_cols <- c("duration", "credit_amt", "install_rate", "residence_since", "age", "num_credits", "num_people_maint")
    for (cn in num_cols) df[[cn]] <- as.numeric(df[[cn]])
    cat_cols <- setdiff(names(df), c("bad", num_cols))
    for (cn in cat_cols) df[[cn]] <- as.factor(df[[cn]])
} else {
    message("UCI 下载失败，回退到 caret::GermanCredit")
    if (!requireNamespace("caret", quietly = TRUE)) 
        stop("此手动示例缺少依赖；请先参照章节README自行安装。", call. = FALSE)
    data("GermanCredit", package = "caret")
    df <- as.data.frame(caret::GermanCredit)
    df$bad <- ifelse(df$Class == "Bad", 1L, 0L)
    df$Class <- NULL
    names(df) <- make.names(names(df))
}

idx_tr <- caret::createDataPartition(df$bad, p = 0.6, list = FALSE)

train <- df[idx_tr, ]

rest <- df[-idx_tr, ]

idx_va <- caret::createDataPartition(rest$bad, p = 0.5, list = FALSE)

valid <- rest[idx_va, ]

test <- rest[-idx_va, ]

dummy <- caret::dummyVars(~., data = train[, setdiff(names(train), "bad")], fullRank = TRUE)

X_tr <- predict(dummy, newdata = train)

X_va <- predict(dummy, newdata = valid)

X_te <- predict(dummy, newdata = test)

center <- colMeans(X_tr)

scale <- apply(X_tr, 2, sd)

scale[scale == 0 | is.na(scale)] <- 1

X_tr <- sweep(sweep(X_tr, 2, center, "-"), 2, scale, "/")

X_va <- sweep(sweep(X_va, 2, center, "-"), 2, scale, "/")

X_te <- sweep(sweep(X_te, 2, center, "-"), 2, scale, "/")

y_tr <- train$bad

y_va <- valid$bad

y_te <- test$bad

fit_logit <- glm(y_tr ~ ., data = data.frame(y_tr = y_tr, X_tr), family = binomial())

fit_rf <- ranger::ranger(dependent.variable.name = "bad", data = dplyr::bind_cols(bad = factor(y_tr), as.data.frame(X_tr)), 
    probability = TRUE, num.trees = 500, mtry = max(1, floor(sqrt(ncol(X_tr)))), min.node.size = 10, seed = 2025)

set.seed(2025)

fit_nn <- nnet::nnet(x = X_tr, y = y_tr, size = 10, decay = 1e-04, maxit = 200, linout = FALSE, trace = FALSE)

clip01 <- function(p) pmin(pmax(p, 1e-06), 1 - 1e-06)

p_logit_va <- clip01(predict(fit_logit, newdata = as.data.frame(X_va), type = "response"))

p_rf_va <- clip01(predict(fit_rf, data = as.data.frame(X_va))$predictions[, "1"])

p_nn_va <- clip01(predict(fit_nn, newdata = X_va, type = "raw"))

platt_fit <- function(p, y) {
    z <- qlogis(clip01(p))
    glm(y ~ z, family = binomial())
}

platt_pred <- function(p, mod) {
    z <- qlogis(clip01(p))
    as.numeric(plogis(cbind(1, z) %*% coef(mod)))
}

cal_logit <- platt_fit(p_logit_va, y_va)

cal_rf <- platt_fit(p_rf_va, y_va)

cal_nn <- platt_fit(p_nn_va, y_va)

iso_cal <- function(p, y, bins = 20) {
    dt <- dplyr::mutate(dplyr::arrange(data.frame(p = p, y = y), p), bin = cut_number(p, bins))
    map <- dplyr::arrange(dplyr::summarise(dplyr::group_by(dt, bin), px = mean(p), py = mean(y), .groups = "drop"), 
        px)
    function(pnew) {
        approx(x = map$px, y = map$py, xout = pnew, rule = 2)$y
    }
}

p_logit_te_raw <- clip01(predict(fit_logit, newdata = as.data.frame(X_te), type = "response"))

p_rf_te_raw <- clip01(predict(fit_rf, data = as.data.frame(X_te))$predictions[, "1"])

p_nn_te_raw <- clip01(predict(fit_nn, newdata = X_te, type = "raw"))

p_logit_te <- platt_pred(p_logit_te_raw, cal_logit)

p_rf_te <- platt_pred(p_rf_te_raw, cal_rf)

p_nn_te <- platt_pred(p_nn_te_raw, cal_nn)

ks_from_roc <- function(y, p) {
    roc <- pROC::roc(y, p, quiet = TRUE, direction = "<")
    coords <- pROC::coords(roc, x = "all", ret = c("sensitivity", "specificity"))
    max(coords["sensitivity", ] - (1 - coords["specificity", ]))
}

brier <- function(y, p) mean((y - p)^2)

report_model <- function(name, y, p) {
    roc <- pROC::roc(y, p, quiet = TRUE, direction = "<")
    auc <- as.numeric(pROC::auc(roc))
    ks <- ks_from_roc(y, p)
    pr <- try(PRROC::pr.curve(scores.class0 = p[y == 1], scores.class1 = p[y == 0])$auc.integral, silent = TRUE)
    if (inherits(pr, "try-error")) 
        pr <- NA_real_
    cat(sprintf("[%s]  AUC=%.4f  KS=%.4f  PR-AUC=%.4f  Brier=%.4f\n", name, auc, ks, pr, brier(y, p)))
    invisible(list(auc = auc, ks = ks, pr_auc = pr, brier = brier(y, p)))
}

cat("== 测试集（校准后）==\n")

m1 <- report_model("Logit", y_te, p_logit_te)

m2 <- report_model("RF   ", y_te, p_rf_te)

m3 <- report_model("NN   ", y_te, p_nn_te)

c_fp <- 1

c_fn <- 5

tau_star <- c_fp/(c_fp + c_fn)

cmat <- function(y, p, tau) {
    pd <- ifelse(p >= tau, 1L, 0L)
    table(Pred = pd, True = y)
}

cat(sprintf("\n成本敏感阈值 tau*=%.3f 下的混淆矩阵（Logit）\n", tau_star))

print(cmat(y_te, p_logit_te, tau_star))

cal_curve <- function(y, p, bins = 10) {
    d <- dplyr::summarise(dplyr::group_by(dplyr::mutate(data.frame(y = y, p = p), bin = cut(p, breaks = quantile(p, 
        probs = seq(0, 1, length.out = bins + 1)), include.lowest = TRUE)), bin), pred = mean(p), obs = mean(y), 
        n = dplyr::n(), .groups = "drop")
    d
}

cal_logit_df <- cal_curve(y_te, p_logit_te, bins = 10)

ggplot(cal_logit_df, aes(pred, obs)) + geom_point() + geom_line() + geom_abline(slope = 1, intercept = 0, linetype = 2) + 
    labs(title = "校准曲线（Logit，测试集）", x = "平均预测概率", y = "实际违约率") + theme_minimal()

psi <- function(x_train, x_test, bins = 10) {
    cuts <- quantile(x_train, probs = seq(0, 1, length.out = bins + 1), na.rm = TRUE)
    cuts[1] <- -Inf
    cuts[length(cuts)] <- Inf
    bt <- table(cut(x_train, cuts))
    be <- table(cut(x_test, cuts))
    pt <- as.numeric(bt)/sum(bt)
    pe <- as.numeric(be)/sum(be)
    sum((pt - pe) * log((pt + 1e-08)/(pe + 1e-08)))
}

psi_score <- psi(clip01(p_logit_te_raw), clip01(p_logit_te))

cat(sprintf("\nPSI（原始Logit概率 vs 校准后概率）：%.4f\n", psi_score))

fair_summary <- function(y, p, age, tau) {
    grp <- ifelse(age < 35, "young", "old")
    df <- data.frame(y = y, p = p, grp = grp)
    auc_y <- as.numeric(pROC::auc(pROC::roc(df$y[df$grp == "young"], df$p[df$grp == "young"], quiet = TRUE)))
    auc_o <- as.numeric(pROC::auc(pROC::roc(df$y[df$grp == "old"], df$p[df$grp == "old"], quiet = TRUE)))
    tpr_y <- with(df[df$grp == "young", ], mean(p >= tau & y == 1)/max(mean(y == 1), 1e-08))
    tpr_o <- with(df[df$grp == "old", ], mean(p >= tau & y == 1)/max(mean(y == 1), 1e-08))
    data.frame(AUC_young = auc_y, AUC_old = auc_o, AUC_gap = auc_o - auc_y, TPR_young = tpr_y, TPR_old = tpr_o, 
        TPR_gap = tpr_o - tpr_y)
}

if ("age" %in% tolower(names(df))) {
    age_col <- names(df)[tolower(names(df)) == "age"][1]
    age_te <- test[[age_col]]
    fs <- fair_summary(y_te, p_logit_te, age_te, tau_star)
    cat("\n按年龄分组的公平性（Logit）：\n")
    print(fs)
} else {
    cat("\n未找到 age 列，跳过年龄分组的公平性示例。\n")
}

cat("\n== 指标小结（测试集，校准后）==\n")

print(rbind(Logit = unlist(m1), RF = unlist(m2), NN = unlist(m3)))
