# 正文来源：CH12-金融计量经济学与人工智能方法.tex，代码块 5；正文第 1352 行。
# 只提取章末习题之前的正文代码；原控制台输出未纳入。
# 手动示例：可能依赖前序代码、外部文件、额外R包；参见本章README与manual/index.csv。
# 已移除自动安装、清空工作空间、保存整个工作空间及本机工作目录切换。
dir.create("results/manual", recursive=TRUE, showWarnings=FALSE)
pred_cols <- c("OLS", "RIDGE", "XGB", "LSTM")

avail <- sapply(pred_cols, function(cn) sum(!is.na(oos[[cn]])))

keep_models <- names(avail[avail > 0])

metric <- function(pred) {
    ok <- !is.na(pred) & !is.na(oos$y)
    if (!any(ok)) 
        return(c(RMSE = NA, DirAcc = NA))
    rmse <- sqrt(mean((oos$y[ok] - pred[ok])^2))
    dir <- mean(sign(oos$y[ok]) == sign(pred[ok]))
    c(RMSE = rmse, DirAcc = dir)
}

res_tab <- do.call(rbind, lapply(keep_models, function(m) metric(oos[[m]])))

rownames(res_tab) <- keep_models

print(round(res_tab, 4))

nav_list <- list()

sum_list <- list()

for (mdl in keep_models) {
    for (tc in bp_set) {
        nav <- make_nav(oos[[mdl]], tc)
        nav$Model <- mdl
        nav$TC <- paste0(tc * 10000, " bp")
        nav_list[[length(nav_list) + 1]] <- nav
        s <- round(summ(nav), 4)
        sum_list[[length(sum_list) + 1]] <- tibble(Model = mdl, TC = paste0(tc * 10000, " bp"), AnnRet = s[["AnnRet"]], 
            AnnVol = s[["AnnVol"]], Sharpe = s[["Sharpe"]])
    }
}

nav_all <- dplyr::bind_rows(nav_list)

sum_all <- dplyr::bind_rows(sum_list)

print(sum_all)
