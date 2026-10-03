# 正文来源：CH12-金融计量经济学与人工智能方法.tex，代码块 5；修订稿第 1349 行。
# 仅提取章末习题之前的正文；不含习题提示或答案。
# 语法已检查；未宣称全部外部数据与可选分支已执行。
if (!identical(Sys.getenv('FIN_ECON_ENABLE_NETWORK'),'1')) stop('此正文示例可能联网；请设置 FIN_ECON_ENABLE_NETWORK=1 后手动运行。')
library(quantmod)

set.seed(2026)

run_xgb <- FALSE

cache <- "data/SPY_monthly_2000_2024.csv"

if (!file.exists(cache)) {
    px <- Ad(getSymbols("SPY", from = "2000-01-01", to = "2025-01-01", auto.assign = FALSE))
    ep <- endpoints(px, on = "months")
    px <- px[ep[ep > 0]]
    write.csv(data.frame(date = as.Date(index(px)), price = as.numeric(px)), cache, row.names = FALSE)
}

z <- read.csv(cache)

z$date <- as.Date(z$date)

stopifnot(all(diff(z$date) > 0), all(z$price > 0))

r <- diff(log(z$price))

dates <- z$date[-1]

k <- 12:(length(r) - 1)

X <- t(vapply(k, function(t) c(r[t], sum(r[(t - 5):t]), sum(r[(t - 11):t]), sd(r[(t - 2):t])),
    numeric(4)))

colnames(X) <- c("r1", "mom6", "mom12", "vol3")

y <- r[k + 1]

target_date <- dates[k + 1]

fit_scale <- function(x) list(mu = colMeans(x), sd = pmax(apply(x, 2, sd), 1e-08))

transform_x <- function(x, s) sweep(sweep(x, 2, s$mu, "-"), 2, s$sd, "/")

ridge_predict <- function(x, y, xt, lambda) {
    A <- cbind(1, x)
    At <- cbind(1, xt)
    penalty <- diag(c(0, rep(lambda, ncol(x))))
    if (lambda == 0)
        b <- qr.solve(A, y)
    else b <- solve(crossprod(A)/nrow(A) + penalty, crossprod(A, y)/nrow(A))
    as.numeric(At %*% b)
}

win <- 120

nv <- 24

lambdas <- 10^seq(-4, 1, length.out = 12)

stopifnot(nrow(X) > win)

pred <- matrix(NA_real_, nrow(X), 3, dimnames = list(NULL, c("OLS", "RIDGE", "XGB")))

for (i in (win + 1):nrow(X)) {
    tr <- (i - win):(i - 1)
    core <- head(tr, -nv)
    val <- tail(tr, nv)
    sc <- fit_scale(X[core, , drop = FALSE])
    xc <- transform_x(X[core, , drop = FALSE], sc)
    xv <- transform_x(X[val, , drop = FALSE], sc)
    err <- sapply(lambdas, function(lam) mean((y[val] - ridge_predict(xc, y[core], xv, lam))^2))
    lam <- lambdas[which.min(err)]
    st <- fit_scale(X[tr, , drop = FALSE])
    xt <- transform_x(X[tr, , drop = FALSE], st)
    xp <- transform_x(X[i, , drop = FALSE], st)
    pred[i, "OLS"] <- ridge_predict(xt, y[tr], xp, 0)
    pred[i, "RIDGE"] <- ridge_predict(xt, y[tr], xp, lam)
    if (run_xgb) {
        stopifnot(requireNamespace("xgboost", quietly = TRUE))
        dc <- xgboost::xgb.DMatrix(xc, label = y[core])
        dv <- xgboost::xgb.DMatrix(xv, label = y[val])
        pars <- list(objective = "reg:squarederror", eta = 0.03, max_depth = 2, min_child_weight = 5,
            subsample = 1, colsample_bytree = 1, nthread = 1)
        fit <- xgboost::xgb.train(pars, dc, nrounds = 300, watchlist = list(validation = dv),
            early_stopping_rounds = 20, verbose = 0)
        best <- which.min(fit$evaluation_log$validation_rmse)
        full <- xgboost::xgb.train(pars, xgboost::xgb.DMatrix(xt, label = y[tr]), nrounds = best,
            verbose = 0)
        pred[i, "XGB"] <- predict(full, xgboost::xgb.DMatrix(xp))
    }
}

mods <- colnames(pred)[colSums(is.finite(pred)) > 0]

ok <- complete.cases(pred[, mods, drop = FALSE]) & is.finite(y)

stopifnot(any(ok))

metric <- function(p) c(RMSE = sqrt(mean((y[ok] - p[ok])^2)), DirAcc = mean(sign(y[ok]) ==
    sign(p[ok])))

print(t(sapply(mods, function(m) metric(pred[, m]))))

results <- list()

nav_all <- list()

for (m in mods) for (bp in c(0, 5, 10)) {
    w <- as.numeric(pred[ok, m] > 0)
    turnover <- abs(w - c(0, head(w, -1)))
    net <- w * expm1(y[ok]) - bp/10000 * turnover
    stopifnot(all(net > -1))
    nav <- cumprod(1 + net)
    vol <- sd(net) * sqrt(12)
    key <- paste(m, bp, sep = "_")
    nav_all[[key]] <- data.frame(date = target_date[ok], nav = nav)
    results[[key]] <- data.frame(model = m, cost_bp = bp, AnnRet = tail(nav, 1)^(12/length(net)) -
        1, AnnVol = vol, Sharpe = if (vol > 0)
        12 * mean(net)/vol
    else NA_real_, MaxDrawdown = min(nav/cummax(c(1, nav))[-1] - 1))
}

print(do.call(rbind, results))
