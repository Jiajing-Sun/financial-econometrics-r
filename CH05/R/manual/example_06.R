# 正文来源：CH5-波动率模型.tex，代码块 6；修订稿第 903 行。
# 仅提取章末习题之前的正文；不含习题提示或答案。
# 语法已检查；未宣称全部外部数据与可选分支已执行。
set.seed(123)

y <- as.numeric(arima.sim(model = list(ar = 0.5), n = 100))

fit <- lm(y[-1] ~ y[-length(y)])

e2 <- residuals(fit)^2

T_eff <- length(e2)

p <- 3L

stopifnot(p >= 1L, T_eff > p + 1L)

lagged <- embed(e2, p + 1L)

aux <- lm(lagged[, 1] ~ lagged[, -1, drop = FALSE])

LM <- nrow(lagged) * summary(aux)$r.squared

z <- e2 - mean(e2)

gamma0 <- sum(z^2)/T_eff

gammaj <- vapply(seq_len(p), function(j) {
    sum(z[(j + 1L):T_eff] * z[seq_len(T_eff - j)])/T_eff
}, numeric(1))

rho2 <- gammaj/gamma0

Q_BP <- T_eff * sum(rho2^2)

Q_LB <- T_eff * (T_eff + 2) * sum(rho2^2/(T_eff - seq_len(p)))

c(LM = LM, Box_Pierce = Q_BP, Ljung_Box = Q_LB)

pchisq(c(LM, Q_BP, Q_LB), df = p, lower.tail = FALSE)
