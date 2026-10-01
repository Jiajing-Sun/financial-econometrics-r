# 正文代码块6和8：AR(2)模拟、Yule-Walker与ar.yw。
run_body_demo <- function(outdir,data_file=NULL) {
  set.seed(123456);x<-as.numeric(arima.sim(n=1000,model=list(ar=c(.6,-.4))))
  rho<-as.numeric(acf(x,plot=FALSE,lag.max=2)$acf)[-1]
  manual<-solve(matrix(c(1,rho[1],rho[1],1),2,2),rho)
  fitted<-ar.yw(x,order.max=2,aic=FALSE)
  save_table(data.frame(t=seq_along(x),SIMULATED_AR2=x),'SIMULATED_AR2.csv',outdir)
  save_table(data.frame(lag=1:2,true=c(.6,-.4),Yule_Walker=manual,ar_yw=fitted$ar),'AR2_coefficients.csv',outdir)
  pdf(file.path(outdir,'AR2_diagnostics.pdf'),width=9,height=4);par(mfrow=c(1,2));acf(x,main='Simulated AR(2): ACF');pacf(x,main='Simulated AR(2): PACF');dev.off()
  finish_demo(c(YW_agrees=max(abs(manual-fitted$ar))<1e-10,stationary_roots=all(Mod(polyroot(c(1,-.6,.4)))>1)),outdir,
    '正文AR(2)模拟；seed=123456；所有生成序列明确为模拟，不是股指行情。')
}
