# 正文GARCH(1,1)递推。默认只执行该独立模拟，估计网格见R/manual/example_03.R。
run_body_demo <- function(outdir,data_file=NULL) {
  set.seed(123);n<-1000L;omega<-.01;alpha<-.05;beta<-.9
  h<-rep(NA_real_,n);y<-numeric(n);h[1]<-omega/(1-alpha-beta)
  for(t in 2:n){h[t]<-omega+alpha*y[t-1]^2+beta*h[t-1];y[t]<-rnorm(1,sd=sqrt(h[t]))}
  save_table(data.frame(t=1:n,SIMULATED_return=y,conditional_variance=h),'SIMULATED_GARCH.csv',outdir)
  pdf(file.path(outdir,'GARCH_simulation.pdf'),width=9,height=4);par(mfrow=c(1,2));plot(y,type='l',xlab='t',ylab='Simulated return');plot(h,type='l',col='#176B87',xlab='t',ylab='Conditional variance');dev.off()
  finish_demo(c(positive_variance=all(h>0),recursion=max(abs(h[-1]-(omega+alpha*y[-n]^2+beta*h[-n])))<1e-12,finite_variance_condition=alpha+beta<1),outdir,
    c('正文GARCH模拟；seed=123；没有将模拟数据命名为真实股票。','完整ARMA/GARCH、BEKK与DCC正文例子为手动入口，需相应R包及真实数据。'))
}
