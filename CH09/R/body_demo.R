# 正文第一段代码：标准布朗运动。
run_body_demo <- function(outdir,data_file=NULL) {
  set.seed(123);n<-1000;T<-1;dt<-T/n;t<-seq(0,T,length.out=n+1);dW<-rnorm(n,sd=sqrt(dt));W<-c(0,cumsum(dW))
  save_table(data.frame(time=t,SIMULATED_Brownian=W),'SIMULATED_Brownian_path.csv',outdir)
  pdf(file.path(outdir,'Brownian_motion.pdf'),width=7,height=4);plot(t,W,type='l',col='#176B87',xlab='Time',ylab='W(t)',main='Simulated standard Brownian motion');dev.off()
  finish_demo(c(initial_zero=W[1]==0,increments=max(abs(diff(W)-dW))<1e-12,time_horizon=tail(t,1)==1),outdir,
    '正文标准布朗运动模拟；seed=123；金融高频行情、连续模型估计及MVP例子见R/manual，默认不下载或估计。')
}
