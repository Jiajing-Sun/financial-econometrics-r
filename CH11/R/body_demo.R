# 正文Hill估计函数（对正损失降序取尾部）；默认用已知Pareto分布演示接口。
hill_body <- function(x,M) {
  x<-sort(x[is.finite(x)&x>0],decreasing=TRUE);stopifnot(M>=2,M<length(x));H<-mean(log(x[1:M]/x[M+1]));c(tail_exponent=1/H,standard_error=(1/H)/sqrt(M))
}
run_body_demo <- function(outdir,data_file=NULL) {
  set.seed(123);loss<-(1-runif(3000))^(-1/3);M<-seq(50,500,50)
  estimates<-t(vapply(M,function(m)hill_body(loss,m),numeric(2)))
  save_table(data.frame(SIMULATED_positive_loss=loss),'SIMULATED_Pareto_losses.csv',outdir)
  save_table(data.frame(M=M,estimates,true_exponent=3),'SIMULATED_Hill_path.csv',outdir)
  pdf(file.path(outdir,'SIMULATED_Hill_path.pdf'),width=7,height=4);plot(M,estimates[,1],type='b',col='#176B87',xlab='Tail order statistics M',ylab='Estimated tail exponent');abline(h=3,col='#C66D24',lty=2);dev.off()
  finish_demo(c(positive=all(loss>=1),finite=all(is.finite(estimates)),manual_Hill=abs(hill_body(c(8,4,2,1),2)[1]-1/mean(log(c(8,4)/2)))<1e-12),outdir,
    '正文Hill函数的Pareto模拟演示（尾指数3，seed=123）。原正文SPY收益、GARCH和Copula实证代码见R/manual，需联网与额外包。未发布习题风险回测结果。')
}
