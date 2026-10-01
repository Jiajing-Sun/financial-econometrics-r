# 正文等权SMB/HML构建思路的模拟演示；非完整Fama-French官方2x3因子。
run_body_demo <- function(outdir,data_file=NULL) {
  set.seed(123);N<-100L;T<-24L
  mv<-matrix(runif(N*T,100,1100),N);pb<-matrix(runif(N*T,1,6),N);r<-matrix(runif(N*T,-.05,.05),N)
  ans<-data.frame(period=2:T,SMB=NA_real_,HML=NA_real_)
  for(t in 2:T){size<-order(mv[,t-1]);value<-order(1/pb[,t-1],decreasing=TRUE)
    ans$SMB[t-1]<-mean(r[size[1:50],t])-mean(r[size[51:100],t])
    ans$HML[t-1]<-mean(r[value[1:30],t])-mean(r[value[71:100],t])}
  save_table(ans,'SIMULATED_equal_weight_factors.csv',outdir)
  panel<-expand.grid(stock=1:N,period=1:T);panel$market_value<-as.vector(mv);panel$PB<-as.vector(pb);panel$return<-as.vector(r)
  save_table(panel,'SIMULATED_factor_input.csv',outdir)
  pdf(file.path(outdir,'SIMULATED_factor_plot.pdf'),width=7,height=4);matplot(ans$period,ans[,c('SMB','HML')],type='l',lty=1:2,col=c('#176B87','#C66D24'),xlab='Period',ylab='Long-short return');legend('topright',c('SMB','HML'),lty=1:2,col=c('#176B87','#C66D24'));dev.off()
  finish_demo(c(finite_factors=all(is.finite(as.matrix(ans))),range=all(abs(ans$SMB)<=.1)&all(abs(ans$HML)<=.1)),outdir,
    c('正文简化等权因子教学模拟；seed=123；不是中国股票真实因子结果。',
      '用t-1期已知市值/PB分组，计算t期收益；与原片段同列分组相比显式落实事前信息要求。','官方因子通常采用交叉排序、市值加权及特定再平衡规则，不应把本模拟直接命名为官方SMB/HML。'))
}
