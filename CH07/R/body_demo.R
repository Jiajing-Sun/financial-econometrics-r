# 正文：核函数及EuStockMarkets中DAX收益率的核密度估计。
run_body_demo <- function(outdir,data_file=NULL) {
  x<-seq(-1.5,1.5,length.out=601)
  kernels<-data.frame(x=x,uniform=.5*(abs(x)<=1),Epanechnikov=.75*(1-x^2)*(abs(x)<=1),Gaussian=dnorm(x))
  save_table(kernels,'kernel_functions.csv',outdir)
  p<-as.numeric(EuStockMarkets[,'DAX']);r<-100*(tail(p,-1)/head(p,-1)-1)
  save_table(data.frame(observation=seq_along(p),DAX=p),'DAX_builtin_prices.csv',outdir)
  save_table(data.frame(observation=seq_along(r),simple_return_pct=r),'DAX_simple_returns_pct.csv',outdir)
  d<-density(r);save_table(data.frame(return_pct=d$x,density=d$y),'DAX_kernel_density.csv',outdir)
  pdf(file.path(outdir,'kernel_density.pdf'),width=9,height=4);par(mfrow=c(1,2))
  matplot(x,kernels[,-1],type='l',lty=1:3,col=c('#176B87','#C66D24','#222222'),xlab='u',ylab='Kernel')
  hist(r,probability=TRUE,breaks=40,col='#BBD6DF',main='DAX: built-in historical sample',xlab='Simple return (%)');lines(d,col='#176B87',lwd=2);dev.off()
  finish_demo(c(Epanechnikov_integral=abs(integrate(function(u).75*(1-u*u),-1,1)$value-1)<1e-10,price_alignment=length(r)==length(p)-1),outdir,
    'R datasets::EuStockMarkets的公开历史DAX样本（1991—1998）；不是最新行情。显式以前一期价格为分母，避免ts滞后索引歧义。')
}
