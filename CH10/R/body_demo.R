# 正文Nelson-Siegel函数：用给定参数展示折现因子、即期和远期曲线。
# 参数仅用于曲线形状演示；不冒充FRED收益率曲线估计。
A_NS <- function(t,theta) {b0<-theta[1];b1<-theta[2];b2<-theta[3];tau0<-theta[4];E<-exp(-t/tau0);b0*t+b1*tau0*(1-E)+b2*(tau0-(t+tau0)*E)}
f_NS <- function(t,theta) theta[1]+(theta[2]+theta[3]*t/theta[4])*exp(-t/theta[4])
run_body_demo <- function(outdir,data_file=NULL) {
  theta<-c(.03,-.02,.025,2);t<-seq(.01,30,length.out=600);A<-A_NS(t,theta)
  tab<-data.frame(years=t,discount=exp(-A),spot=A/t,forward=f_NS(t,theta));save_table(tab,'DEMONSTRATION_NS_curve.csv',outdir)
  save_table(data.frame(parameter=c('beta0','beta1','beta2','tau'),value=theta),'DEMONSTRATION_parameters.csv',outdir)
  pdf(file.path(outdir,'NS_curve.pdf'),width=9,height=4);par(mfrow=c(1,2));plot(t,tab$discount,type='l',col='#176B87',xlab='Years',ylab='Discount factor');matplot(t,tab[,c('spot','forward')],type='l',lty=1:2,col=c('#176B87','#C66D24'),xlab='Years',ylab='Continuously compounded rate');legend('bottomright',c('Spot','Forward'),lty=1:2,col=c('#176B87','#C66D24'));dev.off()
  eps<-1e-5;num<-(A_NS(t+eps,theta)-A_NS(t-eps,theta))/(2*eps)
  finish_demo(c(positive_discount=all(tab$discount>0),forward_derivative=max(abs(num-tab$forward))<1e-8),outdir,
    '正文函数的确定参数演示，不是任何国家的实际曲线。FRED下载/拟合及仿射模型代码只在R/manual中按前置条件手动运行。')
}
