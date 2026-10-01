# 正文第一个树模型选股示例的缩小模拟：按时间训练，下一月留出。
run_body_demo <- function(outdir,data_file=NULL) {
  repo_need('rpart');set.seed(123);N<-40;T<-42
  rate<-as.numeric(scale(arima.sim(list(ar=.8),n=T,sd=.2)))
  d<-expand.grid(id=1:N,month=1:T);d$rate<-rate[d$month];d$pe<-pmax(rnorm(nrow(d),15,5),1);d$mom<-rnorm(nrow(d));d$vol<-exp(rnorm(nrow(d),sd=.3))
  signal<--.02*d$pe-.5*d$rate+.3*d$mom-.1*d$vol+ifelse(d$rate<0&d$pe>18,-.2,0)
  d$ret_fwd<-.02*signal+rnorm(nrow(d),sd=.05);d$y<-factor(ifelse(d$ret_fwd>0,'Up','Down'),levels=c('Down','Up'))
  result<-list()
  for(t in 25:T){train<-d[d$month %in% (t-24):(t-1),];test<-d[d$month==t,]
    fit<-rpart::rpart(y~rate+pe+mom+vol,data=train,method='class',control=rpart::rpart.control(cp=.01,minsplit=30))
    p<-predict(fit,test,type='prob')[,'Up'];result[[length(result)+1]]<-data.frame(month=t,id=test$id,probability_up=p,label=as.integer(test$y=='Up'),realised_return=test$ret_fwd)}
  pred<-do.call(rbind,result);save_table(pred,'SIMULATED_tree_time_holdout.csv',outdir)
  save_table(d,'SIMULATED_stock_panel.csv',outdir)
  metrics<-data.frame(n=nrow(pred),accuracy=mean((pred$probability_up>.5)==pred$label),Brier=mean((pred$probability_up-pred$label)^2));save_table(metrics,'SIMULATED_tree_metrics.csv',outdir)
  finish_demo(c(probability_range=all(pred$probability_up>=0&pred$probability_up<=1),time_holdout=all(pred$month>=25),finite=all(is.finite(pred$probability_up))),outdir,
    '正文树模型选股的缩小模拟（40只股票、42个月）；seed=123，滚动训练24个月。仅验证正文方法流程；不是任何真实投资策略收益，也未运行章末习题。')
}
