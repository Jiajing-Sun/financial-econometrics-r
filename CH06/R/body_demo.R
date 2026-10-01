# 正文贵州茅台案例的数据接口；无原始价格文件时只核验接口，不编造实证结果。
run_body_demo <- function(outdir,data_file=NULL) {
  if(is.null(data_file)) {
    writeLines(c('NEEDS_USER_DATA','正文案例需要用户提供date,close格式的复权收盘价。',
      '运行：Rscript CH06/run.R --data=CH06/data/user/Moutai.csv','默认未运行或验证真实市场检验。'),file.path(outdir,'data_required.txt'))
    return(finish_demo(c(interface_defined=TRUE),outdir,'接口与语法检查通过；真实数据分析未运行。'))
  }
  d<-read.csv(data_file,stringsAsFactors=FALSE);stopifnot(all(c('date','close')%in%names(d)))
  d$date<-as.Date(d$date);d<-d[order(d$date),];stopifnot(!anyNA(d[,c('date','close')]),!anyDuplicated(d$date),all(d$close>0),nrow(d)>30)
  dp<-diff(d$close);r<-diff(log(d$close));tests<-lapply(c('Box-Pierce','Ljung-Box'),function(m)Box.test(dp,lag=10,type=m))
  save_table(data.frame(test=c('Box-Pierce','Ljung-Box'),statistic=sapply(tests,function(z)unname(z$statistic)),p_value=sapply(tests,function(z)z$p.value)),'price_increment_tests.csv',outdir)
  if(requireNamespace('vrtest',quietly=TRUE)) {
    set.seed(123);v<-vrtest::AutoBoot.test(r,nboot=500,wild='Normal');capture.output(v,file=file.path(outdir,'AutoBoot_log_returns.txt'))
  } else writeLines('vrtest未安装；自动方差比部分未运行。',file.path(outdir,'optional_test_status.txt'))
  finish_demo(c(valid_prices=all(is.finite(r))),outdir,
    '使用用户提供的真实价格。价格增量与对数收益分别检验；不拒绝不能证明独立同分布或弱式有效。')
}
