# 正文：mtcars公制回归与出版社匿名A公司CAPM；不包含章末题解。
run_body_demo <- function(outdir,data_file=NULL) {
  cars<-transform(mtcars,weight_t=wt*.45359237,fuel_eff_km_l=mpg*1.609344/3.785411784)
  fit<-lm(fuel_eff_km_l~weight_t,data=cars);original<-lm(mpg~wt,data=mtcars)
  expected<-coef(original)*c(1.609344/3.785411784,(1.609344/3.785411784)/.45359237)
  save_table(data.frame(term=names(coef(fit)),estimate=coef(fit)),'mtcars_metric_coefficients.csv',outdir)
  save_table(data.frame(car=rownames(cars),cars,row.names=NULL),'mtcars_metric_data.csv',outdir)
  if(is.null(data_file)) data_file<-'data/stock_data.csv'
  d<-read.csv(data_file,stringsAsFactors=FALSE)
  required<-c('Date','stock_price','market_index','risk_free_rate');stopifnot(all(required%in%names(d)))
  d$Date<-as.Date(d$Date);stopifnot(!anyNA(d$Date),!anyDuplicated(d$Date),all(diff(d$Date)>0))
  imputed<-!complete.cases(d[,required]);save_table(data.frame(date=d$Date,interpolated=imputed),'publisher_interpolation_flags.csv',outdir)
  for(nm in c('stock_price','market_index','risk_free_rate')) {
    if(anyNA(d[[nm]]))d[[nm]]<-approx(seq_len(nrow(d)),d[[nm]],xout=seq_len(nrow(d)),rule=1)$y
  }
  stopifnot(!anyNA(d[,required]),all(d$stock_price>0),all(d$market_index>0))
  n<-nrow(d);r<-data.frame(date=d$Date[-1],stock=diff(log(d$stock_price)),market=diff(log(d$market_index)),rf=d$risk_free_rate[-1]/100/365)
  r$stock_excess<-r$stock-r$rf;r$market_excess<-r$market-r$rf
  capm<-lm(stock_excess~market_excess,data=r)
  save_table(r,'publisher_CAPM_returns.csv',outdir)
  save_table(data.frame(term=rownames(coef(summary(capm))),coef(summary(capm)),row.names=NULL),'publisher_CAPM_coefficients.csv',outdir)
  pdf(file.path(outdir,'body_regressions.pdf'),width=9,height=4);par(mfrow=c(1,2))
  plot(cars$weight_t,cars$fuel_eff_km_l,pch=16,col='#176B87',xlab='Weight (tonnes)',ylab='Fuel efficiency (km/l)');abline(fit,col='#C66D24')
  plot(r$market_excess,r$stock_excess,pch=16,col='#176B87',xlab='Market excess log return',ylab='Company A excess log return');abline(capm,col='#C66D24');dev.off()
  finish_demo(c(metric_scaling=max(abs(coef(fit)-expected))<1e-10,CAPM_normal_equations=max(abs(crossprod(model.matrix(capm),residuals(capm))))<1e-9),outdir,
    c('mtcars为R内置数据；stock_data.csv为出版社匿名化正文案例，不猜测公司身份。',
      'CAPM保留正文口径：对数收益减年化无风险利率百分数/100/365；这是教学近似，不是逐日历间隔复利收益。',
      '保留出版社正文的按行线性插值口径（原数据12行价格缺失），逐行输出标记；不外推端点。插值价格不是实际成交价格，结果仅用于复现正文。'))
}
