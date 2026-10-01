# 正文：循环、函数、e的数值近似、R内置mtcars与基础图形。
run_body_demo <- function(outdir,data_file=NULL) {
  n<-c(10,100,1000,10000)
  e1<-(1+1/n)^n
  e2<-vapply(n,function(k)sum(1/gamma((0:min(k,170))+1)),numeric(1))
  tab<-data.frame(n=n,limit_approximation=e1,series_approximation=e2,exp_one=exp(1))
  save_table(tab,'e_approximations.csv',outdir)
  cars<-data.frame(car=rownames(mtcars),mtcars,row.names=NULL)
  save_table(cars,'mtcars_builtin.csv',outdir)
  save_table(aggregate(mpg~cyl,cars,mean),'mean_mpg_by_cyl.csv',outdir)
  pdf(file.path(outdir,'mtcars_body_graphs.pdf'),width=9,height=4)
  par(mfrow=c(1,2));hist(cars$mpg,col='#BBD6DF',main='mtcars: fuel economy',xlab='Miles per US gallon')
  plot(cars$wt,cars$mpg,pch=16,col='#176B87',xlab='Weight (1000 lb)',ylab='Miles per US gallon');dev.off()
  finish_demo(c(e_limit=abs(tail(e1,1)-exp(1))<.001,series=all(abs(e2-exp(1))<1e-7),builtin_rows=nrow(cars)==32),outdir,
    '正文离线演示；mtcars为R公开内置数据，e近似为确定性计算。未运行章节习题。')
}
