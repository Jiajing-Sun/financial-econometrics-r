# 正文：圣彼得堡悖论。正文奖金X=2^(N-1)，与其他奖金约定不得混用。
run_body_demo <- function(outdir,data_file=NULL) {
  k<-1:100;prob<-2^(-k);prize<-2^(k-1)
  expected_log_prize<-sum(prob*log(prize))
  tab<-data.frame(truncation=k,truncated_expected_prize=cumsum(prob*prize),truncated_expected_log_prize=cumsum(prob*log(prize)))
  save_table(tab,'st_petersburg_body.csv',outdir)
  pdf(file.path(outdir,'st_petersburg_body.pdf'),width=7,height=4)
  plot(k,tab$truncated_expected_prize,type='l',col='#176B87',xlab='Truncation N',ylab='Truncated expected prize');dev.off()
  finish_demo(c(expected_prize_increments=all(abs(diff(tab$truncated_expected_prize)-.5)<1e-12),expected_log_prize=abs(expected_log_prize-log(2))<1e-12),outdir,
    '正文奖金约定下的确定性级数演示。正文股票与债券行情案例见R/manual，需显式联网。')
}
