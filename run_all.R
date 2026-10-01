# 默认只运行已整理的独立正文演示，不自动联网或安装。
f<-sub('^--file=','',grep('^--file=',commandArgs(),value=TRUE)[1]);root<-dirname(normalizePath(f))
args<-commandArgs(trailingOnly=TRUE);sel<-grep('^--chapters=',args,value=TRUE)
chapters<-if(length(sel))as.integer(strsplit(sub('^--chapters=','',sel[1]),',')[[1]])else 1:12
stopifnot(all(chapters%in%1:12));result<-list()
for(ch in chapters){script<-file.path(root,sprintf('CH%02d/run.R',ch));status<-system2(file.path(R.home('bin'),'Rscript'),shQuote(script));result[[length(result)+1]]<-data.frame(chapter=sprintf('CH%02d',ch),exit_status=status)}
tab<-do.call(rbind,result);print(tab,row.names=FALSE);if(any(tab$exit_status!=0))quit(status=1)
