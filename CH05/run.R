# 在仓库根运行：Rscript CH05/run.R [--data=CH05/data/user/input.csv]
script_arg<-grep('^--file=',commandArgs(),value=TRUE)
if(!length(script_arg)) stop('请使用Rscript运行本入口。')
chapter_dir<-dirname(normalizePath(sub('^--file=','',script_arg[1])))
repo_root<-dirname(chapter_dir)
args<-commandArgs(trailingOnly=TRUE);a<-grep('^--data=',args,value=TRUE)
data_file<-if(length(a))normalizePath(sub('^--data=','',a[1]),mustWork=TRUE)else NULL
setwd(chapter_dir)
source(file.path(repo_root,'tools/run_support.R'),encoding='UTF-8')
source('R/body_demo.R',encoding='UTF-8')
outdir<-if(is.null(data_file))'results/current'else'results/user'
dir.create(outdir,recursive=TRUE,showWarnings=FALSE)
run_body_demo(outdir,data_file)
message('CH05 正文入口完成：',outdir)
