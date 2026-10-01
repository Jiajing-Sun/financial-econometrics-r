repo_need <- function(packages) {
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly=TRUE)]
  if(length(missing)) stop(paste('请按本章 README 手动安装 R 包：',paste(missing,collapse=', ')),call.=FALSE)
}
save_table <- function(x,name,outdir) write.csv(as.data.frame(x),file.path(outdir,name),row.names=FALSE,na='')
finish_demo <- function(checks,outdir,note) {
  save_table(data.frame(check=names(checks),passed=unname(checks)), 'verification.csv',outdir)
  writeLines(note,file.path(outdir,'scope.txt'));stopifnot(all(checks));invisible(checks)
}
