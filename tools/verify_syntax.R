# 从仓库根目录运行：Rscript tools/verify_syntax.R
f <- sub("^--file=", "", grep("^--file=", commandArgs(), value=TRUE)[1])
root <- dirname(dirname(normalizePath(f)))
old <- setwd(root); on.exit(setwd(old))
paths <- sort(list.files(".", pattern="\\.[Rr]$", recursive=TRUE))
paths <- paths[!grepl("^\\.git/", paths)]
rows <- lapply(paths, function(p) {
  err <- tryCatch({parse(p, encoding="UTF-8"); ""}, error=function(e) conditionMessage(e))
  data.frame(file=p, passed=identical(err,""), message=err)
})
x <- do.call(rbind, rows)
dir.create("verification",showWarnings=FALSE)
write.csv(x, "verification/parse_results.csv",row.names=FALSE,na="")
writeLines(R.version.string,"verification/R_version.txt")
print(table(x$passed));stopifnot(all(x$passed))
