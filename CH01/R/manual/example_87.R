# 正文来源：CH1-R语言概述.tex，代码块 87；正文第 2386 行。
# 只提取章末习题之前的正文代码；原控制台输出未纳入。
# 手动示例：可能依赖前序代码、外部文件、额外R包；参见本章README与manual/index.csv。
# 已移除自动安装、清空工作空间、保存整个工作空间及本机工作目录切换。
dir.create("results/manual", recursive=TRUE, showWarnings=FALSE)
if (!identical(Sys.getenv('FIN_ECON_ENABLE_NETWORK'), '1')) stop('此正文案例会联网；确认数据口径后设置 FIN_ECON_ENABLE_NETWORK=1 再手动运行。', call.=FALSE)
library(httr2)

library(jsonlite)

library(tibble)

library(purrr)

library(dplyr)

library(ggplot2)

fetch_wb <- function(iso3, indicator) {
    url <- sprintf("https://api.worldbank.org/v2/country/%s/indicator/%s", iso3, indicator)
    req <- req_retry(req_timeout(req_url_query(request(url), format = "json", per_page = 20000), 60), max_tries = 3)
    resp <- req_perform(req)
    j <- resp_body_json(resp, simplifyVector = TRUE)
    if (length(j) < 2 || is.null(j[[2]])) {
        return(tibble(iso3c = iso3, year = integer(), value = numeric()))
    }
    arrange(transmute(as_tibble(j[[2]]), country = country$value, iso3c = countryiso3code, year = as.integer(date), 
        value = as.numeric(value)), year)
}

df <- map_dfr(c("CHN", "GBR", "USA"), fetch_wb, indicator = "NY.GDP.PCAP.KD")

ggplot(df, aes(year, value, color = iso3c)) + geom_line() + labs(title = "World Bank API（直接请求）— 人均GDP", 
    x = "年份", y = "人均GDP（常量美元）", color = "国家")
