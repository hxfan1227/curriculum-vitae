library(rcrossref)
library(httr)


# 获取 Crossref 引用数
get_crossref_count <- function(doi) {
  tryCatch({
    res <- cr_citation_count(doi = doi)
    if (is.na(res$count)) return(NA)
    return(res$count)
  }, error = function(e) {
    return(NA)
  })
}

# 获取 OpenAlex 引用数
get_openalex_count <- function(doi) {
  tryCatch({
    encoded_doi <- URLencode(paste0("https://doi.org/", doi), reserved = TRUE)
    url <- paste0("https://api.openalex.org/works/", encoded_doi)
    res <- GET(url)
    if (status_code(res) != 200) return(NA)
    data <- content(res, as = "parsed", encoding = "UTF-8")
    return(data$cited_by_count)
  }, error = function(e) {
    return(NA)
  })
}

# 综合比较函数：返回较高引用数
get_best_citation_count <- function(doi) {
  crossref_count <- get_crossref_count(doi)
  openalex_count <- get_openalex_count(doi)
  
  if (is.na(crossref_count) && is.na(openalex_count)) {
    return('-')
  }
  
  return(max(crossref_count, openalex_count, na.rm = TRUE))
}
