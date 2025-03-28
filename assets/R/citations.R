library(rcrossref)

get_cr_citation_count <- function(doi) {
  tryCatch(
    {x <- cr_citation_count(doi = doi)}, 
    error = function(e) return('-')
    )
  count <- if (is.na(x$count)) {
    return('-')
  }
  return(x$count)
}
