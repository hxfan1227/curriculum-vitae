library(rcrossref)

get_cr_citation_count <- function(doi) {
  x <- cr_citation_count(doi = doi)
  count <- if (is.na(x$count)) {
    return('-')
  }
  return(x$count)
}
