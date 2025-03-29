library(rcrossref)

get_cr_citation_count <- function(doi) {
  x <- tryCatch(
    cr_citation_count(doi = doi), 
    error = function(e) return(NULL)
  )
  
  if (is.null(x) || is.na(x$count)) {
    return('-')
  }
  
  return(x$count)
}

