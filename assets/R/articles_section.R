articles_section <- function(bib = "data/cv.bib", author = NULL, author_zh = NULL, page_break_after = FALSE, only_first = FALSE) {
  author <- gsub(" ", "&nbsp;", author)
  text <- data.table::setDT(read_bib(bib))[
    j = sprintf(
      "### %s\n\n%s\n\nN/A\n\n%s %s\n\n::: aside\n\n*[%s](%s)*\n%s\n:::",
      title,
      format_bib_author(authors, first, chinese, author, author_zh),
      month, year,
      journal, doi,
      ifelse(
        test = first,
        yes = '<p style="font-size: 75%;"><sup>&dagger;</sup> As first or corresponding author.</p>',
        no = ""
      )
    )
  ]

  articles_count <- length(text)

  if (only_first) {
    text <- text[grepl("As first or corresponding author", text)]
    articles_count <- sprintf("%s + %s", length(text), articles_count - length(text))
  }

  if (page_break_after) {
    c(
      sprintf("## Publications (%s) {data-icon=newspaper .break-after-me}", articles_count),
      text
    )
  } else {
    c(
      sprintf("## Publications (%s) {data-icon=newspaper}", articles_count),
      text
    )
  }
}

clean_field <- function(pattern, x) {
  gsub(
    pattern = sprintf("^%s = ", pattern),
    replacement = "",
    x = gsub(
      pattern = ",$",
      replacement = "",
      x = gsub(
        pattern = "[{}]",
        replacement = "",
        x = grep(sprintf("^%s", pattern), x, value = TRUE)
      )
    )
  )
}

read_article <- function(.x, customColor = '\\\\textcolorCustomblue\\\\textbf') {
  authors <- do.call("rbind", strsplit(unlist(strsplit(gsub(pattern = customColor, replacement = '', x = clean_field("author", .x)), " and ")), ", "))
  if (NCOL(authors) > 1) { # English
    authors <- apply(X = authors[, c(2, 1)], MARGIN = 1, FUN = function(irow) {
      gsub(" ", "&nbsp;", paste(unique(irow), collapse = " "))
    })
    authors <- paste(paste(authors[-length(authors)], collapse = ", "), authors[length(authors)], sep = " and ")
  } else {
    authors <- paste(paste(authors[-length(authors)], collapse = ", "), authors[length(authors)], sep = " 和 ") # Chinese
  }
  data.frame(
    title = clean_field("title", .x),
    month = gsub("May.", "May", paste0(capitalise(clean_field("month", .x)), ".")),
    year = clean_field("year", .x),
    doi = clean_field("doi", .x),
    authors = authors,
    journal = clean_field("journal", .x),
    chinese = if (any(grepl("keywords", .x))) {
      grepl("chinese", clean_field("keywords", .x))
    } else {
      FALSE
    },
    first = if (any(grepl("keywords", .x))) {
      grepl("first", clean_field("keywords", .x))
    } else {
      FALSE
    },
    stringsAsFactors = FALSE
  )
}

read_bib <- function(path) {
  big_file <- paste(readLines(path), collapse = "")
  big_file <- unlist(strsplit(x = big_file, split = "@", fixed = TRUE))
  big_file <- big_file[nchar(big_file) != 0]

  all_bib <- lapply(strsplit(x = big_file, split = "(,\t)|(,  )"), read_article)
  all_bib <- do.call("rbind.data.frame", all_bib)
  all_bib[["month"]] <- factor(
    x = all_bib[["month"]],
    levels = gsub("May.", "May", paste0(c(
      "Jan", "Feb", "Mar", "Apr", "May", "Jun",
      "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
    ), "."))
  )
  all_bib[["doi"]] <- ifelse(
    test = grepl("^http", all_bib[["doi"]]),
    yes = all_bib[["doi"]],
    no = paste0("https://www.doi.org/", all_bib[["doi"]])
  )

  all_bib[order(-all_bib[["chinese"]], all_bib[["first"]], all_bib[["year"]], all_bib[["month"]], decreasing = TRUE), ]
}

format_bib_author <- function(authors, first, chinese, author, author_zh, max = 10) {
  mapply(
    iauthors = authors,
    ifirst = first,
    ichinese = chinese, 
    FUN = function(iauthors, ifirst, ichinese) {
      split_authors <- unlist(strsplit(strsplit(iauthors, ", ")[[1]], if (ichinese) " 和 " else " and " ))
      split_authors <- gsub(
        pattern = if (ichinese) author_zh else author,
        replacement = paste0("<u>", if (ichinese) author_zh else author, "</u>", if (ifirst) "<sup>&dagger;</sup>" else ""),
        x = split_authors
      )
      pos_author <- grep(author, split_authors)
      if (length(split_authors) <= max) {
        paste(
          paste(split_authors[-length(split_authors)], collapse = ", "),
          split_authors[length(split_authors)],
          sep = if (ichinese) " 和 " else " and "
        )
      } else {
        switch(
          EXPR = paste(
            abs(c(0, length(split_authors)) - pos_author) > ceiling(max / 2),
            collapse = "--"
          ),
          "TRUE--TRUE" = {
            if (pos_author > ceiling((max - 1) / 2)) {
              split_authors[pos_author] <- paste0(
                split_authors[pos_author],
                "<sup>", pos_author, "/", length(split_authors), "</sup>"
              )
            }
            paste0(
              paste(
                c(
                  split_authors[1:ceiling((max - 1) / 2)],
                  "*[...]*",
                  split_authors[pos_author],
                  "*[...]*",
                  split_authors[
                    (length(split_authors) - (max - 1 - ceiling((max - 1) / 2))):(length(split_authors) - 1)
                  ]
                ),
                collapse = ", "
              ),
              if (ichinese) " 和 " else " and ",
              split_authors[length(split_authors)]
            )
          },
          "TRUE--FALSE" = {
            if (pos_author > ceiling(max / 2)) {
              split_authors[pos_author] <- paste0(
                split_authors[pos_author],
                "<sup>", pos_author, "/", length(split_authors), "</sup>"
              )
            }
            paste0(
              paste(
                c(
                  split_authors[1:ceiling(max / 2)],
                  "*[...]*",
                  split_authors[
                    (length(split_authors) - (max - 1 - ceiling(max / 2))):(length(split_authors) - 1)
                  ]
                ),
                collapse = ", "
              ),
              if (ichinese) " 和 " else " and ",
              split_authors[length(split_authors)]
            )
          },
          "FALSE--TRUE" = {
            if (pos_author > ceiling(max / 2)) {
              split_authors[pos_author] <- paste0(
                split_authors[pos_author],
                "<sup>", pos_author, "/", length(split_authors), "</sup>"
              )
            }
            paste0(
              paste(
                c(
                  split_authors[1:ceiling(max / 2)],
                  "*[...]*",
                  split_authors[
                    (length(split_authors) - (max - 1 - ceiling(max / 2))):(length(split_authors) - 1)
                  ]
                ),
                collapse = ", "
              ),
              if (ichinese) " 和 " else " and ",
              split_authors[length(split_authors)]
            )
          },
          "FALSE--FALSE" = {
            paste(
              paste(split_authors[-length(split_authors)], collapse = ", "),
              split_authors[length(split_authors)],
              sep = if (ichinese) " 和 " else " and "
            )
          }
        )
      }
    }
  )
}
