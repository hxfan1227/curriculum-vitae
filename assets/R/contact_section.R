contact_section <- function(xlsx = "data/cv.xlsx", sheet = "contact", colour = "#333333") {
  read_excel_sheet(xlsx, sheet)[
    j = sprintf(
      fmt = paste(
        "## Contact Info {#contact}\n",
        "- %s %s", # position
        "- %s %s", # institute
        "- %s %s", # city
        "- %s [%s](mailto:%s)", # email
        "- %s %s", # phone
        "- %s [%s](%s)", # website
        "- %s [%s](https://orcid.org/%s)", # orcid
        "- %s [%s](https://www.linkedin.com/in/%s)", # linkedin
        "- %s [%s](https://github.com/%s)", # github
        # "- %s [%s](https://twitter.com/%s)", # twiter
        # "- %s [@%s](https://%s)", # mastodon
        "- %s %s", # weixin
        "- %s %s", # H-index
        "\n",
        sep = "\n"
      ),
      fontawesome::fa("user", fill = colour), position,
      fontawesome::fa("building-columns", fill = colour), institute,
      fontawesome::fa("map-location-dot", fill = colour), city,
      fontawesome::fa("envelope", fill = colour), gsub("\\.", "[dot]", sub("@", "[at]", email)), email,
      fontawesome::fa("phone", fill = colour), phone,
      fontawesome::fa("house", fill = colour), sub("/$", "", sub("https*://", "", website)), website,
      fontawesome::fa("orcid", fill = colour), orcid, orcid,
      fontawesome::fa("linkedin", fill = colour), linkedin, linkedin,
      fontawesome::fa("github", fill = colour), github, github,
      # fontawesome::fa("twitter", fill = colour), twitter, twitter,
      # fontawesome::fa("mastodon", fill = colour), mastodon, paste(rev(strsplit(mastodon, "@")[[1]]), collapse = "/@"),
      fontawesome::fa("weixin", fill = colour), weixin,
      'H-Index:', scholar::get_profile(id = scholar)$h_index
    )
  ]
}
