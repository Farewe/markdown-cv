library(rcrossref)

format_reference_from_doi <- function(doi) {
  if (grepl("doi.org", doi)) {
    # Remove the "http(s)://(dx.)?doi.org/" part
    doi <- sub("^https?://(dx\\.)?doi.org/", "", doi)
  }
  
  
  # Fetch metadata in citeproc-json format
  metadata <- tryCatch({
    rcrossref::cr_cn(dois = doi, format = "citeproc-json")
  }, error = function(e) {
    stop("Failed to retrieve data for the given DOI.")
  })
  
  # Query HAL for a matching record
  hal_url <- ""
  hal_api_url <- paste0("https://api.archives-ouvertes.fr/search/?q=", doi, "&fl=halId_s")
  res <- tryCatch({
    httr::GET(hal_api_url)
  }, error = function(e) NULL)
  
  if (!is.null(res) && httr::status_code(res) == 200) {
    js <- httr::content(res, as = "parsed", type = "application/json")
    if (!is.null(js$response$docs) && length(js$response$docs) > 0) {
      hal_id <- js$response$docs[[1]]$halId_s
      if (!is.null(hal_id)) {
        hal_url <- paste0("https://hal.science/", hal_id)
      }
    }
  }
  
  # Extract fields
  authors <- metadata$author
  # Format authors as "LastName Initials"
  # and bold the second author if present.
  authors_formatted <- sapply(1:nrow(authors), function(i) {
    author <- authors[i, ]
    family <- author$family
    given_names <- author$given
    # Extract initials from given names
    initials <- paste(sapply(strsplit(given_names, "\\s+")[[1]], function(x) substr(x, 1, 1)), collapse = "")
    author_str <- paste0(family, " ", initials)
    # Bold the second author
    if (author_str == "Leroy B") {
      author_str <- paste0("**", author_str, "**")
    }
    author_str
  })
  
  authors_str <- paste(authors_formatted, collapse = ", ")
  
  # Year
  # issued$date-parts is typically a list of lists with year, month, day
  year <- metadata$issued$`date-parts`[[1]][1]
  
  title <- metadata$title
  journal <- metadata$`container-title`
  volume <- if (!is.null(metadata$volume)) metadata$volume else ""
  page   <- if (!is.null(metadata$page)) metadata$page else ""
  
  # Construct the HAL link portion
  hal_part <- if (nzchar(hal_url)) paste0("[HAL](", hal_url, ")") else "[HAL]()"
  
  # Construct the final reference string
  # Example format:
  # Marino C, **Leroy B**, Latombe G, Bellard C. (2024). Exposure and Sensitivity ...
  # *Global Change Biology* 30:e17607 [doi](https://doi.org/10.1111/gcb.17607) --- [HAL]()
  
  ref <- sprintf("%s. (%s). %s. *%s* %s:%s [doi](https://doi.org/%s) --- %s", 
                 authors_str, year, title, journal, volume, page, doi, hal_part)
  
  return(ref)
}
# Example usage:
# format_reference_from_doi("10.1111/gcb.17607")
