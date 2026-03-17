library(httr)
library(jsonlite)
library(stringr)
library(dplyr)

fetch_semantic_scholar_citations <- function(dois) {
  # Return NA if no DOI is provided
  if (is.na(dois) || dois == "") return(NA)
  
  # Split into individual DOIs (assumes semicolon-separated) and trim whitespace
  doi_list <- unlist(strsplit(dois, ";"))
  doi_list <- trimws(doi_list)
  
  # Function to fetch citation count for a single DOI
  get_citation <- function(doi) {
    # Remove any "doi:" prefix
    doi_clean <- gsub("^doi:\\s*", "", doi, ignore.case = TRUE)
    # Remove URL prefixes such as "https://doi.org/" or "http://doi.org/"
    doi_clean <- gsub("^(https?://(dx\\.)?doi\\.org/)", "", doi_clean, ignore.case = TRUE)
    
    # Construct the API URL using only the cleaned DOI
    url <- paste0("https://api.semanticscholar.org/graph/v1/paper/DOI:", doi_clean, "?fields=citationCount")
    
    response <- GET(url)
    if (response$status_code != 200) {
      message("Error: HTTP ", response$status_code, " for DOI ", doi)
      return(NA)
    }
    
    data <- fromJSON(content(response, as = "text", encoding = "UTF-8"))
    if (!is.null(data$citationCount)) {
      return(data$citationCount)
    } else {
      message("Citation count not found for DOI: ", doi)
      return(NA)
    }
  }
  
  # Get citation counts for each DOI and sum them (ignoring NAs)
  citation_counts <- sapply(doi_list, get_citation)
  total <- sum(citation_counts, na.rm = TRUE)
  return(total)
}


# ── Profile comparison ────────────────────────────────────────────────────────

#' Extract all DOIs from publications.md
#'
#' Returns a character vector of normalised DOIs (lowercase, no URL prefix).
extract_dois_from_md <- function(md_path) {
  text <- paste(readLines(md_path, encoding = "UTF-8"), collapse = "\n")
  # Matches bare DOIs like 10.1234/xxx AND those embedded in URLs
  raw <- regmatches(text, gregexpr("10\\.\\d{4,}/[^\\s)\"\\]>]+", text, perl = TRUE))[[1]]
  # Strip trailing punctuation that may have been captured
  raw <- gsub("[.,;:>]+$", "", raw)
  unique(tolower(trimws(raw)))
}

#' Fetch all papers for a Semantic Scholar author (handles pagination)
#'
#' @param author_id  Numeric/character S2 author ID (preferred).
#' @param author_name  Author name used for a lookup when ID is unknown.
#' @return Data frame with columns: title, year, doi (NA when absent).
fetch_semantic_scholar_author_papers <- function(author_id = NULL,
                                                  author_name = NULL) {
  # ── Resolve author ID if not supplied ────────────────────────────────────
  if (is.null(author_id)) {
    if (is.null(author_name)) stop("Provide author_id or author_name.")
    search_url <- "https://api.semanticscholar.org/graph/v1/author/search"
    resp <- GET(search_url, query = list(query = author_name,
                                         fields = "name,paperCount", limit = 5))
    if (resp$status_code != 200) {
      message("Semantic Scholar author search failed (HTTP ", resp$status_code, ")")
      return(NULL)
    }
    res <- fromJSON(content(resp, as = "text", encoding = "UTF-8"), flatten = TRUE)
    if (length(res$data) == 0) {
      message("No Semantic Scholar author found for: ", author_name)
      return(NULL)
    }
    author_id <- res$data$authorId[1]
    message("Semantic Scholar: using author '", res$data$name[1],
            "' (id=", author_id, ", ", res$data$paperCount[1], " papers)")
  }

  # ── Paginate through all papers ───────────────────────────────────────────
  base_url  <- paste0("https://api.semanticscholar.org/graph/v1/author/",
                       author_id, "/papers")
  fields    <- "title,year,externalIds"
  page_size <- 500
  offset    <- 0
  all_papers <- list()

  repeat {
    resp <- GET(base_url, query = list(fields = fields,
                                       limit = page_size, offset = offset))
    if (resp$status_code != 200) {
      message("Semantic Scholar papers fetch failed (HTTP ", resp$status_code,
              ") at offset ", offset)
      break
    }
    data <- fromJSON(content(resp, as = "text", encoding = "UTF-8"), flatten = TRUE)
    batch <- data$data
    if (is.null(batch) || nrow(batch) == 0) break
    all_papers <- append(all_papers, list(batch))
    offset <- offset + nrow(batch)
    if (nrow(batch) < page_size) break
    Sys.sleep(0.5)   # be polite to the API
  }

  if (length(all_papers) == 0) return(data.frame())
  papers <- bind_rows(all_papers)

  # externalIds.DOI is the column name after flatten = TRUE
  doi_col <- if ("externalIds.DOI" %in% names(papers)) papers$externalIds.DOI else NA
  data.frame(
    title = papers$title,
    year  = if ("year" %in% names(papers)) papers$year else NA,
    doi   = tolower(trimws(doi_col)),
    stringsAsFactors = FALSE
  )
}

#' Fetch all publications from Google Scholar via the `scholar` package
#'
#' Requires the `scholar` package to be installed.
#' @param gs_author_id  Google Scholar author ID (e.g. "7HzlVT4AAAAJ").
#' @return Data frame with columns: title, year, cites (citation count from GS).
fetch_google_scholar_author_papers <- function(gs_author_id) {
  if (!requireNamespace("scholar", quietly = TRUE))
    stop("Install the 'scholar' package first: install.packages('scholar')")
  pubs <- scholar::get_publications(gs_author_id)
  data.frame(
    title = trimws(pubs$title),
    year  = pubs$year,
    cites = pubs$cites,
    stringsAsFactors = FALSE
  )
}

#' Compare online scholar profiles to publications.md and report missing DOIs
#'
#' Identifies papers present in your Semantic Scholar or Google Scholar author
#' profiles whose DOIs are **not** found in publications.md.
#'
#' For Semantic Scholar the comparison is exact (DOI-to-DOI).
#' For Google Scholar, a fuzzy title match (Jaro-Winkler) is used because
#' Google Scholar does not expose DOIs directly; unmatched papers are then
#' looked up in Semantic Scholar to retrieve their DOI.
#'
#' @param publications_md_path   Path to publications.md.
#' @param gs_author_id           Google Scholar author ID string.
#' @param semantic_scholar_author_id   Numeric/character Semantic Scholar ID.
#' @param semantic_scholar_author_name  Author name for S2 lookup (fallback).
#' @param fuzzy_threshold  Jaro-Winkler distance below which a GS title is
#'   considered matched in publications.md (default 0.12).
#'
#' @return A named list with elements:
#'   \itemize{
#'     \item \code{dois_in_md}  – DOIs found in publications.md
#'     \item \code{semantic_scholar_missing} – data frame of S2 papers whose
#'       DOI is absent from publications.md
#'     \item \code{google_scholar_missing} – data frame of GS papers that could
#'       not be fuzzy-matched to any publications.md entry, with a \code{doi}
#'       column populated where Semantic Scholar could resolve it
#'   }
compare_scholar_profiles_to_publications <- function(
    publications_md_path,
    gs_author_id                 = NULL,
    semantic_scholar_author_id   = NULL,
    semantic_scholar_author_name = NULL,
    fuzzy_threshold              = 0.12) {

  if (!requireNamespace("stringdist", quietly = TRUE))
    stop("Install the 'stringdist' package: install.packages('stringdist')")

  # ── 1. Extract DOIs already in publications.md ────────────────────────────
  message("Extracting DOIs from publications.md ...")
  dois_in_md <- extract_dois_from_md(publications_md_path)
  message("  Found ", length(dois_in_md), " DOIs in publications.md")

  results <- list(dois_in_md = dois_in_md,
                  semantic_scholar_missing = NULL,
                  google_scholar_missing   = NULL)

  # ── 2. Semantic Scholar comparison (DOI-based) ───────────────────────────
  if (!is.null(semantic_scholar_author_id) || !is.null(semantic_scholar_author_name)) {
    message("Fetching Semantic Scholar papers ...")
    s2_papers <- fetch_semantic_scholar_author_papers(
      author_id   = semantic_scholar_author_id,
      author_name = semantic_scholar_author_name
    )
    if (!is.null(s2_papers) && nrow(s2_papers) > 0) {
      message("  Retrieved ", nrow(s2_papers), " papers from Semantic Scholar")
      s2_with_doi <- s2_papers[!is.na(s2_papers$doi) & s2_papers$doi != "", ]
      missing_mask <- !s2_with_doi$doi %in% dois_in_md
      results$semantic_scholar_missing <- s2_with_doi[missing_mask, ]
      message("  Papers in Semantic Scholar NOT in publications.md: ",
              nrow(results$semantic_scholar_missing))
    }
  }

  # ── 3. Google Scholar comparison (title substring + fuzzy fallback) ────────
  if (!is.null(gs_author_id)) {
    message("Fetching Google Scholar papers ...")
    gs_papers <- fetch_google_scholar_author_papers(gs_author_id)
    message("  Retrieved ", nrow(gs_papers), " papers from Google Scholar")

    # Keep only numbered publication lines from publications.md
    md_lines  <- readLines(publications_md_path, encoding = "UTF-8")
    pub_lines <- md_lines[grepl("^\\d+\\\\\\.", md_lines)]

    clean_str <- function(x) {
      x <- iconv(x, to = "ASCII//TRANSLIT", sub = "")   # normalise accents / en-dashes
      tolower(gsub("[^a-z0-9 ]", " ", x))
    }

    md_lines_clean  <- sapply(pub_lines, clean_str, USE.NAMES = FALSE)
    gs_titles_clean <- sapply(gs_papers$title, clean_str, USE.NAMES = FALSE)

    # Primary: check if the cleaned GS title is a substring of any MD line.
    # This is robust to punctuation differences (colons vs commas, dashes, etc.)
    # Fallback: Jaro-Winkler fuzzy match for slight wording variations.
    is_missing <- sapply(gs_titles_clean, function(t) {
      t <- trimws(gsub("\\s+", " ", t))
      if (nchar(t) < 5) return(FALSE)
      # substring match
      if (any(sapply(md_lines_clean, function(line) grepl(t, line, fixed = TRUE))))
        return(FALSE)
      # fuzzy fallback
      dists <- stringdist::stringdist(t, md_lines_clean, method = "jw")
      min(dists, na.rm = TRUE) > fuzzy_threshold
    })

    gs_missing <- gs_papers[is_missing, ]

    # For unmatched GS papers, try to retrieve their DOI via Semantic Scholar
    if (nrow(gs_missing) > 0) {
      message("  Looking up DOIs for ", nrow(gs_missing),
              " unmatched Google Scholar paper(s) via Semantic Scholar ...")
      gs_missing$doi <- sapply(gs_missing$title, function(title) {
        url    <- "https://api.semanticscholar.org/graph/v1/paper/search"
        params <- list(query = title, fields = "externalIds", limit = 1)
        resp   <- GET(url, query = params)
        if (resp$status_code != 200) return(NA_character_)
        d <- fromJSON(content(resp, as = "text", encoding = "UTF-8"),
                      flatten = TRUE)
        if (length(d$data) == 0) return(NA_character_)
        doi_val <- d$data$externalIds.DOI[1]
        if (is.null(doi_val) || is.na(doi_val)) return(NA_character_)
        Sys.sleep(0.3)
        tolower(trimws(doi_val))
      })
    } else {
      gs_missing$doi <- character(0)
    }

    results$google_scholar_missing <- gs_missing
    message("  Papers in Google Scholar NOT matched in publications.md: ",
            nrow(gs_missing))
  }

  # ── 4. Print a tidy summary ───────────────────────────────────────────────
  message("\n── Summary ──────────────────────────────────────────────────────────")

  if (!is.null(results$semantic_scholar_missing) &&
      nrow(results$semantic_scholar_missing) > 0) {
    message("\nSemantic Scholar papers missing from publications.md:")
    for (i in seq_len(nrow(results$semantic_scholar_missing))) {
      row <- results$semantic_scholar_missing[i, ]
      message("  [", row$year, "] ", row$title)
      message("       DOI: ", row$doi)
    }
  }

  if (!is.null(results$google_scholar_missing) &&
      nrow(results$google_scholar_missing) > 0) {
    message("\nGoogle Scholar papers not matched in publications.md:")
    for (i in seq_len(nrow(results$google_scholar_missing))) {
      row <- results$google_scholar_missing[i, ]
      doi_str <- if (!is.na(row$doi)) row$doi else "(DOI not found)"
      message("  [", row$year, "] ", row$title)
      message("       DOI: ", doi_str)
    }
  }

  invisible(results)
}


# ── Original citation-count helpers ───────────────────────────────────────────

fetch_semantic_scholar_citations_software <- function(software_name, authors = NULL) {
  # Construct a search query
  # For example: "bioregion Lenormand Leroy Denelle"
  query <- if (!is.null(authors) && authors != "") {
    paste(software_name, authors)
  } else {
    software_name
  }
  
  base_url <- "https://api.semanticscholar.org/graph/v1/paper/search"
  params <- list(query = query, fields = "title,citationCount", limit = 1)
  response <- GET(url = base_url, query = params)
  
  if (response$status_code != 200) {
    message("Error: HTTP ", response$status_code, " while searching for software: ", query)
    return(NA)
  }
  
  data <- fromJSON(content(response, as = "text", encoding = "UTF-8"), flatten = TRUE)
  
  # Check if we have any results
  if (!"data" %in% names(data) || length(data$data) == 0) {
    message("No search results found for query: ", query)
    return(NA)
  }
  
  # Extract the citationCount from the first result
  citation_count <- data$data$citationCount[1]
  return(citation_count)
}

