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

