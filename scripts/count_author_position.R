library(stringr)

# Function to read and process the publications file
read_publications <- function(file_path) {
  # Read the entire file into a single string
  content <- readLines(file_path)
  
  content <- content[!grepl("^`\\d{4}`$", content)]
  
  # Combine all lines into a single string, then split by two consecutive newlines
  combined_content <- paste(content, collapse = "\n")
  publications <- str_split(combined_content, "\\n\\n")[[1]]
  
  # Keep only peer-reviewed publications
  publications <- publications[
    (which(publications == "<!-- start_pub_list -->") + 1):
       (which(publications == "<!-- end_pub_list -->") - 1)
  ]
  
  publications <- publications[which(publications != "")]
  
  return(publications)
}

# Function to read and process the publications file
read_software  <- function(file_path) {
  # Read the entire file into a single string
  content <- readLines(file_path)
  
  content <- content[!grepl("^`\\d{4}`$", content)]
  
  # Combine all lines into a single string, then split by two consecutive newlines
  combined_content <- paste(content, collapse = "\n")
  publications <- str_split(combined_content, "\\n\\n")[[1]]
  
  # Keep only peer-reviewed publications
  publications <- publications[
    (which(publications == "<!-- start_soft_list -->") + 1):
      (which(publications == "<!-- end_soft_list -->") - 1)
  ]
  
  publications <- publications[which(publications != "")]
  
  return(publications)
}

read_expertise <- function(file_path) {
  # Read the entire file into a single string
  content <- readLines(file_path)
  
  content <- content[!grepl("^`\\d{4}`$", content)]
  
  # Combine all lines into a single string, then split by two consecutive newlines
  combined_content <- paste(content, collapse = "\n")
  publications <- str_split(combined_content, "\\n\\n")[[1]]
  
  # Keep only peer-reviewed publications
  publications <- publications[
    (which(publications == "<!-- start_exp_list -->") + 1):
      (which(publications == "<!-- end_exp_list -->") - 1)
  ]
  
  publications <- publications[which(publications != "")]
  
  return(publications)
}

read_publications <- function(file_path) {
  # Read the entire file into a single string
  content <- readLines(file_path)
  
  content <- content[!grepl("^`\\d{4}`$", content)]
  
  # Combine all lines into a single string, then split by two consecutive newlines
  combined_content <- paste(content, collapse = "\n")
  publications <- str_split(combined_content, "\\n\\n")[[1]]
  
  # Keep only peer-reviewed publications
  publications <- publications[
    (which(publications == "<!-- start_pub_list -->") + 1):
      (which(publications == "<!-- end_pub_list -->") - 1)
  ]
  
  publications <- publications[which(publications != "")]
  
  return(publications)
}



# Function to count first and last author publications
count_author_positions <- function(publications, name) {
  first_author_count <- 0
  second_author_count <- 0
  last_author_count <- 0
  
  for (pub in publications) {
    authors <- str_extract(pub, "^[^\\.]+\\.\\s*(.+?)\\.")
    # cat("\n\n")
    # cat(pub, "\n")
    # cat(authors, "\n\n")
    if (!is.na(authors)) {
      author_list <- str_split(authors, ",\\s*")[[1]]
      author_list <- str_replace_all(author_list, "\\.\\s*", "")
      
      
      # cat(author_list, "\n")
      # cat("First author: ", grepl(name, author_list[1]), "\n")
      # 
      # cat("Longueur liste: ", length(author_list), "\n")  
      # cat("Last author: ", grepl(name, author_list[length(author_list)]), "\n\n\n")
      
      if (grepl(name, author_list[1])) {
        first_author_count <- first_author_count + 1
      }
      
      if (length(author_list) > 2 && grepl(name, author_list[2])) {
        second_author_count <- second_author_count + 1
        cat(pub, "\n")
      }

      
      # Count as last author only if there are multiple authors
      if (length(author_list) > 1 && grepl(name, author_list[length(author_list)])) {
        last_author_count <- last_author_count + 1
      }
      
      
    }
  }
  
  # Add one paper as a co-last author (bioregion in MEE)
  last_author_count <- last_author_count + 1
  
  return(list(first_author = first_author_count, 
              second_author = second_author_count,
              last_author = last_author_count))
}



library(httr)

# Function to query Google Scholar for a given query string and extract the "Cited by" count
get_google_scholar_citation <- function(query) {
  # Construct the search URL
  url <- paste0("https://scholar.google.com/scholar?q=", URLencode(query))
  
  # Use a common browser user agent to get the full page
  ua <- user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/90.0.4430.93 Safari/537.36")
  
  # Fetch the page using httr::GET() with the user agent
  response <- GET(url, ua)
  
  if (response$status_code != 200) {
    message("Error fetching page: HTTP ", response$status_code)
    return(NA)
  }
  
  page <- read_html(response)
  
  # Look for an anchor element whose href contains "/scholar?cites="
  citation_elem <- page %>% html_elements("a[href*='/scholar?cites=']")
  
  if (length(citation_elem) == 0) {
    message("Citation element not found.")
    return(NA)
  }
  
  # Get the text from the first citation element (e.g., "Cité 2 fois")
  citation_text <- html_text(citation_elem)[1]
  
  # Extract the first number from the text
  count <- str_extract(citation_text, "\\d+")
  if (is.na(count)) {
    message("No citation count found in: ", citation_text)
    return(NA)
  }
  
  return(as.numeric(count))
}

library(stringr)
library(dplyr)

# Define a function to parse one entry
parse_entry <- function(entry) {
  # Replace newline characters with spaces
  entry_clean <- gsub("\n", " ", entry)
  
  # Remove extraneous backslashes so that "5\\." becomes "5."
  entry_clean_mod <- gsub("\\\\", "", entry_clean)
  
  # --- Extract authors ---
  # Capture everything between the entry number and the year range.
  authors_pattern <- "^[0-9]+\\.\\s*(.*?)\\s*[0-9]{4}-[0-9]{4}"
  authors <- str_match(entry_clean_mod, authors_pattern)[,2]
  authors <- str_replace_all(authors, "\\*\\*", "")
  authors <- str_trim(authors)
  authors <- sub("[[:punct:]]$", "", authors)
  
  # --- Extract software name ---
  # This pattern grabs a short token immediately after the year range.
  software_name_pattern <- "[0-9]{4}-[0-9]{4}\\.\\s*(?:\\*\\*)?([\\w\\.-]+)(?:\\*\\*)?[,:\\s]"
  software_name <- str_match(entry_clean_mod, software_name_pattern)[,2]
  
  # --- Extract full software title ---
  # This pattern captures text from after the year range until "Current version".
  # It should capture the complete title, e.g., "bioregion: Comparison of Bioregionalisation Methods."
  title_pattern <- "[0-9]{4}-[0-9]{4}\\.\\s*(.*?)(?=\\s*Current version)"
  software_title <- str_match(entry_clean_mod, title_pattern)[,2]
  software_title <- str_trim(software_title)
  software_title <- str_replace_all(software_title, "\\*\\*", "")
  
  # --- Extract software URL ---
  # Get all URLs (delimited by < and >)
  urls <- str_extract_all(entry_clean, "<(http[^>]+)>")[[1]]
  # Remove the angle brackets from URLs
  urls <- gsub("[<>]", "", urls)
  # Assume the first URL that does NOT contain 'doi.org' is the package URL.
  software_url <- NA
  for(u in urls) {
    if (!grepl("doi.org", u, ignore.case = TRUE)) {
      software_url <- u
      break
    }
  }
  
  # --- Extract DOI(s) for peer-reviewed studies ---
  dois <- str_extract_all(entry_clean, "<(https?://doi.org/[^>]+)>")[[1]]
  dois <- gsub("[<>]", "", dois)
  doi_peer_review <- if(length(dois) > 0) paste(dois, collapse = "; ") else NA
  
  # Return a one-row data.frame with the extracted values
  return(data.frame(
    authors = authors,
    software_name = software_name,
    software_title = software_title,
    software_url = software_url,
    doi_peer_review = doi_peer_review,
    stringsAsFactors = FALSE
  ))
}



# Helper function to handle one or more DOIs (if multiple, separated by semicolons)
get_citations_from_doi <- function(dois) {
  if (is.na(dois) || dois == "") return(NA)
  # Split into individual DOIs and remove extra whitespace
  doi_list <- unlist(strsplit(dois, ";"))
  doi_list <- trimws(doi_list)
  
  # For each DOI, query Google Scholar using a search string like "doi:<DOI>"
  counts <- sapply(doi_list, function(doi) {
    query <- paste0("doi:", doi)
    get_google_scholar_citation(query)
  })
  # Return the total citations (sum all counts)
  total <- sum(counts, na.rm = TRUE)
  return(total)
}

