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
