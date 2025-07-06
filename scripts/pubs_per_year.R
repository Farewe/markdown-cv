pub_per_year <- function(pubs) {
  library(stringr)
  library(dplyr)
  
  # Step 1: Extract 4-digit year
  years <- str_extract(pubs, "\\b(20\\d{2}|19\\d{2})\\b")
  years_num <- as.integer(na.omit(years))
  
  # Step 2: Handle "In Press"
  in_press_count <- sum(str_detect(pubs, regex("In Press", ignore_case = TRUE)))
  
  # If we have valid years, use max; otherwise, use current year
  latest_year <- if (length(years_num) > 0) max(years_num) else as.integer(format(Sys.Date(), "%Y"))
  
  # Step 3: Combine regular years and synthetic "In Press" years
  all_years <- c(years_num, rep(latest_year, in_press_count))
  
  # Step 4: Count per year
  pubs_per_year <- as.data.frame(table(all_years)) %>%
    mutate(year = as.integer(as.character(all_years))) %>%
    arrange(year)
  
  return(pubs_per_year)
}
