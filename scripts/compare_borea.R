library(rvest)
library(httr)
library(dplyr)

# 1. Charger la page web
url <- "https://borea.mnhn.fr/fr/users/boris-leroy"
page <- read_html(url)

# 2. Identifier les blocs contenant les publications
# Les publications sont dans des <div> avec class "views-row"
refs <- page %>% html_element("div") %>%
  html_text2()

refs <- strsplit(refs, split = "Mes publications")[[1]][2]
refs <- strsplit(refs, split = "Archives")[[1]][1]
refs <- strsplit(refs, split = "\n")[[1]]
refs <- refs[2:length(refs)]

sapply(refs, function(x, split) strsplit(x, split = split)[[1]][2],
       split = ").")


# Packages nécessaires
library(stringr)
library(dplyr)
library(stringdist)

# 1. Nettoyage léger des deux vecteurs
clean_string <- function(x) {
  x %>%
    str_to_lower() %>%
    str_replace_all("[^a-z0-9 ]", " ") %>%
    str_squish()
}

refs_clean <- sapply(refs, clean_string)
pubs_clean <- sapply(pubs, clean_string)

# 2. Extraire le titre des `pubs` (en supprimant numéro, auteurs, année)
extract_title <- function(x) {
  x %>%
    str_replace("^\\d+\\.", "") %>%
    str_replace_all("\\*\\*", "") %>%  # enlever mise en gras markdown
    str_replace_all("in press|\\(\\d{4}\\)|\\d{4}", "") %>%  # supprimer les années
    str_replace_all("\\. *", ". ") %>%
    str_replace("^.*?\\. ", "") %>%    # retirer les auteurs
    str_replace("\\. *[a-z].*", "") %>%  # garder le titre seul
    clean_string()
}

titles_pubs <- sapply(pubs, extract_title)

# 3. Fuzzy matching pour trouver si chaque titre est présent dans `refs_clean`
find_missing_pubs <- function(titles, refs) {
  sapply(titles, function(title) {
    distances <- stringdist(title, refs, method = "jw")  # Jaro-Winkler distance
    min_dist <- min(distances)
    if (min_dist < 0.15) {
      return(NA)  # trouvé (distance faible)
    } else {
      return(title)  # manquant
    }
  }) %>% na.omit()
}

missing_titles <- find_missing_pubs(titles_pubs, refs_clean)

# 4. Affichage
missing_titles