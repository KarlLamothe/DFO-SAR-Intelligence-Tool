#===============================================================================
# CSAS Publications Extraction and SAR Species Matching
#
# Purpose:
#   1. Download all CSAS publications from the CSAS API
#   2. Build a searchable publication database
#   3. Create species aliases from SAR Registry species records
#   4. Match CSAS publications to SAR species
#   5. Flag SAR-related CSAS publications (RPA, critical habitat, etc.)
#
# Outputs:
#   csas_publications
#   species_aliases
#   matches
#===============================================================================

library(httr2)
library(jsonlite)
library(dplyr)
library(purrr)
library(stringr)
library(tidyr)

#===============================================================================
# Helper Functions
#===============================================================================

# Remove HTML tags from API text fields
clean_html <- function(x) {
  ifelse(is.na(x), NA_character_, x |>
      gsub("<[^>]+>", "", x = _) |>
      trimws())
}

# Download a single page of CSAS publications
get_csas_page <- function(page_index = 0, max_attempts = 5) {
  url <- paste0("https://csas-scas-api.dfo-mpo.gc.ca/api/publication?",
                "pageIndex=", page_index, "&pageSize=100", "&language=En")
  attempt <- 1
  while (attempt <= max_attempts) {
    result <- tryCatch(fromJSON(url)$items, error = function(e) NULL)
    if (!is.null(result)) {
      return(result)
    }
    message(sprintf("Page %s failed (attempt %s/%s). Retrying...",
                    page_index, attempt, max_attempts))
    Sys.sleep(3)
    attempt <- attempt + 1
  }
  
  stop(sprintf("Failed to download page %s after %s attempts.",
               page_index, max_attempts))
}

# Helper to get URLS
get_csas_details <- function(publication_id) {
  url <- paste0("https://csas-scas-api.dfo-mpo.gc.ca/api/publication/",
    publication_id)
  tryCatch(fromJSON(url), error = function(e) NULL)
}

#===============================================================================
# Download All CSAS Publications
#===============================================================================

page_count <- fromJSON(
  "https://csas-scas-api.dfo-mpo.gc.ca/api/publication?pageIndex=0&pageSize=100&language=En"
)$paginationInfo$pageCount

message("Downloading CSAS publications...")

all_csas <- map_dfr(0:(page_count - 1),
                    function(i){
                      Sys.sleep(0.5)
                      get_csas_page(page_index = i, max_attempts = 5)})

message(sprintf("Downloaded %s publications.", nrow(all_csas)))

#===============================================================================
# Extract Meeting Titles
#===============================================================================

meeting_titles <- map_chr(
  all_csas$associatedMeetingInfo,
  function(x) {
    if (length(x) == 0) {
      return(NA_character_)
    }
    x$meetingTitle$english[1]
  })

#===============================================================================
# Build Publication Database
#===============================================================================

csas_publications <- tibble(
  publication_id = all_csas$id,
  publication_number = all_csas$publicationNumber,
  publication_date = as.Date(all_csas$publishedDate),
  publication_year = all_csas$publishedYear,
  publication_type = all_csas$series$title$english,
  title = clean_html(all_csas$title$english),
  meeting_title = clean_html(meeting_titles)) %>%
  mutate(search_text = str_to_lower(paste(title, meeting_title)))

#===============================================================================
# Load Species Data
#===============================================================================

all_species <- read.csv("data_processed/all_species_master.csv")

#===============================================================================
# Reformat previous names
#===============================================================================
colnames(all_species)

# reformat previous common names
extract_previous_names <- function(x){
  if(is.na(x) || x == "" || x == "character(0)"){
    return(character(0))
  }
  # decode html entities
  x <- gsub("&lt;", "<", x)
  x <- gsub("&gt;", ">", x)
  # remove html tags
  x <- gsub("<[^>]+>", "", x)
  # handle c("a","b","c") format
  if(grepl("^c\\(", x)){
    out <- tryCatch(eval(parse(text = x)), error = function(e) character(0))
    out <- gsub('\\"', "", out)
    return(trimws(out))
  }
  trimws(x)
}

previous_aliases <- all_species %>%
  select(species_id = id, species_name = recordName, previous_common_name) %>%
  rowwise() %>%
  mutate(alias = list( extract_previous_names(previous_common_name))) %>%
  unnest(alias) %>%
  mutate(alias_source = "previous_common") %>%
  mutate(alias = stringr::str_squish(alias)) %>%
  filter(!is.na(alias), alias != "") %>%
  # remove generic population-only aliases
  filter(!grepl(" population$| populations$", alias, ignore.case = TRUE)) %>%
  distinct(species_id, alias, .keep_all = TRUE)

# reformat previous scientific names
extract_previous_scientific_names <- function(x){
  if(is.na(x) || x == "" || x == "character(0)"){
    return(character(0))
  }
  # decode encoded HTML entities
  x <- gsub("&lt;", "<", x)
  x <- gsub("&gt;", ">", x)
  if(grepl("^c\\(", x)){
    out <- tryCatch(eval(parse(text = x)), error = function(e) character(0))
  } else {
    out <- x
  }
  # remove html tags
  out <- gsub("<[^>]+>", "", out)
  # remove escaped quotes
  out <- gsub('\\"', "", out)
  out <- trimws(out)
  out
}

previous_scientific_aliases <- all_species %>%
  select(species_id = id, species_name = recordName, previous_scientific_name) %>%
  rowwise() %>%
  mutate(alias = list(extract_previous_scientific_names(
     previous_scientific_name))) %>%
  unnest(alias) %>%
  mutate(alias_source = "previous_scientific") %>%
  mutate(alias = stringr::str_squish(alias)) %>%
  filter(!is.na(alias), alias != "") %>%
  distinct(species_id, alias, .keep_all = TRUE)

#===============================================================================
# Build Species Alias Table
#===============================================================================

species_aliases <- bind_rows(
  all_species %>%
    select(species_id = id, species_name = recordName, alias = commonName) %>%
    mutate(alias_source = "common"),
  all_species %>%
    select(species_id = id, species_name = recordName, alias = cosewicCommonName) %>%
    mutate(alias_source = "cosewic_common"),
  all_species %>%
    select(species_id = id, species_name = recordName, alias = legalCommonName) %>%
    mutate(alias_source = "legal_common"),
  all_species %>%
    select(species_id = id, species_name = recordName, alias = cosewicScientificName) %>%
    mutate(alias_source = "cosewic_scientific"),
  all_species %>%
    select(species_id = id, species_name = recordName, alias = legalScientificName) %>%
    mutate(alias_source = "legal_scientific"),
  previous_aliases,
  previous_scientific_aliases) %>%
  filter(!is.na(alias), alias != "") %>%
  mutate(alias = stringr::str_squish(alias)) %>%
  distinct(species_id, alias, .keep_all = TRUE)

#===============================================================================
# Species-Publication Matching
#===============================================================================

message("Matching publications to SAR species...")

matches <- map_dfr(
  seq_len(nrow(species_aliases)),
  function(i) {
    alias <- species_aliases$alias[i]
    hits <- csas_publications %>%
      filter(str_detect(search_text, fixed(str_to_lower(alias))))
    if (nrow(hits) == 0) {
      return(NULL)
    }
    hits %>%
      mutate(species_id = species_aliases$species_id[i],
             species_name = species_aliases$species_name[i],
             matched_alias = alias,
             alias_source = species_aliases$alias_source[i])})

#===============================================================================
# Assign Match Confidence
#===============================================================================

matches <- matches %>%
  mutate(confidence = case_when(
    alias_source %in% c("cosewic_scientific", "legal_scientific", "previous_scientific") ~ "High",
    alias_source %in% c("common", "cosewic_common", "legal_common","previous_common") ~ "Medium",
    TRUE ~ "Low"))

#===============================================================================
# Species-at-Risk Publication Flags
#===============================================================================

sar_terms <- c("recovery potential", "recovery potential assessment",
               "species at risk", "allowable harm",
               "critical habitat", "residence description")

csas_publications <- csas_publications %>%
  mutate(sar_related = str_detect(search_text, paste(sar_terms, collapse = "|")))

#===============================================================================
# Coverage Summary
#===============================================================================

species_coverage <- all_species %>%
  transmute(species_id = id, species_name = recordName) %>%
  left_join(matches %>%
      count(species_id), by = "species_id") %>%
  mutate(n = replace_na(n, 0))

#===============================================================================
# Get the URLS
#===============================================================================

pdf_candidates <- bind_rows(
  matches %>%
    select(publication_id),
  csas_publications %>%
    filter(sar_related) %>%
    select(publication_id)) %>%
  distinct()

get_csas_pdf <- function(publication_id){
  out <- tryCatch({
    x <- jsonlite::fromJSON(
      paste0("https://csas-scas-api.dfo-mpo.gc.ca/api/publication/", publication_id))
    docs <- x$publicationDocuments
    if(is.null(docs) || nrow(docs) == 0){
      return(NA_character_)
    }
    pdf <- docs$pdfUrl[1]
    if(length(pdf) == 0 || pdf == ""){
      NA_character_
    } else {
      pdf
    }
  },
  error = function(e) NA_character_)
  out
}

# NOTE: THIS WILL TAKE TIME
pdf_lookup <- purrr::map_dfr(
  seq_along(pdf_candidates$publication_id),
  function(i){
    if(i %% 50 == 0){
      message(sprintf("%s / %s", i,nrow(pdf_candidates)))
    }
    tibble(publication_id = pdf_candidates$publication_id[i],
           pdf_url = get_csas_pdf(pdf_candidates$publication_id[i]))
  }
)

csas_publications <- csas_publications %>%
  left_join(pdf_lookup, by = "publication_id")

#===============================================================================
# Export Results
#===============================================================================

write.csv(csas_publications, "data_processed/csas_publications.csv", row.names = FALSE)
write.csv(matches, "data_processed/csas_species_links.csv", row.names = FALSE)
write.csv(filter(csas_publications, sar_related),
          "data_processed/csas_sar_publications.csv", row.names = FALSE)

message(sprintf(
  "CSAS extraction complete: %s publications, %s species-publication matches.",
  nrow(csas_publications), nrow(matches)))

#===============================================================================
# Summary stats
#===============================================================================

message(sprintf("Downloaded %s CSAS publications.", nrow(csas_publications)))
message(sprintf("Matched %s publications to SAR species.", n_distinct(matches$publication_id)))
message(sprintf("Identified %s SAR-related publications.", sum(csas_publications$sar_related)))
