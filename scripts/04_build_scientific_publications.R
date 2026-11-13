source('scripts/00_setup.R')

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

################################################################################
################################################################################
# CWS Series
################################################################################
################################################################################
cws_series <- tribble(
  ~series_id, ~series_name,
  "9.504438", "Technical report series",
  "9.514489", "Occasional papers",
  "9.859941", "Wildlife management bulletin")

get_cws_page <- function(series_id, page = 1, max_attempts = 3){
  url <- sprintf(
    "https://publications.gc.ca/site/eng/%s/issues.html?page=%s&results_per_page=10&sort=catalogue_number&order=descending",
    series_id, page)
  
  for(attempt in seq_len(max_attempts)){
    result <- tryCatch({
      httr2::request(url) %>%
        httr2::req_perform() %>%
        httr2::resp_body_string()
    }, error = function(e) NULL)
    if(!is.null(result)){
      return(result)
    }
    Sys.sleep(2 * attempt)
  }
  warning(sprintf("Failed page %s", page))
  NULL
}

parse_cws_page <- function(x){
  if(is.null(x)){
    return(tibble())
  }
  titles <- str_match_all(x,
    '\\\\\\\"title_display\\\\\\\":\\\\\\\"(.*?)\\\\\\\"')[[1]][,2]
  ids <- str_match_all(x,
    '\\\\\\\"id\\\\\\\":\\\\\\\"(9\\.[0-9]+)\\\\\\\"')[[1]][,2]
  tibble(publication_id = ids[-c(1,2)], title = titles[-1])
}

extract_total_results <- function(x){
  as.numeric(stringr::str_match(x, 'totalResults\\\\":([0-9]+)')[,2])
}

get_cws_series <- function(series_id, series_name){
  first_page <- get_cws_page(series_id, 1)
  n_pages <- ceiling(extract_total_results(first_page) / 10)
  map_dfr(seq_len(n_pages), function(i){
      message(sprintf("%s page %s/%s", series_name, i, n_pages))
      Sys.sleep(0.5)
      parse_cws_page(get_cws_page(series_id, i))
    }) %>%
    mutate(publication_series = series_name)
}

# scrape
cws_publications <- map2_dfr(cws_series$series_id,
                             cws_series$series_name,
                             get_cws_series)
colnames(cws_publications)

cws_publications <- cws_publications %>%
  mutate(search_text = stringr::str_to_lower(paste(title, publication_id)),
    publication_source = "Canadian Wildlife Service")

cws_publications <- cws_publications %>%
  mutate(publication_url = sprintf(
      "https://publications.gc.ca/site/eng/%s/publication.html", publication_id))

message("Matching CWS publications to SAR species...")

cws_matches <- purrr::map_dfr(
  seq_len(nrow(species_aliases)),
  function(i){
    alias <- species_aliases$alias[i]
    hits <- cws_publications %>%
      filter(str_detect(search_text, regex(
        paste0("\\b",stringr::str_replace_all(alias, "\\.", "\\\\."), "\\b"),
            ignore_case = TRUE)))
    if(nrow(hits) == 0){
      return(NULL)
    }
    bind_cols(species_aliases[i, ],  hits)
  })

saveRDS(cws_publications,"data_processed/cws_publications.rds")
saveRDS(cws_matches,"data_processed/cws_species_links.rds")
write.csv(cws_publications, "data_processed/cws_publications.csv", row.names = FALSE)
write.csv(cws_matches, "data_processed/cws_species_links.csv", row.names = FALSE)

################################################################################
# DFO SERIES
################################################################################
report_series <- tibble::tribble(
  ~series_name, ~base_url,
  
  "Technical Reports",
  "https://dfo-mpo.summon.serialssolutions.com/api/search?screen_res=W1018H1142&__refererURL=&pn=1&ho=t&include.ft.matches=t&fvf%5B%5D=Library%2CDFO-MPO%2Cf&fvf%5B%5D=IsFullText%2Ctrue%2Cf&l=en&q=(PublicationSeriesTitle%3A(Canadian%20technical%20report%20of%20fisheries%20and%20aquatic%20sciences))%20OR%20(PublicationSeriesTitle%3A(Rapport%20technique%20canadien%20des%20sciences%20halieutiques%20et%20aquatiques))%20OR%20(ISSN%3A(07066457%20OR%2014885379%20OR%2007066570%20OR%201488545X))&sort=PublicationDate%3Adesc",
  
  "Manuscript Reports",
  "https://dfo-mpo.summon.serialssolutions.com/api/search?screen_res=W1018H1142&__refererURL=https%3A%2F%2Ffsl-bsf.scitech.gc.ca%2F&pn=1&ho=t&include.ft.matches=f&fvf%5B%5D=IsFullText%2Ctrue%2Cf&l=en&q=(%5C(%5C(PublicationSeriesTitle%3A%5C(%5C%5C(Canadian%20manuscript%20report%20of%20fisheries%20and%20aquatic%20sciences%5C%5C)%20OR%20%5C%5C(Rapport%20manuscrit%20canadien%20des%20sciences%20halieutiques%20et%20aquatiques%5C%5C)%5C)%20OR%20%5C(IssnAll%3A%5C(%22%5C%5C(07066589%20OR%2014885468%20OR%2007066473%20OR%2014885387%5C%5C)%22%5C)%5C)%5C))%20NOT%20(PublicationSeriesTitle%3A(Canadian%20technical%20report%20of%20fisheries%20and%20aquatic%20sciences)",
  
  "Data Reports",
  "https://dfo-mpo.summon.serialssolutions.com/api/search?screen_res=W1018H1142&__refererURL=&pn=1&ho=t&include.ft.matches=t&fvf%5B%5D=SourceType%2CLibrary%20Catalog%2Cf&fvf%5B%5D=Library%2CDFO-MPO%2Cf&fvf%5B%5D=IsFullText%2Ctrue%2Cf&l=en&q=(ISSN%3A(07066465%20OR%2014885395%20OR%2007066597%20OR%2014885506))%20OR%20(PublicationSeriesTitle%3A(Canadian%20data%20report%20of%20fisheries%20and%20aquatic%20sciences))%20OR%20(PublicationSeriesTitle%3A(Rapport%20statistique%20canadien%20des%20sciences%20halieutiques%20et%20aquatiques))&sort=PublicationDate%3Adesc"
)

get_fsln_page <- function(url, page){
  page_url <- gsub("pn=[0-9]+", paste0("pn=", page), url)
  resp <- httr2::request(page_url) %>%
    httr2::req_perform()
  jsonlite::fromJSON(httr2::resp_body_string(resp))
}

get_fsln_series <- function(url, series_name){
  first_page <- get_fsln_page(url, 1)
  n_pages <- first_page$page_count
  purrr::map_dfr(seq_len(n_pages), function(i){
    message(sprintf("%s page %s/%s", series_name, i, n_pages))
    page <- get_fsln_page(url, i)
    if(is.null(page)){
      return(NULL)
    }
    page$documents %>%
      mutate(report_series = series_name)})
}

all_reports <- purrr::map2_dfr(report_series$base_url,
                               report_series$series_name,
                               get_fsln_series)
saveRDS(all_reports,"data_processed/data_manuscript_tech_reports.rds")
all_reports <- readRDS("data_processed/data_manuscript_tech_reports.rds")

fsln_publications <- all_reports %>%
  transmute(
    publication_id = external_document_id,
    title,
    publication_series = publication_series_title,
    publication_year = publication_years,
    publication_date,
    snippet,
    abstracts,
    subject_terms,
    pdf_url = purrr::map_chr(uris, ~ if(length(.x) > 0) .x[1] 
                             else NA_character_),
    report_series)

fsln_publications <- fsln_publications %>%
  mutate(publication_source = "DFO Science")

fsln_publications <- fsln_publications %>%
  mutate(subject_text = purrr::map_chr(subject_terms, ~ paste(.x, collapse = " ")),
         search_text = stringr::str_to_lower(
           paste(title, snippet, abstracts, subject_text)))

fsln_publications_export <- fsln_publications %>%
  mutate(across(where(is.list), ~ purrr::map_chr(.x, function(x) {
    if (is.null(x) || length(x) == 0) {
      return(NA_character_)
    }
    paste(as.character(unlist(x)), collapse = "; ")
  })))

#===============================================================================
# Species-Publication Matching
#===============================================================================

message("Matching publications to SAR species...")

matches <- map_dfr(
  seq_len(nrow(species_aliases)),
  function(i) {
    alias <- species_aliases$alias[i]
    hits <- fsln_publications %>%
      filter(str_detect(search_text, fixed(str_to_lower(alias))))
    if (nrow(hits) == 0) {
      return(NULL)
    }
    hits %>%
      mutate(species_id = species_aliases$species_id[i],
             species_name = species_aliases$species_name[i],
             matched_alias = alias,
             alias_source = species_aliases$alias_source[i])})

matches <- matches %>%
  left_join(all_species %>%
              transmute(species_id = id, 
                        common_name = common_name, 
                        scientific_name = scientific_name),
            by = "species_id")

#===============================================================================
# Coverage Summary
#===============================================================================

species_coverage <- all_species %>%
  transmute(species_id = id, species_name = recordName) %>%
  left_join(matches %>%
              count(species_id), by = "species_id") %>%
  mutate(n = replace_na(n, 0))

#===============================================================================
# Consolidate Species-Publication Matches
#===============================================================================

matches_clean <- matches %>%
  group_by(species_id, publication_id) %>%
  mutate(matched_aliases = paste(unique(matched_alias), collapse = "; "),
         alias_sources = paste(unique(alias_source), collapse = "; ")) %>%
  slice(1) %>%
  ungroup()

matches_clean <- matches_clean %>%
  mutate(parent_species = species_name %>% 
           str_remove("\\s*-\\s*.*$") %>% 
           str_squish())

species_publications <- matches_clean %>%
  group_by(parent_species, publication_id) %>%
  mutate(species_names = paste(unique(species_name), collapse = "; "),
         matched_aliases = paste(unique(matched_aliases), collapse = "; "),
         alias_sources = paste(unique(alias_sources), collapse = "; ")) %>%
  slice(1) %>%
  ungroup()

#===============================================================================
# Export Results
#===============================================================================
matches_export <- matches %>%
  mutate(across(where(is.list), ~ purrr::map_chr(.x, function(x) {
    if (is.null(x) || length(x) == 0) {
      return(NA_character_)
    }
    paste(as.character(unlist(x)), collapse = "; ")
  })))

species_publications_export <- species_publications %>%
  mutate(across(where(is.list), ~ purrr::map_chr(.x, function(x) {
    if (is.null(x) || length(x) == 0) {
      return(NA_character_)
    }
    paste(as.character(unlist(x)), collapse = "; ")
  })))

write.csv(matches_export, "data_processed/science_species_links.csv", row.names = FALSE)
write.csv(species_publications_export, "data_processed/science_sar_publications.csv", row.names = FALSE)

#################################################################################
# combine cws and dfo
#################################################################################
fsln_publications_combined <- fsln_publications %>%
  mutate(publication_year = purrr::map_chr(publication_year,
      ~ if(length(.x) == 0 || is.null(.x))  NA_character_
      else
        as.character(.x[[1]]))) %>%
  transmute(
    publication_id,
    title,
    publication_series = report_series,
    publication_year,
    publication_source,
    publication_url = pdf_url)

cws_publications_combined <- cws_publications %>%
  transmute(
    publication_id,
    title,
    publication_series,
    publication_year = NA_character_,
    publication_source,
    publication_url
  )

all_publications <- bind_rows(fsln_publications_combined, cws_publications_combined)

# Species link
science_links <- matches %>%
  select(species_id, species_name, scientific_name, publication_id,
         matched_alias, alias_source)

cws_links <- cws_matches %>%
  select(species_id, species_name, publication_id, alias, alias_source) %>%
  rename(matched_alias = alias) %>%
  left_join(all_species %>%
      transmute(species_id = id, scientific_name = scientific_name),
    by = "species_id")

all_species_links <- bind_rows(science_links, cws_links)

################################################################################
# Export
################################################################################
saveRDS(all_publications, "data_processed/all_science_publications.rds")
saveRDS(all_species_links, "data_processed/all_species_links.rds")

all_publications_export <- all_publications %>%
  mutate(across(where(is.list), ~ purrr::map_chr(.x, function(x) {
    if (is.null(x) || length(x) == 0) {
      return(NA_character_)
    }
    paste(as.character(unlist(x)), collapse = "; ")
  })))

all_species_links_export <- all_species_links %>%
  mutate(across(where(is.list), ~ purrr::map_chr(.x, function(x) {
    if (is.null(x) || length(x) == 0) {
      return(NA_character_)
    }
    paste(as.character(unlist(x)), collapse = "; ")
  })))

write.csv(all_publications_export, "data_processed/all_science_publications.csv", row.names = FALSE)
write.csv(all_species_links_export, "data_processed/all_species_links.csv", row.names = FALSE)

