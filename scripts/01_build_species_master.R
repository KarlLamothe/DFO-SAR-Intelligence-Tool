source('scripts/00_setup.R')

get_species_page <- function(skip = 0, top = 100){
  payload <- list(count = TRUE, search = "*", searchMode = "all",
                  queryType = "full", skip = skip, top = top)
  response <- POST(url = endpoint, add_headers(
    `api-key` = api_key, `Content-Type` = "application/json"),
    body = payload, encode = "json")
  content_text <- content(response, "text", encoding = "UTF-8" )
  jsonlite::fromJSON(content_text)$value
}

# scape all species
all_species <- purrr::map_dfr(
  seq(0, 1200, by = 100),
  get_species_page
)
nrow(all_species)

# subset freshwater species
freshwater_species <- all_species %>%
  filter(taxonomyId == "Fishes (freshwater)")
nrow(freshwater_species)

# include RecordName
freshwater_species <- freshwater_species %>%
  mutate(
    recordName = case_when(
      !is.na(cosewicPopulation) &
        cosewicPopulation != "" ~
        paste(commonName, "-", cosewicPopulation),
      TRUE ~ commonName
    )
  )
colnames(freshwater_species)

# save raw master
write_csv(freshwater_species, "data_processed/freshwater_species_master_raw.csv")
