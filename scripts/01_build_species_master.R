source('scripts/00_setup.R')

# Retrieve species records
get_species_page <- function(skip = 0, top = 100){
  payload <- list(count = TRUE, search = "*", searchMode = "all",
                  queryType = "full", skip = skip, top = top)
  response <- POST(url = endpoint, add_headers(
    `api-key` = api_key, `Content-Type` = "application/json"),
    body = payload, encode = "json")
  content_text <- content(response, "text", encoding = "UTF-8" )
  jsonlite::fromJSON(content_text)$value
}

# all species
all_species <- purrr::map_dfr(seq(0, 1500, by = 100), get_species_page)
#nrow(all_species)
#colnames(all_species)
#head(all_species)

# Build record names
all_species <- all_species %>%
  mutate(recordName = case_when(
    !is.na(cosewicPopulation) & cosewicPopulation != "" ~
        paste(commonName, "-", cosewicPopulation), TRUE ~ commonName))
colnames(all_species)

# Keep required fields
all_species <- all_species %>%
  select(id, commonName, taxonomyId, cosewicCommonName, cosewicScientificName,
         cosewicStatusCriteria, cosewicStatusHis, cosewicReasonDes,
         cosewicPopulation, cosewicLastAssessmentDate, emergencyAssessment,
         cosewicStatusId, legalCommonName, legalScientificName,
         legalStatusHistory, legalPopulation, legalDateListing,
         underConsiderationId, legalStatusId, legalScheduleId,azSpecies,
         profiles, ministerReceiptDate, statusChange, ranges, 
         imageFileName, lastGicDecisionDate, currentGicDecisionId, 
         hasRelatedSpeciesOnScheduleOne, photoCopyright, Id, recordName)

# Derived fields
all_species <- all_species %>%
  mutate(range_text = purrr::map_chr(ranges, ~ paste(.x, collapse = "; ")))

# Clean html text
all_species <- all_species %>%
  mutate(across(where(is.character), clean_html))

# Export
write_csv(all_species, "data_processed/all_species_raw.csv")

# completion message for when running full data refresh
message(sprintf("Species extraction complete: %s records saved.",
                nrow(all_species)))
