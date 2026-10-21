source("scripts/00_setup.R")

extract_profile("110-407")

# test for redside dace
redside_profile <- extract_profile("110-407")
colnames(redside_profile)

redside_profile_clean <- redside_profile %>%
  mutate(across(c(description, population, habitat, biology, threats, other_protection),
                clean_html))
redside_profile_clean$threats

# moving to all species
species_profiles <- purrr::map2_dfr(
  freshwater_species$id,
  freshwater_species$profiles,
  extract_profile)

# clean this up
str(species_profiles)
clean_html <- function(x){
  x |>
    stringr::str_remove_all("<.*?>") |>
    stringr::str_replace_all("&nbsp;", " ") |>
    stringr::str_squish()
}

species_profiles_clean <- species_profiles %>%
  mutate(across(c(description, population, habitat, biology, threats,
                  other_protection, critical_habitat, federal_protection),
                clean_html))
str(species_profiles_clean)

# add species names
species_profiles_clean <- species_profiles_clean %>%
  left_join(freshwater_species %>%
              select(id, commonName, legalScientificName, legalStatusId), by = "id")

# remove fields without entries across all species
species_profiles_clean <- species_profiles_clean %>%
  select(
    -critical_habitat,
    -federal_protection
  )

# combine data
species_full <- freshwater_species %>%
  left_join(species_profiles_clean, by = "id")
sapply(species_full, class)

# search for lists
list_cols <- names(species_full)[
  sapply(species_full, is.list)]
list_cols

# remove lists
species_full_export <- species_full %>%
  select(-profiles, -teams, -related) %>%
  mutate(ranges = purrr::map_chr( ranges, ~ paste(.x, collapse = "; ")),
         prevCosewicCommonName = purrr::map_chr(
           prevCosewicCommonName, ~ paste(.x, collapse = "; ")),
         prevCosewicScientificName = purrr::map_chr(
           prevCosewicScientificName, ~ paste(.x, collapse = "; ")))

names(species_full_export)[sapply(species_full_export, is.list)]

# save data
write.csv(species_profiles_clean, "data_processed/species_profiles.csv")
write.csv(species_full_export, "data_processed/species_full.csv")

# quick checks
species_full_export %>%
  count(legalStatusId, sort = TRUE)

species_full_export %>%
  summarise(
    species = n(),
    profiles = sum(!is.na(description)))

species_full_export %>%
  filter(!is.na(threats) ) %>%
  select(commonName, threats) %>%
  slice(2)

profile_presence <- species_profiles %>%
  summarise(
    description = sum(!is.na(description)),
    population = sum(!is.na(population)),
    habitat = sum(!is.na(habitat)),
    biology = sum(!is.na(biology)),
    threats = sum(!is.na(threats)),
    critical_habitat = sum(!is.na(critical_habitat)),
    federal_protection = sum(!is.na(federal_protection)),
    other_protection = sum(!is.na(other_protection))
  )

profile_presence
