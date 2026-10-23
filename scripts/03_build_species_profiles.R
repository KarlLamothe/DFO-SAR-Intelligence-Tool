source("scripts/00_setup.R")

species_master <- read.csv("data_processed/freshwater_species_master.csv")
colnames(species_master)

# moving to all species
species_profiles <- purrr::map2_dfr(
  freshwater_species$id,
  freshwater_species$profiles,
  extract_profile)

# clean this up
str(species_profiles)
colnames(species_profiles)
species_profiles_clean <- species_profiles %>%
  mutate(across(c(description, population, habitat, biology, threats,
                  other_protection, critical_habitat, federal_protection),
                clean_html))
str(species_profiles_clean)

# add species names
species_profiles_clean <- species_profiles_clean %>%
  left_join(species_master %>%
              select(id, record_name, common_name, scientific_name, 
                     sara_status, cosewic_status), by = "id")
colnames(species_profiles_clean)
head(species_profiles_clean)

# remove fields without entries across all species
species_profiles_clean <- species_profiles_clean %>%
  select(-critical_habitat, -federal_protection)

species_profiles_clean <- species_profiles_clean %>%
  relocate(id, record_name, common_name, scientific_name, sara_status, cosewic_status)

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
  select(-profiles)

names(species_full_export)[sapply(species_full_export, is.list)]
head(species_full_export)

species_full_export <- species_full_export %>%
  mutate(
    range_text = sapply(
      ranges,
      function(x) paste(x, collapse = "; ")
    )
  ) %>%
  select(-ranges)

# change to sentence case rather than all caps
species_full_export <- species_full_export %>%
  mutate(across(c(
    cosewicStatusId, legalStatusId, legalScheduleId, statusChange),
    ~ .x %>%
      str_replace_all("_", " ") %>%
      str_to_title()))
head(species_full_export)
head(species_profiles_clean)

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
  select(common_name, threats) %>%
  slice(2)
colnames(species_full_export)

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
