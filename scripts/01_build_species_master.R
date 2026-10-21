source('scripts/Packages-Themes.R')

species_master <- tibble(
  common_name = character(),
  scientific_name = character(),
  sara_status = character(),
  cosewic_status = character(),
  species_url = character())

write_csv(species_master, "data_processed/species_master.csv")
