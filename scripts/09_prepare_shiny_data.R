
# revise photo copyrights before saving
species_master <- read.csv("data_processed/all_species_master.csv")

species_master <- species_master %>%
  mutate(photoCopyright = ifelse(
    photoCopyright ==
    "© His Majesty The King in Right of Canada; photo: Fisheries and Oceans Canada",
    "© Fisheries and Oceans Canada", photoCopyright))

species_profiles <- read.csv("data_processed/all_species_profiles.csv")
species_threats <- read.csv("data_processed/all_species_threats.csv")
species_documents <- read.csv("data_processed/all_species_documents.csv")
recovery_compliance <- read.csv("data_processed/all_recovery_compliance.csv")
progress_compliance <- read.csv("data_processed/all_progress_report_compliance.csv")

saveRDS(list(data = species_master, scrape_date = Sys.Date()), "app/data/species_master.rds")
saveRDS(species_profiles, "app/data/species_profiles.rds")
saveRDS(species_threats, "app/data/species_threats.rds")
saveRDS(species_documents, "app/data/species_documents.rds")
saveRDS(recovery_compliance, "app/data/recovery_compliance.rds")
saveRDS(progress_compliance, "app/data/progress_report_compliance.rds")
