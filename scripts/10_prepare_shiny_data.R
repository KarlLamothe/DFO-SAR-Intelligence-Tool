
# revise photo copyrights before saving
species_master <- read.csv("data_processed/all_species_master.csv")

species_master <- species_master %>%
  mutate(photoCopyright = ifelse(
    photoCopyright ==
    "© His Majesty The King in Right of Canada; photo: Fisheries and Oceans Canada",
    "© Fisheries and Oceans Canada", photoCopyright))

aquatic_molluscs <- c(
  "Kidneyshell",
  "Mapleleaf",
  "Threehorn Wartyback",
  "Round Hickorynut",
  "Eastern Pondmussel",
  "Brook Floater",
  "Northern Abalone",
  "Lilliput",
  "Olympia Oyster",
  "Purple Wartyback",
  "Atlantic Mud-piddock",
  "Yellow Lampmussel",
  "Mapleleaf",
  "Hickorynut",
  "Snuffbox",
  "Salamander Mussel",
  "Rocky Mountain Ridged Mussel",
  "Rainbow",
  "Rayed Bean",
  "Northern Riffleshell",
  "Round Pigtoe",
  "Wavy-rayed Lampmussel",
  "Dwarf Wedge Mussel",
  "Eastern Pondmussel",
  "Fawnsfoot"
)

species_master <- species_master %>%
  mutate(taxonomy_display = case_when(
    taxonomyId == "Molluscs" & common_name %in% aquatic_molluscs ~ "Molluscs (Bivalves)",
      taxonomyId == "Molluscs" ~"Molluscs (Non-Bivalves)",
      TRUE ~ taxonomyId))

species_master %>%
  filter(taxonomyId == "Molluscs") %>%
  count(taxonomy_display)

species_profiles <- read.csv("data_processed/all_species_profiles.csv")
species_threats <- read.csv("data_processed/all_species_threats.csv")
species_documents <- read.csv("data_processed/all_species_documents.csv")
recovery_compliance <- read.csv("data_processed/all_recovery_compliance.csv")
progress_compliance <- read.csv("data_processed/all_progress_report_compliance.csv")
csas_publications <- read.csv("data_processed/csas_publications.csv",stringsAsFactors = FALSE)
csas_species_links <- read.csv("data_processed/csas_species_links.csv", stringsAsFactors = FALSE)

csas_publications <- csas_publications %>%
  distinct(publication_id, .keep_all = TRUE)

saveRDS(list(data = species_master, scrape_date = Sys.Date()), "app/data/species_master.rds")
saveRDS(species_profiles, "app/data/species_profiles.rds")
saveRDS(species_threats, "app/data/species_threats.rds")
saveRDS(species_documents, "app/data/species_documents.rds")
saveRDS(recovery_compliance, "app/data/recovery_compliance.rds")
saveRDS(progress_compliance, "app/data/progress_report_compliance.rds")
saveRDS(csas_publications, "app/data/csas_publications.rds")
saveRDS(csas_species_links, "app/data/csas_species_links.rds")

