source("scripts/00_setup.R")

# revise photo copyrights before saving
species_master <- read.csv("data_processed/freshwater_species_master.csv")

species_master <- species_master %>%
  mutate(
    photoCopyright = case_when(
      common_name == "Paddlefish" ~ "© Wikimedia Commons",
      common_name == "Channel Darter" ~ "© George Coker",
      common_name == "Copper Redhorse" ~ "© Fisheries and Oceans Canada",
      common_name == "Salish Sucker" ~ "© Mike Pearson",
      common_name == "Lake Chubsucker" ~ "© Howard Jelks",
      common_name == "Plains Minnow" ~ "© Niall Clancy",
      common_name == "Western Silvery Minnow" ~ "© Fisheries and Oceans Canada",
      common_name == "Striped Bass" ~ "© D. Ross Robertson",
      common_name == "Westslope Cutthroat Trout" ~ "© USGS",
      common_name == "Striped Bass" ~ "© D. Ross Robertson",
      common_name == "Rainbow Trout" ~ "© Eric Engbretson",
      common_name == "Misty Lake Lentic Stickleback" ~ "© A. Hendry",
      common_name == "Misty Lake Lotic Stickleback" ~ "© A. Hendry",
      common_name == "Eastern Sand Darter" ~ "© Alan Dextrase",
      common_name == "Silver Shiner" ~ "© Jacob Burbank",
      TRUE ~ photoCopyright
    )
  )

species_master <- species_master %>%
  mutate(photoCopyright = ifelse(
    photoCopyright ==
    "© His Majesty The King in Right of Canada; photo: Fisheries and Oceans Canada",
    "© Fisheries and Oceans Canada", photoCopyright))

species_profiles <- read.csv("data_processed/species_profiles.csv")
species_threats <- read.csv("data_processed/species_threats.csv")
species_documents <- read.csv("data_processed/species_documents.csv")
recovery_compliance <- read.csv("data_processed/recovery_compliance.csv")
saveRDS(species_master, "app/data/species_master.rds")
saveRDS(species_profiles, "app/data/species_profiles.rds")
saveRDS(species_threats, "app/data/species_threats.rds")
saveRDS(species_documents, "app/data/species_documents.rds")
saveRDS(recovery_compliance, "app/data/recovery_compliance.rds")

