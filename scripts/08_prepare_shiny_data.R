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
      common_name == "Speckled Dace" ~ "© USFWS",
      common_name == "Spotted Gar" ~ "© Brian Gratwicke",
      common_name == "Vancouver Lamprey" ~ "© Les Harris",
      common_name == "Carmine Shiner" ~ "© Mathew Zappa",
      common_name == "Banded Killifish" ~ "© Smithsonian Environmental Research Center",
      common_name == "Columbia Sculpin" ~ "© Vince Golder",
      common_name == "Cutlip Minnow" ~ "© Smithsonian Environmental Research Center",
      common_name == "Dolly Varden" ~ "© Fisheries and Oceans Canada",
      common_name == "Lake Sturgeon" ~ "© Ellen Edmonson and Hugh Chrisp",
      common_name == "Northern Sunfish" ~ "© qfc79",
      common_name == "Shortnose Sturgeon" ~ "© Joel Sartore",
      common_name == "Silver Lamprey" ~ "© Ellen Edmonson and Hugh Chrisp",
      common_name == "Upper Great Lakes Kiyi" ~ "© Joseph R. Tomelleri",
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
progress_compliance <- read.csv("data_processed/progress_report_compliance.csv")

saveRDS(species_master, "app/data/species_master.rds")
saveRDS(species_profiles, "app/data/species_profiles.rds")
saveRDS(species_threats, "app/data/species_threats.rds")
saveRDS(species_documents, "app/data/species_documents.rds")
saveRDS(recovery_compliance, "app/data/recovery_compliance.rds")
saveRDS(progress_compliance, "app/data/progress_report_compliance.rds")

