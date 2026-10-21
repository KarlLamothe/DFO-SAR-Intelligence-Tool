source("scripts/00_setup.R")

# read in raw data
raw_species <- read.csv("data_processed/freshwater_species_master_raw.csv")
names(raw_species)
glimpse(raw_species)

# reduce
species_master <- raw_species %>%
  mutate(legalScientificName = legalScientificName,
         cosewicScientificName = cosewicScientificName,
         common_name = coalesce(legalCommonName, cosewicCommonName),
         scientific_name = coalesce(legalScientificName, cosewicScientificName),
         record_name = case_when(
           !is.na(cosewicPopulation) &
             cosewicPopulation != "" ~
             paste(common_name, "-", cosewicPopulation),
           TRUE ~ common_name),
         sara_status = legalStatusId,
         cosewic_status = cosewicStatusId,
         status = coalesce(legalStatusId, cosewicStatusId),
         schedule = legalScheduleId)

# reorder columns
species_master <- species_master %>%
  relocate(id, common_name, scientific_name, sara_status, cosewic_status, record_name,
           schedule, taxonomyId)
unique(species_master$sara_status)

# change to sentence case rather than all caps
species_master <- species_master %>%
  mutate(across(c(
    sara_status, cosewic_status, schedule, cosewicStatusId, 
    legalStatusId, legalScheduleId, statusChange, status),
      ~ .x %>%
        str_replace_all("_", " ") %>%
        str_to_title()))
head(species_master)

# look at status of species
species_master %>%
  count(status, sort = TRUE)

species_master %>%
  count(sara_status, sort = TRUE)

# save
#write_csv(species_master, "data_processed/freshwater_species_master.csv")
