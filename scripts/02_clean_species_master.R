source("scripts/00_setup.R")

# read in raw data
raw_species <- read.csv("data_processed/all_species_raw.csv")
names(raw_species)
glimpse(raw_species)

# reduce
species_master <- raw_species %>%
  mutate(
    legalScientificName = legalScientificName,
    cosewicScientificName = cosewicScientificName,
    
    common_name = coalesce(legalCommonName, cosewicCommonName),
    scientific_name = coalesce(legalScientificName, cosewicScientificName)
  ) %>%
  mutate(
    common_name = gsub("\u2019", "'", common_name),
    scientific_name = gsub("\u2019", "'", scientific_name)
  ) %>%
  mutate(
    record_name = case_when(
      !is.na(cosewicPopulation) &
        cosewicPopulation != "" ~
        paste(common_name, "-", cosewicPopulation),
      TRUE ~ common_name
    ),
    sara_status = legalStatusId,
    cosewic_status = cosewicStatusId,
    status = coalesce(legalStatusId, cosewicStatusId),
    schedule = legalScheduleId,
    range_text = range_text
  )

# reorder columns
species_master <- species_master %>%
  relocate(id, common_name, scientific_name, sara_status, cosewic_status, record_name,
           schedule, taxonomyId, range_text)
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

species_master <- species_master %>%
  mutate(
    local_image_file = paste0(
      gsub(" ", "_", tolower(common_name)),
      ".jpg"
    ),
    image_url = paste0(
      "https://ecprccsarstacct.z9.web.core.windows.net/files/photo/",
      imageFileName
    )
  )

# save
write_csv(species_master, "data_processed/all_species_master.csv")
