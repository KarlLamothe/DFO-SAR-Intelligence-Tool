
source("scripts/00_setup.R")

species_documents <- read.csv("data_processed/species_documents.csv")
species_master <- read.csv("data_processed/freshwater_species_master.csv")


recovery_dates <- species_documents %>%
  filter(planning_document_type %in% 
           c("Recovery Strategy", "Recovery Strategy & Action Plan")) %>%
  group_by(record_name) %>%
  summarise(first_recovery_strategy_date =  min(as.Date(attachment_date)),.groups = "drop")

compliance <- species_master %>%
  select(record_name, sara_status, legalDateListing) %>%
  mutate(listing_date =as.Date(legalDateListing,format = "%m/%d/%Y %H:%M:%S"))%>%
  left_join(recovery_dates, by = "record_name")

compliance <- compliance %>%
  mutate(required_date = case_when(
    sara_status == "Endangered" ~ listing_date + 365,
    sara_status %in% c("Threatened", "Extirpated") ~listing_date + 730,
    TRUE ~ as.Date(NA)))

compliance <- compliance %>%
  mutate(days_late = as.numeric(first_recovery_strategy_date - required_date),
         compliance_status = case_when(is.na(first_recovery_strategy_date) ~
                                         "No Recovery Strategy",
                                       days_late <= 0 ~"On Time",
                                       days_late > 0 ~"Late"))

compliance %>%
  filter(
    record_name ==
      "Black Redhorse"
  ) %>%
  select(
    listing_date,
    first_recovery_strategy_date
  )


compliance %>%
  count(compliance_status)

compliance %>%
  arrange(desc(days_late)) %>%
  select(record_name, sara_status, listing_date, first_recovery_strategy_date, days_late)

write.csv(compliance,"data_processed/recovery_compliance.csv")
