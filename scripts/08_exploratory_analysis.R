source("scripts/00_setup.R")

species_threats <- read.csv("data_processed/species_threats.csv")
species_threats_summary <- read.csv("data_processed/species_threats_summary.csv")
species_document_summary <- read.csv("data_processed/species_document_summary.csv")
species_documents <- read.csv("data_processed/species_documents.csv")

species_threats %>%
  distinct(id, sara_status, threat_category) %>%
  count(sara_status, threat_category, sort = TRUE)

names(species_threats_summary)
species_threats_summary %>%
  arrange(desc(n_threat_categories))

species_threat_summary %>%
  select( record_name, n_threat_categories) %>%
  arrange(desc(n_threat_categories))

species_document_summary %>%
  arrange(desc(n_documents))

species_threats %>%
  distinct(id, threat_category) %>%
  count(threat_category, sort = TRUE)
species_documents %>%
  count(primary_document_type, sort = TRUE)

