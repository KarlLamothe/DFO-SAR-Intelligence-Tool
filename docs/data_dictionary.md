# Data Dictionary

This document describes the datasets generated through the Freshwater Species at Risk (SAR) Intelligence Tool workflow. All datasets are derived from the Species at Risk Public Registry and associated Registry APIs.

---

# Primary Keys

| Dataset | Primary Key |
|----------|-------------|
| freshwater_species_master_raw | id |
| freshwater_species_master | id |
| species_profiles | id |
| species_full | id |
| species_threats | id + threat_category + matched_term |
| species_threat_summary | id |
| species_documents | species_id + document_id |
| species_document_summary | species_id |

---

# Join Fields

| Field | Description |
|---------|------------|
| id | Species registry identifier |
| species_id | Foreign key linking documents to species records |
| document_id | Registry document identifier |

---

# freshwater_species_master_raw.csv

Raw freshwater fish dataset downloaded directly from the Registry API.

## Description

Contains all fields returned by the Species at Risk Registry species search endpoint for freshwater fish records.

# freshwater_species_master.csv

Cleaned and standardized species dataset.

## Description

Primary species table used throughout the project. One row represents one Registry species record, population, or designatable unit.

# species_profiles.csv

Species profile information extracted from profile records.

## Description

Contains biological and ecological information extracted from species profiles available through the Registry API.

---

# species_full.csv

Combined species and profile dataset.

## Description

Joins the cleaned species master table with profile information.

---

# species_threats.csv

Threat classification dataset.

## Example Threat Categories

- Agriculture
- Aquaculture
- Barriers
- Channel Modification
- Climate Change
- Competition
- Contaminants
- Forestry
- Habitat Fragmentation
- Habitat Loss
- Hybridization
- Invasive Species
- Mining
- Predation
- Recreation
- Residential and Commercial Development
- Riparian Degradation
- Sedimentation
- Shoreline Development
- Small Population Size
- Transportation
- Urbanization
- Water Quality
- Water Withdrawal
- Wetland Loss

---

# species_threat_summary.csv

Threat summary dataset.

## Description

Summarizes standardized threat categories for each species record.

# species_documents.csv

Document inventory dataset.

## Description

Inventory of Registry documents associated with freshwater fish records.

Where multiple document attachments exist (e.g., proposed, final, or amended versions of a document), the most recent attachment is selected and used to generate the document URL.

## Example Document Types

- Action Plan
- Consultation Document
- COSEWIC Assessment
- COSEWIC Annual Report
- Critical Habitat Order
- Management Plan
- Recovery Strategy
- Residence Description
- Response Statement

---

# species_document_summary.csv

Document summary dataset.

## Description

Summarizes Registry document coverage for each species record.

# Application Assets

## Species Images

Species photographs are obtained from Registry image metadata and downloaded for local use within the Shiny application.

# Notes

- Species records may represent full species, populations, or COSEWIC designatable units.
- Not all freshwater fish records possess profiles or Registry documents.
- Threat classifications are generated using a rule-based keyword dictionary and should be considered screening-level classifications.
- Document inventories are restricted to species records identified within the freshwater fish dataset.
- Document URLs are derived from attachment metadata and Registry file paths.
- Species images are obtained from Registry image metadata and downloaded locally for use within the Shiny application.
- Where multiple document attachments exist for a Registry document, the most recent attachment is used to generate document URLs.
