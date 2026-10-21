# Data Dictionary

This document describes the datasets generated through the Freshwater Species at Risk (SAR) Intelligence Tool workflow. All datasets are derived from the Species at Risk Public Registry and associated Registry APIs.

---

# Dataset Relationships

```text
freshwater_species_master_raw
           ↓
freshwater_species_master
           ↓
species_profiles
           ↓
species_full

species_profiles
           ↓
species_threats
           ↓
species_threat_summary

freshwater_species_master
           ↓
species_documents
           ↓
species_document_summary
```

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

## Key Fields

| Field | Description |
|---------|------------|
| id | Registry species identifier |
| commonName | Registry common name |
| legalScientificName | Registry scientific name |
| legalStatusId | SARA legal status |
| cosewicStatusId | COSEWIC assessment status |
| profiles | Embedded profile JSON |
| ranges | Geographic range information |
| teams | Registry team information |
| related | Related registry records |

---

# freshwater_species_master.csv

Cleaned and standardized species dataset.

## Description

Primary species table used throughout the project. One row represents one Registry species record, population, or designatable unit.

## Fields

| Field | Description |
|---------|------------|
| id | Registry species identifier |
| record_name | Unique species record name (species plus population/designatable unit where applicable) |
| common_name | Common species name |
| scientific_name | Scientific name |
| sara_status | SARA Schedule 1 status |
| cosewic_status | Current COSEWIC status |
| species_url | URL for the Registry species page |

---

# species_profiles.csv

Species profile information extracted from profile records.

## Description

Contains biological and ecological information extracted from species profiles available through the Registry API.

## Fields

| Field | Description |
|---------|------------|
| id | Registry species identifier |
| record_name | Unique species record name |
| common_name | Common species name |
| scientific_name | Scientific name |
| sara_status | SARA Schedule 1 status |
| cosewic_status | Current COSEWIC status |
| description | General species description |
| population | Population information |
| habitat | Habitat description |
| biology | Biological and life-history information |
| threats | Threat description as presented in the Registry |
| other_protection | Other protection information identified in the Registry |

---

# species_full.csv

Combined species and profile dataset.

## Description

Joins the cleaned species master table with profile information.

## Fields

| Field | Description |
|---------|------------|
| id | Registry species identifier |
| record_name | Unique species record name |
| common_name | Common species name |
| scientific_name | Scientific name |
| sara_status | SARA Schedule 1 status |
| cosewic_status | Current COSEWIC status |
| description | Species description |
| population | Population information |
| habitat | Habitat description |
| biology | Biological information |
| threats | Threat information |
| other_protection | Other protection information |

---

# species_threats.csv

Threat classification dataset.

## Description

Threat statements extracted from species profiles and categorized using a rule-based threat dictionary.

## Fields

| Field | Description |
|---------|------------|
| id | Registry species identifier |
| record_name | Unique species record name |
| common_name | Common species name |
| scientific_name | Scientific name |
| sara_status | SARA Schedule 1 status |
| threat_category | Standardized threat category |
| matched_term | Dictionary term that triggered classification |

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

## Fields

| Field | Description |
|---------|------------|
| id | Registry species identifier |
| record_name | Unique species record name |
| common_name | Common species name |
| n_threat_categories | Number of unique threat categories identified |

---

# species_documents.csv

Document inventory dataset.

## Description

Inventory of Registry documents associated with freshwater fish records.

## Fields

| Field | Description |
|---------|------------|
| species_id | Registry species identifier |
| record_name | Unique species record name |
| common_name | Common species name |
| scientific_name | Scientific name |
| sara_status | SARA Schedule 1 status |
| cosewic_status | Current COSEWIC status |
| document_id | Registry document identifier |
| primary_document_type | Simplified document classification |
| documentTitle | Registry document title |
| documentDescription | Registry document description |
| documentCreateDate | Document creation date |
| attachment_id | Registry attachment identifier |
| attachment_title | Attachment title |
| attachment_date | Attachment publication date |
| document_url | Direct URL to associated document |

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

## Fields

| Field | Description |
|---------|------------|
| species_id | Registry species identifier |
| record_name | Unique species record name |
| common_name | Common species name |
| scientific_name | Scientific name |
| sara_status | SARA Schedule 1 status |
| cosewic_status | Current COSEWIC status |
| n_documents | Number of associated Registry documents |

---

# Notes

- Species records may represent full species, populations, or COSEWIC designatable units.
- Not all freshwater fish records possess profiles or Registry documents.
- Threat classifications are generated using a rule-based keyword dictionary and should be considered screening-level classifications.
- Document inventories are restricted to species records identified within the freshwater fish dataset.
- Document URLs are derived from attachment metadata and Registry file paths.