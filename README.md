# Freshwater SAR Intelligence Tool

## Overview

The Freshwater Species at Risk (SAR) Intelligence Tool is an R-based workflow that compiles, organizes, and standardizes information from the Canadian Species at Risk Public Registry for freshwater fish species.

The project currently integrates:

- Species inventory information
- Species profile information
- Threat classifications
- Registry document inventories

The resulting datasets support species-at-risk science, recovery planning, threat assessment, habitat offsetting analyses, and information synthesis.

---

## Project Structure

```
.
├── data_raw/
├── data_processed/
├── docs/
├── scripts/
├── .Renviron
├── .gitignore
└── README.md
```

---

## Workflow

```
01_build_species_master.R
        ↓
02_clean_species_master.R
        ↓
03_build_species_profiles.R
        ↓
04_build_threat_dictionary.R
        ↓
05_build_document_inventory.R
```

---

## Processed Data Outputs

### freshwater_species_master_raw.csv

Raw freshwater fish records retrieved from the Registry API.

### freshwater_species_master.csv

Cleaned freshwater species inventory.

### species_profiles.csv

Species profile information including:

- Description
- Population
- Habitat
- Biology
- Threats
- Other protection information

### species_full.csv

Combined species inventory and profile dataset.

### species_threats.csv

Keyword-based threat classifications.

### species_threat_summary.csv

Summary of threat categories by species record.

### species_documents.csv

Inventory of Registry documents associated with freshwater fish records.

### species_document_summary.csv

Summary of document coverage by species record.

---

## Current Status

### Completed

- Species inventory extraction
- Species profile extraction
- Threat classification framework
- Document inventory framework
- Data dictionary development

### Planned

- Threat classification refinement
- Analytical summaries
- SQLite database implementation
- Shiny dashboard development
- Additional document extraction and synthesis

---

## Data Sources

- Species at Risk Public Registry
- Registry Azure Search services
- Registry document repositories

---

## Author

Karl Lamothe; karl.lamothe@dfo-mpo.gc.ca
Fisheries and Oceans Canada