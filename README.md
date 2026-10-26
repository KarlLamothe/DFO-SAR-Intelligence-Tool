# Freshwater SAR Species Explorer

An interactive Shiny application for exploring Canadian freshwater fish species listed under the *Species at Risk Act* (SARA).

The application was developed to make information available through the Species at Risk Public Registry easier to access, explore, and summarize. The application brings together species profiles, threat information, species documents, recovery planning information, and species imagery in a single interface for Schedule 1 freshwater fishes listed under SARA.

## Features

- Browse Schedule 1 freshwater fishes listed under SARA.
- Explore species profile information and identified threats.
- Access Recovery Strategies, Action Plans, Management Plans, Progress Reports, and COSEWIC Assessments.
- Review recovery planning compliance metrics, including:
  - Recovery Strategy publication timelines.
  - Progress Report publication timelines.
  - Legislative due dates and compliance status.
- Summarize recovery planning coverage and threat occurrence across species and jurisdictions.

### Dashboard

Provides a high-level overview of the application and its underlying data sources.

- Application overview
- Data source information
- Coverage statistics
  - Species records
  - Species documents
  - Threat groups
- Important notes

### Species Information

Explore detailed species profile information by species, designatable unit (DU), SARA status, and geographic range.

**Includes:**

- Common and scientific names
- SARA status
- COSEWIC status
- DU information
- Species photographs
- Geographic range information
- Species description
- Population information
- Habitat information
- Biology information
- Species-specific threat summaries
- Threat category listings

**Filters:**

- SARA Status
- Province/Territory
- Species
- DU

### Species Documents

Access recovery planning and assessment documents associated with each DU.

**Includes:**

- Recovery Strategies
- Action Plans
- Management Plans
- COSEWIC Assessments and Status Reports
- Additional Species at Risk Public Registry documents
- Direct links to source documents

**Filters:**

- SARA Status
- Province/Territory
- Species
- DU

### Recovery Planning Compliance

The application calculates legislative recovery-planning metrics using document publication dates from the Species at Risk Public Registry.

Metrics currently include:

- Recovery Strategy compliance (time between SARA listing and Recovery Strategy publication).
- Progress Report compliance (time between Recovery Strategy publication and required progress reporting milestones).

These metrics are intended as screening-level indicators and should not be interpreted as formal assessments of regulatory compliance.

### Recovery Planning

Summarizes recovery planning progress across species and DUs.

**Includes:**

- Recovery planning coverage by DU
- Recovery Strategy tracking
- Action Plan tracking
- Most recent recovery planning document date
- Years since publication of the most recent recovery planning document
- Visual highlighting of older planning documents

**Filters:**

- SARA Status
- Province/Territory

### Threat Summary

- DUs with the greatest number of threat categories
- Frequency of threat categories across species
- Summary views of threat prevalence

**Filters:**

- Province/Territory

### Explore Threats

Allows users to identify species affected by specific threat categories.

**Filter by:**

- Threat Group
- SARA Status
- Province/Territory

**Includes:**

- Interactive species results table
- Direct navigation to Species Information records

**Interactive Feature:**

- Selecting a species record automatically loads the corresponding species profile in the **Species Information** tab.

### Province Summary

Provides an overview of species-at-risk information within a selected province or territory.

**Includes:**

- Species record counts
- SARA status summaries
- Most common threat categories
- Provincial species overview metrics

**Filters:**

- Province/Territory

### Data Sources

Data are derived from:

- Species at Risk Public Registry
- Species profile pages
- Recovery Strategies
- Action Plans
- Management Plans
- Progress Reports
- COSEWIC Assessment and Status Reports

# Threat Group Consolidation

Several Registry threat categories are consolidated into broader groups loosely following the IUCN Threat Classification Scheme, including:

- Residential and Commercial Development
- Agriculture and Aquaculture
- Energy Production and Gas
- Transportation and Service Corridors
- Biological Resource Use
- Human Intrusions and Disturbance
- Natural Systems Modification
- Invasive Species and other Problematic Species, Genes, and Diseases
- Pollution
- Geological Events
- Climate Change
- Other Limiting Factors

Threat categories are intended as **screening-level classifications** and should not be interpreted as formal threat assessments.

# Included Taxa

The current application focuses on freshwater fishes listed under Schedule 1 of SARA with statuses of:

- Endangered
- Threatened
- Extirpated

## Running the App

Go to the run_app.R script, install the required packages if needed, and run:

```r
library(shiny)
library(dplyr)
library(DT)
library(bslib)

runApp("app/app.R")
```

## Project Structure

```text
DFO-SAR-Intelligence-Tool/
│
├── app/
│   ├── app.R
|   |
|   ├── data/
│   |   ├── progress_report_compliance.rds
│   |   ├── recovery_compliance.rds
│   |   ├── species_master.rds
│   |   ├── species_profiles.rds
│   |   ├── species_threats.rds
│   |   └── species_documents.rds
|   |
│   └── www/
|       └── images/
|           ├── alaskan_brook_lamprey.jpg
|           ├── atlantic_whitefish.jpg
|           ├── ...
|           └── white_sturgeon.jpg
│
├── data_processed/
│   ├── freshwater_species_master.csv
│   ├── freshwater_species_master_raw.csv
│   ├── progress_report_compliance.csv
│   ├── recovery_compliance.csv
│   ├── species_document_summary.csv
│   ├── species_documents.csv
│   ├── species_full.csv
│   ├── species_profiles.csv
│   ├── species_threats.csv
│   ├── species_threats_summary.csv
│   └── threat_category_frequency.csv
|
├── scripts/
|   ├── 00_setup.R
|   ├── 01_build_species_master.R
|   ├── 02_clean_species_master.R
|   ├── 03_build_species_profiles.R
|   ├── 04_build_threat_dictionary.R
|   ├── 05_build_document_inventory.R
|   ├── 06_exploratory_analysis.R
|   ├── 07_prepare_shiny_data.R
|   └── 08_download_images.R
|
├── .gitignore
├── DFO-SAR-Intelligence-Tool.Rproj
├── README.md
├── LICENSE
└── run_app.R

```

## Acknowledgements

This application was developed using information obtained from the Species at Risk Public Registry and associated recovery planning resources.

## License

This project is licensed under the **MIT License**. See `LICENSE` for complete licensing terms.

Copyright (c) 2026 His Majesty the King in Right of Canada, as represented by the Minister of Fisheries and Oceans.

## Contact

For questions, suggestions, or data updates, please contact:

**Karl Lamothe**
Aquatic Science Biologist
Fisheries and Oceans Canada

📧 karl.lamothe@dfo-mpo.gc.ca