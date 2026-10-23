# Freshwater SAR Species Explorer

An interactive Shiny application for exploring Canadian freshwater fish species listed under the Species at Risk Act (SARA).

The application was developed to make information available through the Species at Risk Public Registry easier to access, explore, and summarize. The application brings together species profiles, threat information, species documents, recovery planning information, and species imagery in a single interface for Schedule 1 freshwater fishes listed under SARA.

## Features

### Dashboard

- Application overview
- Data source information
- Coverage statistics
  - Species records
  - Species documents
  - Threat groups
- Important notes

### Species Information

- Common and scientific names
- SARA status
- COSEWIC status
- Designatable unit information
- Species photographs
- Description
- Population information
- Habitat information
- Biology information
- Species-specific threat summaries
- Threat categories

### Species Documents

- Recovery Strategies
- Action Plans
- Management Plans
- COSEWIC Assessments and Status Reports
- Additional SARA Registry documents
- Direct links to source documents

### Recovery Planning

- Recovery planning coverage by DU
- Recovery Strategy and Action Plan tracking
- Most recent recovery planning document date
- Years since publication of the most recent recovery planning document
- Visual highlighting of older recovery planning documents

### Threat Summary

- DUs with the greatest number of threat categories
- Frequency of threat categories across species
- Summary views of threat prevalence

### Explore Threats

Filter species records by:

- Threat Group
- SARA Status

Interactive results table:

- Click a species record to automatically load it into the Species Information tab

## Data Sources

Data are derived from:

- Species at Risk Public Registry
- Species profile pages
- Recovery and planning documents
- Species images provided through Registry metadata

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

# Included Taxa

The current application focuses on freshwater fishes listed under Schedule 1 of SARA with statuses of:

- Endangered
- Threatened
- Extirpated

## Requirements

```r
library(shiny)
library(dplyr)
library(DT)
library(shinydashboard)
library(bslib)
```

## Running the App

Go to the run_app.R script and run:

```r
library(shiny)
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
└── run_app.R

```

## Potential Future Enhancements

- Supporting additional aquatic taxa beyond freshwater fishes
- Adding export functionality for selected species information
- Enhancing search and filtering capabilities

## Acknowledgements

This application was developed using information obtained from the Species at Risk Public Registry and associated recovery planning resources.

## License

This project is licensed under the MIT License. See ./LICENSE for the full license terms.

Copyright (c) 2026 His Majesty the King in Right of Canada, as represented by the Minister of Fisheries and Oceans.

## Contact

For questions, suggestions, or data updates, please contact:

Karl Lamothe; karl.lamothe@dfo-mpo.gc.ca
Fisheries and Oceans Canada