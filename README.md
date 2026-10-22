# Freshwater SAR Species Explorer

An interactive Shiny application for exploring Canadian freshwater fish species listed under the Species at Risk Act (SARA).

The application integrates species profile information, threat data, recovery planning documents, and species imagery from the Species at Risk Public Registry.

## Features

### Dashboard

- Summary statistics
- Species records
- Recovery and planning documents
- Threat groups
- Document types
- Visual summaries of SARA status and threat prevalence

### Species Overview

- Common and scientific names
- SARA status
- COSEWIC status
- Designatable unit information
- Species photographs
- Description
- Population information
- Habitat information
- Biology information

### Threats

- Consolidated threat groups
- Species-specific threat summaries
- Threat information extracted from profile pages

### Documents

- Recovery Strategies
- Action Plans
- Management Plans
- COSEWIC Assessments and Status Reports
- Additional SARA Registry documents
- Direct links to source documents

### Explore

Filter species by:

- Threat Group
- SARA Status

Interactive results table:

- Click a species to automatically load it in the Species Overview

## Data Sources

Data are derived from:

- Species at Risk Public Registry
- Species profile pages
- Recovery and planning documents
- Species images provided through Registry metadata

## Threat Group Consolidation

Several Registry threat categories are consolidated into broader groups:

| Registry Categories | Consolidated Group |
|---------------------|-------------------|
| Urbanization, Residential and Commercial Development | Urban Development |
| Agriculture, Agricultural and Forestry Effluents | Agriculture |
| Barriers, Transportation and Service Corridors | Connectivity |
| Contaminants, Sedimentation | Water Quality |

## Included Taxa

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
library(ggplot2)
```

## Running the App

Go to the app folder and set your working directory to that source file location. Then, in your console, run:

```r
shiny::runApp()
```

## Project Structure

```text
project/
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
|           ├── atlantic_whitefish.jpg
|           ├── american_eel.jpg
|           ├── redside_dace.jpg
|           ├── ...
|           └── no_image_available.jpg
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
│
├── docs/
│   ├── data_dictionary.md
│   └── project_roadmap.md
|
└── scripts/
    ├── 00_setup.R
    ├── 01_build_species_master.R
    ├── 02_clean_species_master.R
    ├── 03_build_species_profiles.R
    ├── 04_build_threat_dictionary.R
    ├── 05_build_document_inventory.R
    ├── 06_exploratory_analysis.R
    ├── 07_prepare_shiny_data.R
    └── 08_download_images.R

```

## Future Enhancements

Potential future enhancements include:

- Expanding the species image library to provide photographs for all listed taxa
- Supporting additional aquatic taxa beyond freshwater fishes
- Adding export functionality for filtered Explore results
- Developing additional dashboard visualizations and summary metrics
- Enhancing search and filtering capabilities
- Providing direct links to Species at Risk Public Registry species profile pages
- Incorporating species distribution or range maps where available
- Adding species comparison tools based on status, threats, and recovery planning information

## Acknowledgements

This application was developed using information obtained from the Species at Risk Public Registry and associated recovery planning resources. It is intended to improve access to species profile, threat, and recovery planning information for freshwater fish species listed under the Species at Risk Act (SARA).

## License

This project is provided for research, analysis, and information-sharing purposes. Users should consult the Species at Risk Public Registry for official species status information and recovery planning documents.

## Contact

For questions, suggestions, or data updates, please contact:

Karl Lamothe; karl.lamothe@dfo-mpo.gc.ca
Fisheries and Oceans Canada