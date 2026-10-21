# Project Roadmap

## Project Objective

Develop a structured intelligence platform for freshwater Species at Risk (SAR) records using publicly available Species at Risk Registry information.

------------------------------------------------------------------------

# Phase 1: Data Acquisition

## Species Inventory

Status: Complete

Outputs:

- freshwater_species_master_raw.csv
- freshwater_species_master.csv

Scripts:

- 01_build_species_master.R
- 02_clean_species_master.R

------------------------------------------------------------------------

## Species Profiles

Status: Complete

Outputs:

- species_profiles.csv
- species_full.csv

Script:

- 03_build_species_profiles.R

Information captured:

- Description
- Population
- Habitat
- Biology
- Threats
- Protection information

Future Enhancement: 

- Extract province/jurisdiction information from species profile text. 
- Generate province_presence and province_count fields. 
- Validate against Registry source material before inclusion in master datasets.

------------------------------------------------------------------------

## Threat Classification

Status: Complete

Outputs:

- species_threats.csv
- species_threat_summary.csv

Script:

- 04_build_threat_dictionary.R

Current threat categories include:

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

------------------------------------------------------------------------

## Document Inventory

Status: Complete

Outputs:

- species_documents.csv
- species_document_summary.csv

Script:

- 05_build_document_inventory.R

Information captured:

- Document titles
- Document type
- Publication information
- Attachment information
- Document URLs
- Species-document relationships

------------------------------------------------------------------------

# Phase 2: Quality Assurance

Status: In Progress

Potential future tasks:

- Threat dictionary refinement
- Additional document-type normalization
- Duplicate record assessment
- Validation against Registry species pages

------------------------------------------------------------------------

# Phase 3: Analysis

Status: Planned

Potential products:

- Threat prevalence summaries
- Status-specific threat analyses
- Recovery document coverage analyses
- Species information completeness assessments

------------------------------------------------------------------------

# Phase 4: Database Development

Status: Planned

Potential outputs:

- SQLite database
- DuckDB database
- Parquet-based storage

Proposed tables:

- species
- profiles
- threats
- documents

------------------------------------------------------------------------

# Phase 5: Application Development

Status: Planned

Potential products:

- Shiny dashboard
- Species search interface
- Threat exploration tools
- Document explorer
- Reporting tools

------------------------------------------------------------------------
