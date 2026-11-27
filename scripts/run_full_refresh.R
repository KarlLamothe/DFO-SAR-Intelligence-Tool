################################################################################
# Canadian Species at Risk Explorer
# Full Data Refresh Pipeline
#
# Purpose:
# Executes all data acquisition, processing, publication integration,
# compliance calculation, image download, and application data preparation
# scripts required to update the Canadian Species at Risk Explorer.
#
# Workflow:
#   1. Load package dependencies and project settings.
#   2. Build the species master dataset from Registry sources.
#   3. Clean and standardize species information.
#   4. Build CSAS publication datasets and species linkages.
#   5. Build DFO Science and Canadian Wildlife Service publication datasets
#      and species linkages.
#   6. Build species profile datasets.
#   7. Build the Registry document inventory.
#   8. Download species images for local application use.
#   9. Prepare application-ready RDS files for the Shiny application.
################################################################################

message("Starting full data refresh...")

################################################################################
# PROJECT SETUP
################################################################################
source("scripts/00_setup.R")

################################################################################
# SPECIES DATA
################################################################################
source("scripts/01_build_species_master.R")
source("scripts/02_clean_species_master.R")

################################################################################
# CSAS PUBLICATIONS
################################################################################
source("scripts/03_build_csas_publications.R")

################################################################################
# SCIENTIFIC PUBLICATIONS
################################################################################
source("scripts/04_build_scientific_publications.R")

################################################################################
# SPECIES PROFILES
################################################################################
source("scripts/05_build_species_profiles.R")

################################################################################
# DOCUMENT INVENTORY
################################################################################
source("scripts/06_build_document_inventory.R")

################################################################################
# SPECIES IMAGES
################################################################################
source("scripts/07_download_images.R")

################################################################################
# PREPARE SHINY APPLICATION DATA
################################################################################
source("scripts/08_prepare_shiny_data.R")

message("Data refresh complete.")
