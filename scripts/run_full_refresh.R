################################################################################
# Canadian Species at Risk Explorer
# Full Data Refresh Pipeline
#
# Purpose:
# Executes all data acquisition, processing, compliance calculation,
# image download, and application data preparation scripts required to
# update the Canadian Species at Risk Explorer.
#
# Workflow:
#   1. Load package dependencies and project settings.
#   2. Build the species master dataset from Registry sources.
#   3. Clean and standardize species information.
#   4. Build species profile datasets.
#   5. Classify and consolidate threat information.
#   6. Build the document inventory.
#   7. Calculate recovery planning and progress reporting compliance.
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
# SPECIES PROFILES AND THREATS
################################################################################
source("scripts/03_build_species_profiles.R")
source("scripts/04_build_threat_dictionary.R")

################################################################################
# DOCUMENT INVENTORY
################################################################################
source("scripts/05_build_document_inventory.R")

################################################################################
# RECOVERY PLANNING COMPLIANCE
################################################################################
source("scripts/07_build_recovery_compliance.R")

################################################################################
# SPECIES IMAGES
################################################################################
source("scripts/08_download_images.R")

################################################################################
# PREPARE SHINY APPLICATION DATA
################################################################################
source("scripts/09_prepare_shiny_data.R")

message("Data refresh complete.")
