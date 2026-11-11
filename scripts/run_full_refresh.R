################################################################################
# Canadian Species at Risk Explorer
# Full Data Refresh Pipeline
#
# Purpose:
# Executes all data acquisition, processing, compliance calculation,
# image download, CSAS publication integration, and application data
# preparation scripts required to update the Canadian Species at Risk
# Explorer.
#
# Workflow:
#   1. Load package dependencies and project settings.
#   2. Build the species master dataset from Registry sources.
#   3. Clean and standardize species information.
#   4. Build species profile datasets.
#   5. Classify and consolidate threat information.
#   6. Build the Registry document inventory.
#   7. Build and link CSAS publication datasets.
#   8. Calculate recovery planning and progress reporting compliance.
#   9. Download species images for local application use.
#  10. Prepare application-ready RDS files for the Shiny application.
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
# SPECIES PROFILES AND THREATS
################################################################################
source("scripts/04_build_species_profiles.R")
source("scripts/05_build_threat_dictionary.R")

################################################################################
# DOCUMENT INVENTORY
################################################################################
source("scripts/06_build_document_inventory.R")

################################################################################
# RECOVERY PLANNING COMPLIANCE
################################################################################
source("scripts/08_build_recovery_compliance.R")

################################################################################
# SPECIES IMAGES
################################################################################
source("scripts/09_download_images.R")

################################################################################
# PREPARE SHINY APPLICATION DATA
################################################################################
source("scripts/10_prepare_shiny_data.R")

message("Data refresh complete.")
