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
#   4. Build species profile datasets.
#   5. Classify and consolidate threat information.
#   6. Build the Registry document inventory.
#   7. Build CSAS publication datasets and species linkages.
#   8. Build DFO Science and Canadian Wildlife Service publication datasets
#      and species linkages.
#   9. Calculate recovery planning and progress reporting compliance.
#  10. Download species images for local application use.
#  11. Prepare application-ready RDS files for the Shiny application.
#
# Key Outputs:
#   • Species master, profiles, threats, and documents
#   • Recovery planning compliance metrics
#   • CSAS publication datasets
#   • DFO Science publication datasets
#   • Canadian Wildlife Service publication datasets
#   • Consolidated scientific publication datasets
#   • Shiny application data objects
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
# SPECIES PROFILES AND THREATS
################################################################################
source("scripts/05_build_species_profiles.R")
source("scripts/06_build_threat_dictionary.R")

################################################################################
# DOCUMENT INVENTORY
################################################################################
source("scripts/07_build_document_inventory.R")

################################################################################
# RECOVERY PLANNING COMPLIANCE
################################################################################
source("scripts/09_build_recovery_compliance.R")

################################################################################
# SPECIES IMAGES
################################################################################
source("scripts/10_download_images.R")

################################################################################
# PREPARE SHINY APPLICATION DATA
################################################################################
source("scripts/11_prepare_shiny_data.R")

message("Data refresh complete.")
