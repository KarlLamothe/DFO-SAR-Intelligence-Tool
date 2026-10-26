################################################################################
# RECOVERY COMPLIANCE
# Build datasets that evaluate compliance with key SARA recovery planning
# requirements:
# 
# 1. Recovery Strategy Compliance
#    - Measures time between SARA listing and publication of the first
#      Recovery Strategy.
#    - Legislative timelines:
#        Endangered = 1 year from listing
#        Threatened = 2 years from listing
#        Extirpated = 2 years from listing
#
# 2. Progress Report Compliance
#    - Measures whether a progress report has been published within
#      five years of the most recent Recovery Strategy.
#
################################################################################

source("scripts/00_setup.R")

species_documents <- read.csv("data_processed/species_documents.csv")
species_master <- read.csv("data_processed/freshwater_species_master.csv")

################################################################################
# RECOVERY STRATEGY COMPLIANCE
#
# Determine the first Recovery Strategy publication date for each
# Designatable Unit (DU).
#
# Combined "Recovery Strategy & Action Plan" documents are treated as
# Recovery Strategies for compliance purposes.
################################################################################
recovery_dates <- species_documents %>%
  filter(planning_document_type %in% 
           c("Recovery Strategy", "Recovery Strategy & Action Plan")) %>%
  group_by(record_name) %>%
  summarise(first_recovery_strategy_date =  min(as.Date(attachment_date)),.groups = "drop")

################################################################################
# Join recovery strategy dates to species listing information.
#
# legalDateListing represents the legal SARA listing date and is used to
# calculate statutory publication deadlines.
################################################################################
compliance <- species_master %>%
  select(record_name, sara_status, legalDateListing) %>%
  mutate(listing_date =as.Date(legalDateListing,format = "%m/%d/%Y %H:%M:%S"))%>%
  left_join(recovery_dates, by = "record_name")

################################################################################
# Calculate Recovery Strategy due dates.
#
# Legislative deadlines:
#   Endangered = 1 year after listing
#   Threatened = 2 years after listing
#   Extirpated = 2 years after listing
################################################################################
compliance <- compliance %>%
  mutate(required_date = case_when(
    sara_status == "Endangered" ~ listing_date + 365,
    sara_status %in% c("Threatened", "Extirpated") ~listing_date + 730,
    TRUE ~ as.Date(NA)))

################################################################################
# Calculate delay and compliance status.
#
# days_late:
#   Negative = published before deadline
#   Positive = published after deadline
#
# compliance_status:
#   On Time
#   Late
#   No Recovery Strategy
################################################################################
compliance <- compliance %>%
  mutate(days_late = as.numeric(first_recovery_strategy_date - required_date),
         compliance_status = case_when(is.na(first_recovery_strategy_date) ~
                                         "No Recovery Strategy",
                                       days_late <= 0 ~"On Time",
                                       days_late > 0 ~"Late"))

# QA CHECKS
compliance %>%
  count(compliance_status)

compliance %>%
  arrange(desc(days_late)) %>%
  select(record_name, sara_status, listing_date, first_recovery_strategy_date, days_late)

# EXPORT
write.csv(compliance,"data_processed/recovery_compliance.csv")

################################################################################
# PROGRESS REPORT COMPLIANCE
#
# Determine whether a Progress Report has been published within five years
# of the most recent Recovery Strategy.
#
# The most recent Recovery Strategy is used because amended or updated
# strategies effectively reset the recovery planning cycle.
################################################################################
# Most recent Recovery Strategy date
recovery_strategy_dates <- species_documents %>%
  filter(planning_document_type %in% 
           c("Recovery Strategy","Recovery Strategy & Action Plan")) %>%
  group_by(record_name) %>%
  summarise(recovery_strategy_date = max(as.Date(attachment_date)), .groups = "drop")

# Most recent Progress Report date
progress_report_dates <- species_documents %>%
  filter(planning_document_type == "Progress Report") %>%
  group_by(record_name) %>%
  summarise(latest_progress_report_date = max(as.Date(attachment_date)),.groups = "drop")

################################################################################
# Calculate Progress Report due dates.
#
# Progress Reports are expected within five years of publication of the
# Recovery Strategy.
################################################################################
progress_compliance <- recovery_strategy_dates %>%
  left_join(progress_report_dates, by = "record_name") %>%
  mutate(progress_report_due_date = recovery_strategy_date + round(365.25 * 5))

# Calculate delay and compliance status.
progress_compliance <- progress_compliance %>%
  mutate(days_late = case_when(!is.na(
    latest_progress_report_date) ~as.numeric(latest_progress_report_date -
                                               progress_report_due_date),TRUE ~
      as.numeric(Sys.Date() -progress_report_due_date)),

    next_progress_report_due_date = case_when(
      !is.na(latest_progress_report_date) ~ latest_progress_report_date + 
        round(365.25 * 5), TRUE ~ progress_report_due_date),
    
    compliance_status = case_when(
      
      # No report yet
      is.na(latest_progress_report_date) &
        progress_report_due_date > Sys.Date() ~
        "Not Yet Due",
      
      is.na(latest_progress_report_date) &
        progress_report_due_date <= Sys.Date() ~
        "Overdue",
      
      # Report exists and next report not yet due
      !is.na(latest_progress_report_date) &
        next_progress_report_due_date > Sys.Date() ~
        "Up to Date",
      
      # Report exists but next one is overdue
      !is.na(latest_progress_report_date) &
        next_progress_report_due_date <= Sys.Date() ~
        "Overdue"
    ),
    
    last_report_performance = case_when(
      is.na(latest_progress_report_date) ~ NA_character_,
      days_late <= 0 ~ "On Time",
      days_late > 0 ~ "Submitted Late"
    ))

progress_compliance <- progress_compliance %>%
  mutate(years_late = round(days_late / 365.25, 1))

# QA CHECKS
progress_compliance %>%
  count(compliance_status)

progress_compliance %>%
  arrange(desc(days_late))

# EXPORT
write.csv(progress_compliance, "data_processed/progress_report_compliance.csv",)
