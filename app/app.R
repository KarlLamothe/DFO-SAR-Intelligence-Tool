################################################################################
# Canadian Species at Risk Explorer
#
# Purpose:
# Interactive Shiny application for exploring Schedule 1 species listed under
# the Species at Risk Act (SARA). The application integrates:
#   • Species profile information
#   • Threat classifications
#   • Recovery planning documents
#   • Registry summaries
#
# Primary data source:
# Species at Risk Public Registry
################################################################################

library(shiny)
library(dplyr)
library(DT)
library(bslib)

################################################################################
# LOAD DATA
################################################################################
# These RDS files are generated during data processing and contain:
#
# species_master:
#   One record per species / designatable unit (DU).
#   Includes status, taxonomy, range, image metadata, etc.
#
# species_profiles:
#   Species profile text scraped from Registry species pages.
#   Includes description, habitat, biology, threats, population information.
#
# species_threats:
#   Screening-level threat classifications derived from profile text.
#   One row per DU-threat combination.
#
# species_documents:
#   Recovery planning and registry documents associated with each DU.
################################################################################

species_data <- readRDS("data/species_master.rds")
species_master <- species_data$data
scrape_date <- species_data$scrape_date
species_profiles <- readRDS("data/species_profiles.rds")
species_threats <- readRDS("data/species_threats.rds")
species_documents <- readRDS("data/species_documents.rds")
recovery_compliance <- readRDS("data/recovery_compliance.rds")
progress_compliance <- readRDS("data/progress_report_compliance.rds")

################################################################################
# APPLICATION DATASETS
################################################################################

species_master_app <- species_master %>%
  filter(sara_status %in% c("Threatened", "Endangered","Extirpated","Special Concern"))

################################################################################
# IMAGE FILE REFERENCES
# Local species images are stored in:
#   www/images/
# Image filenames follow the convention:
#   "Atlantic Whitefish" -> "atlantic_whitefish.jpg"
################################################################################

species_master_app <- species_master_app %>%
  mutate(image_file = paste0(common_name %>%
                               tolower() %>%
                               gsub(" ", "_", .),".jpg"))

################################################################################
# THREAT DATA USED BY APP
# Restrict threat analyses to the same SARA statuses used elsewhere in the app.
################################################################################

species_threats_app <- species_threats %>%
  filter(sara_status %in% c("Threatened", "Endangered", "Extirpated","Special Concern"))

################################################################################
# RANGE, SARA STATUS, PANEL STYLE FILTER OPTIONS
################################################################################

range_choices <- c("All", sort(unique(
  unlist(strsplit(paste(species_master_app$range_text,
                        collapse = "; "),"; ")))))

sara_choices <- c("All","Endangered","Threatened","Extirpated","Special Concern")

panel_style <- "
  background-color: #f8f9fa;
  border: 1px solid #dee2e6;
  border-radius: 6px;
"
summary_card <- function(value, label, bg, border){
  div(style = paste0(
      "background:", bg, ";
       border:1px solid ", border, ";
       border-radius:10px;
       padding:15px;
       text-align:center;
       min-height:120px;
       height:120px;
       box-shadow:0 1px 3px rgba(0,0,0,0.08);"),
    h2(style = "margin:0;", value),
    p(style = "margin-top:5px;
               margin-bottom:0;
               font-size:16px;",
      label))
}

################################################################################
# RECOVERY PLANNING COVERAGE DATASET
# Create a DU-level recovery planning summary used in the "Recovery Planning" tab.
# For each Designatable Unit (DU), determine:
# Document identification is performed using Registry metadata and document
# titles.
################################################################################

# derived recovery data dataset
recovery_coverage <- species_master_app %>%
  # ensure one record per DU
  distinct(record_name, sara_status, range_text) %>%
  # join recovery planning metrics
  left_join(species_documents %>%
      group_by(record_name) %>%
      summarise(
        # Recovery Strategy present?
        has_recovery_strategy = any(grepl("Recovery Strategy",
                documentTitle,ignore.case = TRUE)),
        
        # Action Plan present?
        has_action_plan =
          any(primary_document_type == "Action Plan") |
          any(grepl("Action Plan", documentTitle,ignore.case = TRUE)),
        
        # Management Plan present?
        has_management_plan = any(
          grepl("Management Plan", documentTitle, ignore.case = TRUE)),
        
        # Latest planning document date
        latest_recovery_planning_document = {
          planning_idx <- planning_document_type %in% c(
            "Recovery Strategy","Action Plan", "Progress Report",
            "Recovery Strategy & Action Plan")
          
          dates <- as.Date(documentCreateDate[planning_idx])
          if(length(dates) == 0 || all(is.na(dates))) {
            as.Date(NA)
          } else {
            max(dates, na.rm = TRUE)}},
        
        # Latest planning document type
        latest_document_type = {
          
          planning_idx <- planning_document_type %in% c(
            "Recovery Strategy", "Action Plan",
            "Progress Report", "Recovery Strategy & Action Plan")
          
          dates <- as.Date(documentCreateDate[planning_idx])
          types <- planning_document_type[planning_idx]
          
          if(length(dates) == 0 || all(is.na(dates))) {
            NA_character_
          } else {
            types[which.max(dates)]}},.groups="drop"), by="record_name")%>%
  
  # Calculate document age
  mutate(latest_recovery_planning_document = as.Date(
    ifelse(is.infinite(latest_recovery_planning_document), NA,
           latest_recovery_planning_document),origin = "1970-01-01"),
    
    years_since_recovery_planning = round( 
      as.numeric(Sys.Date() - latest_recovery_planning_document) / 365.25, 1))

recovery_coverage <- recovery_coverage %>%
  left_join(progress_compliance %>%
      mutate(latest_progress_report_date = as.Date(latest_progress_report_date)) %>%
      select( record_name, compliance_status, latest_progress_report_date),by = "record_name") %>%
  mutate(
    compliance_status = case_when(
    sara_status == "Special Concern" ~ "Not Required",TRUE ~ compliance_status),
    latest_document_date = pmax(
      latest_recovery_planning_document,latest_progress_report_date,na.rm = TRUE),
    
    latest_document_type = case_when(
      !is.na(latest_progress_report_date) &
        latest_progress_report_date == latest_document_date ~
        "Progress Report", TRUE ~ latest_document_type),
    
    years_since_recovery_planning = round(
        as.numeric(Sys.Date() - latest_document_date) / 365.25, 1))

recovery_coverage <- recovery_coverage %>%
  left_join(species_master_app %>%
      select(record_name, common_name, taxonomyId), by = "record_name")

################################################################################
# REUSABLE UI COMPONENTS
# Several tabs use similar filtering controls:SARA Status, Range (Province),
# Species, Designatable Unit (DU)
# To avoid duplicating UI code throughout the application, filter panels are
# defined once and inserted into tab layouts as needed.
# Each object below creates a left-side filter panel used within a specific tab.
################################################################################

################################################################################
# SPECIES INFORMATION FILTER PANEL
################################################################################
species_selector_ui <- column(
  width = 2,
  div(class = "threat-card",
      h4(icon("filter"), " Filters", class = "card-title"),
      tags$p(style = "color:#6c757d; font-size:13px;",
      "Filter species records by status, range, species, and DU."),
  selectInput("taxa", "Taxonomic Group",
              choices = c( "All", sort(unique(species_master_app$taxonomyId))), 
              selected = "All"),   
  selectInput("status","SARA Status", choices = sara_choices),
  selectInput("range","Range", choices = range_choices),
  selectInput("species","Species",
              choices = sort(unique(species_master_app$common_name))),
  selectInput("record","Population / Designatable Unit", choices = NULL)))

################################################################################
# SPECIES DOCUMENTS FILTER PANEL
################################################################################
species_documents_selector_ui <- column(
  width = 2,
  div(class = "threat-card",
      h4(icon("filter"), " Filters",class = "card-title"),
      tags$p(style = "color:#6c757d; font-size:13px;",
             "Filter species records by status, range, species, and DU."),
      selectInput("taxa_docs", "Taxonomic Group",
                  choices = c("All", sort(unique(species_master_app$taxonomyId))),
                  selected = "All"),
    selectInput("status_docs", "SARA Status", choices = sara_choices),
    selectInput("range_docs","Range", choices = range_choices),
    selectInput("species_docs","Species", choices = sort(unique(species_master_app$common_name)) ),
    selectInput("record_docs", "Population / Designatable Unit",choices = NULL)))

################################################################################
# THREAT EXPLORATION FILTER PANEL
################################################################################
explore_selector_ui <- column(
  width = 3,
  div(class = "threat-card",
      h4(icon("filter"), " Filters", class = "card-title"),
      tags$p(style = "color:#6c757d; font-size:13px;",
      "Filter species records by threat category, status, and range."),
      selectInput("explore_taxa", "Taxonomic Group",
                  choices = c("All", sort(unique(species_master_app$taxonomyId))),
                  selected = "All"),
    selectInput("explore_threat", "Threat Group",choices = NULL),
    selectInput("explore_status", "SARA Status",choices = sara_choices,selected = "All"),
    selectInput("explore_range","Range",choices = range_choices)))

################################################################################
# RECOVERY PLANNING FILTER PANEL
################################################################################
recovery_planning_selector_ui <- column(
  width = 2,
  div(class = "threat-card",
      h4(icon("filter"), " Filters", class = "card-title"),
      tags$p(style = "color:#6c757d; font-size:13px;",
             "Filter species records by status and range."),
      selectInput("planning_taxa", "Taxonomic Group",
                  choices = c("All", sort(unique(species_master_app$taxonomyId))),
                  selected = "All"),
            selectInput("planning_status", "SARA Status", choices = sara_choices,
                        selected = "All"),
            selectInput("planning_range","Range",
                        choices = range_choices)))

################################################################################
# PROVINCE SUMMARY FILTER PANEL
################################################################################
province_selector_ui <- column(
  width = 2,
  div(class = "threat-card",
      h4(icon("filter")," Filters", class = "card-title"),
      tags$p(style = "color:#6c757d; font-size:12px;",
             "Filter species records by range."),
      selectInput("province_taxa", "Taxonomic Group",
                  choices = c("All", sort(unique(species_master_app$taxonomyId))),
                  selected = "All"),
  selectInput("province_range", "Range", choices = range_choices)))

################################################################################
# USER INTERFACE (UI)
################################################################################
# The application is organized into six major tabs:
#
#   1. Dashboard
#   2. Species Information
#   3. Species Documents
#   4. Recovery Planning
#   5. Threat Summary
#   6. Explore Threats
#   7. Province Summary
#
# Reusable selector panels defined above are inserted into tab layouts to
# provide consistent filtering behaviour across the application.
################################################################################

ui <- fluidPage(
  #application theme
  tags$head(
    tags$style(HTML("
      .small-box {
        text-align: center;
      }
      .small-box h3 {
        font-size: 32px;
        font-weight: bold;
      }
      .small-box p {
        font-size: 18px;
      }
      table.dataTable a:hover {
      text-decoration: underline;
      color: #0a58ca;
      background-color: #f8f9fa;
      font-weight: 500;
      }
      table.dataTable tbody tr:hover {
      background-color: #f5f9ff !important;
      }
      table.dataTable.stripe tbody tr.odd {
      background-color: #fafafa;
      }
      table.dataTable thead th {
      font-weight: 700;
      }
      .badge-yes {
      background-color: #d4edda;
      color: #155724;
      padding: 5px 12px;
      border-radius: 14px;
      font-size: 12px;
      font-weight: 700;
      }
      .badge-no {
      background-color: #f8d7da;
      color: #721c24;
      padding: 5px 12px;
      border-radius: 14px;
      font-size: 12px;
      font-weight: 700;
      }
      .threat-card {
      background-color: #f8f9fa;
      border: 1px solid #d0d7de;
      border-radius: 8px;
      padding: 12px;
      box-shadow: 0 2px 6px rgba(0,0,0,0.08);
      margin-bottom: 15px;
      }
      .form-group {
      margin-bottom: 10px;
      }
      .status-endangered {
      background-color: #f8d7da;
      color: #721c24;
      padding: 4px 10px;
      border-radius: 12px;
      font-size: 12px;
      font-weight: 600;
      white-space: nowrap;
      }
      .status-threatened {
      background-color: #fff3cd;
      color: #856404;
      padding: 4px 10px;
      border-radius: 12px;
      font-size: 12px;
      font-weight: 600;
      white-space: nowrap;
      }
      .status-extirpated {
      background-color: #e2d9f3;
      color: #5a3e8c;
      padding: 4px 10px;
      border-radius: 12px;
      font-size: 12px;
      font-weight: 600;
      white-space: nowrap;
      }
      .status-special {
      background-color: #d1ecf1;
      color: #0c5460;
      padding: 4px 10px;
      border-radius: 12px;
      font-size: 12px;
      font-weight: 600;
      white-space: nowrap;
      }
      .recovery-card {
      border-radius: 8px;
      padding: 12px;
      text-align: center;
      min-height: 120px;
      }
      .recovery-card h3 {
       margin-top: 5px;
       margin-bottom: 5px;
      }
      .recovery-card p {
       margin-bottom: 0;
       font-weight: 500;
      }
      .footer-card {
      padding:15px;
      max-width: 700px;
      font-size:0.9em;
      }
      .card-title {
      color: #2c5aa0;
      font-weight: 600;
      }
      .badge-na {
      background-color: #e9ecef;
      color: #495057;
      padding: 5px 12px;
      border-radius: 14px;
      font-size: 12px;
      font-weight: 700;
      }
      .content-card {
      background:white;
      border:1px solid #dee2e6;
      border-radius:10px;
      padding:20px;
      margin-bottom:15px;
      }
      .nav-pills .nav-link {
      color: #2c5aa0 !important;
      }
      .nav-pills .nav-link:hover {
      background-color: #eef4ff;
      }
      .nav-pills .nav-link.active {
      background-color: #2c5aa0;
      font-weight: 700 !important;
      color:white !important;
      border-radius: 8px;
}"))),
  theme = bs_theme(version = 5, bootswatch = "cosmo"),
  
  # Main navigation tabs
  div(style = "padding: 10px 15px 0px 15px;",
      tabsetPanel(id = "main_tabs",type="pills",
                  
                  # DASHBOARD
                  tabPanel(title = tagList(icon("house"),"Dashboard"), value = "dashboard",
                     br(),
                     div(style = "background-color:#f8f9fa;
                     border:1px solid #dee2e6;
                     border-radius:10px;
                     padding:15px;
                     margin-bottom:5px;
                     margin-top:10px;",
                         h2(strong("Canadian Species at Risk Explorer"),
                            style = "margin-top:0; margin-bottom:5px; color:#2c5aa0;"),
                         p(style = "margin-bottom:0; font-size:16px;",
                           "Explore species profiles, threats, and recovery planning information for SARA Schedule 1 species.")),
                     tags$img(src = "images/dashboard_banner.jpg",
                       style = " width:100%;
                       height:180px;
                       object-fit:cover;
                       border-radius:10px;
                       background-color:#f8f9fa;
                       border:1px solid #dee2e6;
                       margin-top:15px;
                       margin-bottom:15px;"),
                     div(style = "background-color:#f8f9fa;
                       border:1px solid #dee2e6;
                       border-radius:10px;
                       padding:15px;
                       margin-top:5px;
                       margin-bottom:15px;",
                     h3("Quick Start",style = "margin-top:0;"),
                     tags$ol(
                       tags$li("Use Species Information to explore species profiles and recovery information."),
                       tags$li("Use Species Documents to review recovery strategies, action plans, management plans, and progress reports."),
                       tags$li("Use Threat Summary and Explore Threats to examine threats affecting species recovery."),
                       tags$li("Use Province Summary to summarize species and recovery planning information by jurisdiction.")),
                     h4("Important Notes"),
                     tags$ul(tags$li("Threat categories are screening-level classifications derived from Species at Risk Public Registry profile information and should not be interpreted as formal threat assessments."),
                             tags$li("Recovery planning metrics are based on Recovery Strategies, Action Plans, Management Plans, and Progress Reports identified within Registry planning documents.")),
                     fluidRow(column(6,h4("Data Sources"),
                         tags$ul(
                           tags$li("Species at Risk Public Registry"),
                           tags$li("Species profile pages"),
                           tags$li("Recovery and planning documents"))),
                       column(6,h4("Application Coverage"),
                         tags$ul(
                           tags$li(
                             paste(n_distinct(species_master_app$record_name),
                               "species records")),
                           tags$li(paste(n_distinct(species_documents$document_id),
                               "documents")),
                           tags$li(paste(n_distinct(species_threats_app$threat_category),
                               "threat groups"))))))),
                  
                  # SPECIES INFORMATION
                  tabPanel(title = tagList(icon("fish"),"Species Information"),
                           value = "species_info",
                           br(),
                           fluidRow(species_selector_ui,
                                    column(9,
                                    style = "margin-top: -8px;",
                                    uiOutput("species_header"),
                                    br(),
                                    
                                    hr(style = "margin-top:20px; margin-bottom:20px;"),
                                    h3("Description"),
                                    textOutput("description"),
                                    
                                    hr(style = "margin-top:20px; margin-bottom:20px;"),
                                    h3("Population"),
                                    textOutput("population"),
                                    
                                    hr(style = "margin-top:20px; margin-bottom:20px;"),
                                    h3("Habitat"),
                                    textOutput("habitat"),
                                    
                                    hr(style = "margin-top:20px; margin-bottom:20px;"),
                                    h3("Biology"),
                                    textOutput("biology"),
                                    
                                    hr(style = "margin-top:20px; margin-bottom:20px;"),
                                    h3("Threats"),
                                    textOutput("profile_threats"),
                                    
                                    br(),
                                    div(class="content-card",
                                        h3(textOutput("n_threats")),
                                    DTOutput("threats"),
                                    tags$p(style = "font-style: italic; color: #666666;",
                                           "Threat groups indicate the presence of a threat within profile text and should be considered screening-level classifications rather than formal threat assessments."))
                                    ))),
                  
                  # SPECIES DOCUMENTS TAB
                  tabPanel(title = tagList(icon("file-lines"),"Species Documents"),
                           value = "species_docs",
                           br(),
                           fluidRow(species_documents_selector_ui, 
                                    column(10,
                                           uiOutput("documents_species_banner"),
                                           div(class = "threat-card",
                                           h3("Species Recovery Documents Compliance"),
                                           uiOutput("compliance_summary")),
                                           div(class = "threat-card",
                                           h3("Legislated Recovery Documents", style = "margin-top:0;"),
                                           DTOutput("documents"),
                                           tags$p(style = "font-size:12px; color:gray;",
                                             tags$b("Document Notes: "),
                                             "Track Recovery Strategies, Action Plans, Management Plans, and Progress Reporting obligations for ", tags$i("Species at Risk Act"), " listed species. Efforts were made to removed 'Proposed' documents."
                                           ))))),
                 
                   # RECOVERY PLANNING TAB
                  tabPanel(title = tagList(icon("clipboard-list"),"Recovery Planning"),
                           value = "recovery_plan",
                           div(style = "max-width:1200px; margin-left:10px;",
                               br(),
                               h3("Recovery Planning Coverage"),
                               tags$p(class = "text-muted", style = "margin-bottom:20px;",
                                 "Track Recovery Strategies, Action Plans, Management Plans, and Progress Reporting obligations for SARA-listed species."),
                               fluidRow(recovery_planning_selector_ui, 
                                        column(10,
                                               div(class = "threat-card",
                                               uiOutput("planning_summary"),
                                               DTOutput("recovery_coverage", width = "100%"),
                                               tags$p(style = "font-style: italic; color: #666666;",
                                                 "Recovery planning coverage is based on recovery strategies, action plans, management plans, and associated progress reports identified within Species at Risk Public Registry planning documents."
                                                 )))))),
                  
                  # THREAT SUMMARY TAB
                  tabPanel(title = tagList(icon("triangle-exclamation"), "Threat Summary"),
                           value = "threat_sum",
                    div(style = "max-width:1200px; margin-left:10px;",
                      br(),
                      h3("Threats to Species Recovery"),
                      tags$p(class = "text-muted", style = "margin-bottom:20px;",
                        "Threat frequencies are based on threat categories identified from Species at Risk Public Registry species profile information."),
                      fluidRow(column(2,
                          div(class = "threat-card",
                            h4(class = "card-title", icon("filter"), " Filters"),
                            tags$p(style = "color:#6c757d; font-size:13px;",
                              "Filter threat summaries by range and taxonomic group."),
                            selectInput("threat_taxa", "Taxonomic Group",
                                        choices = c("All",
                                                    sort(unique(species_master_app$taxonomyId))),
                                        selected = "All"),
                            selectInput("threat_range", "Range",
                                        choices = range_choices, selected = "All"))),
                        # Main content
                        column(10,fluidRow(
                            column(6,
                              div(class = "threat-card",
                                h3("DUs with the Most Threat Categories",
                                   style = "text-align:center; margin-top:0;"),
                                DTOutput("top_threat_species"))),
                            column(6,
                              div(class = "threat-card",
                                h3("Most Common Threat Categories",
                                  style = "text-align:center; margin-top:0;"),
                                DTOutput("common_threats")))))))),
                  
                  # EXPLORE THREATS TAB
                  tabPanel(title = tagList(icon("magnifying-glass"),"Explore Threats"),
                           value = "explore_thr",
                           div(style = "max-width:1200px; margin-left:10px;",
                               br(),
                               h3("Explore Threats"),
                               tags$p(class = "text-muted",
                                      "Explore DUs associated with a selected threat category."),
                               fluidRow(explore_selector_ui,
                                        column(9,
                                               div(class = "threat-card",
                                                   uiOutput("explore_species_count"),
                                               tags$p(class = "text-muted",
                                                 "Click a species record to open the Species Information tab."),
                                               DTOutput("explore_species"))
                                        )))),
                  # PROVINCIAL SUMMARY TAB
                  tabPanel(title = tagList(icon("map"),"Province Summary"),
                           value = "prov_sum",
                           div(style = "max-width:1200px; margin-left:10px;",
                               br(),
                               fluidRow(province_selector_ui,
                                        column(9,
                                               h3(textOutput("province_title")),
                                               fluidRow(uiOutput("province_cards")))),
                               br(),
                               fluidRow(column(6,
                                               div(class = "threat-card",
                                               h3("Species Records by SARA Status",
                                                  style="text-align:center;"),
                                               DTOutput("province_status")),
                                               br(),
                                               div(class = "threat-card",
                                               h3("Recovery Planning Status",
                                                  style = "text-align:center"),
                                               uiOutput("province_recovery_summary"))),
                                        column(6,
                                               div(class = "threat-card",
                                               h3("Most Common Threat Categories",
                                                  style="text-align:center;"),
                                               DTOutput("province_top_threats")))
                                        ),
                               br(),
                               div(class = "threat-card",
                               h3("Species Records"),
                               DTOutput("province_species_table")))))),
  # FOOTER INFORMATION
  hr(),
  tags$div(style = "font-size:12px;",
    class = "footer-card",
    "Canadian Species at Risk Explorer v3.0",
    tags$br(),
    paste("Registry data last refreshed:", format(scrape_date, "%Y-%m-%d")),
    tags$br(),
    paste("Application updated:", format(Sys.Date(), "%Y-%m-%d"))))

################################################################################
################################################################################
# server
################################################################################
################################################################################
server <- function(input, output, session){
  
  ##############################################################################
  # SPECIES SELECTION REACTIVES
  ##############################################################################
  selected_species <- reactive({
    species_master_app %>%
      filter(record_name == input$record)%>%
      slice(1)
  })
  
  selected_record_from_explore <- reactiveVal(NULL)
  
  ##############################################################################
  # COMPLIANCE REACTIVES
  ##############################################################################
  selected_compliance <- reactive({
    recovery_compliance %>%
      filter(record_name == input$record)
  })
  
  selected_progress_compliance <- reactive({
    progress_compliance %>%
      filter(record_name == input$record_docs)
  })
  
  ##############################################################################
  # THREAT EXPLORATION REACTIVES
  ##############################################################################
  explore_results <- reactive({
    dat <- species_threats_app %>%
      left_join(species_master_app %>%
                  select(record_name, range_text, taxonomyId),by = "record_name") %>%
      filter(threat_category == input$explore_threat)
    if(input$explore_range != "All"){
      dat <- dat %>%
        filter(grepl(input$explore_range, range_text, fixed = TRUE))}
    if(input$explore_taxa != "All"){
      dat <- dat %>%
        filter(taxonomyId == input$explore_taxa)
    }
    if(input$explore_status != "All"){
      dat <- dat %>%
        filter(sara_status == input$explore_status)}
    dat %>%
      distinct(common_name, record_name, sara_status)
  })
  
  selected_profile <- reactive({
    prof <- species_profiles %>%
      filter(record_name == input$record)
    has_text <- nrow(prof) > 0 &&
      any(
        !is.na(prof$description) |
          !is.na(prof$population) |
          !is.na(prof$habitat) |
          !is.na(prof$biology))
    if(!has_text){
      prof <- species_profiles %>%
        filter(common_name == input$species) %>%
        filter(!is.na(description)) %>%
        slice(1)
    }
    prof
  })
  
  ##############################################################################
  # PROVINCE SUMMARY REACTIVES
  ##############################################################################
  province_species <- reactive({
    dat <- species_master_app
    if(input$province_range != "All"){
      dat <- dat %>%
        filter(grepl(input$province_range, range_text, fixed = TRUE))
    }
    if(input$province_taxa != "All"){
      dat <- dat %>%
        filter(taxonomyId == input$province_taxa)
    }
    dat
  })
  
  province_recovery_summary <- reactive({
    dat <- recovery_coverage
    if(input$province_range != "All"){
      dat <- dat %>%
        filter(grepl(input$province_range, range_text, fixed = TRUE))
    }
    if(input$province_taxa != "All"){
      dat <- dat %>%
        filter(taxonomyId == input$province_taxa)
    }
    dat %>%
      count(compliance_status)
  })
  
  ##############################################################################
  # CASCADING FILTER OBSERVERS
  ##############################################################################
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
  # SPECIES FILTER OBSERVERS
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
  # dynamic range selector
  observe({
    req(input$taxa)
    dat <- species_master_app
    if(input$taxa != "All"){
      dat <- dat %>%
        filter(taxonomyId == input$taxa)
    }
    if(!is.null(input$status) && input$status != "All"){
      dat <- dat %>%
        filter(sara_status == input$status)
    }
    ranges <- c("All", sort(unique(
      unlist(strsplit( paste(dat$range_text, collapse = "; "), "; ")))))
    updateSelectInput(session,"range", choices = ranges,
                      selected = if(!is.null(input$range) &&
                                    input$range %in% ranges) 
                        input$range else "All")
  })
  
  # dynamic status
  observe({
    req(input$taxa)
    dat <- species_master_app
    if(input$taxa != "All"){
      dat <- dat %>%
        filter(taxonomyId == input$taxa)
    }
    updateSelectInput(session, "status",
                      choices = c("All", sort(unique(dat$sara_status))))
  })
  
  # species selector
  observe({
    species_choices <- species_master_app
    if(input$status != "All"){
      species_choices <- species_choices %>%
        filter(sara_status == input$status)
    }
    if(input$range != "All"){
      species_choices <- species_choices %>%
        filter(grepl(input$range, range_text, fixed = TRUE))
    }
    if(input$taxa != "All"){
      species_choices <- species_choices %>%
        filter(taxonomyId == input$taxa)
    }
    choices <- sort(unique(species_choices$common_name))
    selected_species <- if(
      !is.null(input$species) &&
      input$species %in% choices) {
      input$species} else if(length(choices) > 0) {
        choices[1]
    } else {
      character(0)
    }
    updateSelectInput(session, "species", choices = choices, selected = selected_species)
  })

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
  # DOCUMENT FILTER OBSERVERS
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
  # dynamic document range selector
  observe({
    req(input$taxa_docs)
    dat <- species_master_app
    if(input$taxa_docs != "All"){
      dat <- dat %>%
        filter(taxonomyId == input$taxa_docs)
    }
    if(!is.null(input$status_docs) &&
       input$status_docs != "All"){
      dat <- dat %>%
        filter(sara_status == input$status_docs)
    }
    ranges <- c("All", sort(unique(unlist(
      strsplit(paste(dat$range_text, collapse = "; "), "; ")))))
    updateSelectInput(session, "range_docs", choices = ranges)
  })
  
  # dynamic document status selector
  observe({
    req(input$taxa_docs)
    dat <- species_master_app
    if(input$taxa_docs != "All"){
      dat <- dat %>%
        filter(taxonomyId == input$taxa_docs)
    }
    statuses <- c("All", sort(unique(dat$sara_status)))
    updateSelectInput(session, "status_docs",
                      choices = statuses,
                      selected = if(
        !is.null(input$status_docs) &&
        input$status_docs %in% statuses) input$status_docs else "All")
  })
  
  # Document selector
  observe({
    species_choices <- species_master_app
    if(input$status_docs != "All"){
      species_choices <- species_choices %>%
        filter(sara_status == input$status_docs)
    }
    if(input$range_docs != "All"){
      species_choices <- species_choices %>%
        filter(grepl(input$range_docs,range_text, fixed = TRUE))
    }
    if(input$taxa_docs != "All"){
      species_choices <- species_choices %>%
        filter(taxonomyId == input$taxa_docs)
    }
    choices <- sort(unique(species_choices$common_name))

    selected_species <- if(
      !is.null(input$species_docs) &&
      input$species_docs %in% choices
    ) {
      input$species_docs
    } else if(length(choices) > 0) {
      choices[1]
    } else {
      character(0)
    }
    updateSelectInput(session, "species_docs",choices = choices, selected = selected_species)
  })
  
  ##############################################################################
  # THREAT SELECTOR
  ##############################################################################
  observe({
    updateSelectInput(session, "explore_threat",
      choices = sort(unique(species_threats_app$threat_category)))
  })
  
  ##############################################################################
  # TAB NAVIGATION & SYNCHRONIZATION
  ##############################################################################
  observeEvent(input$explore_species_rows_selected, {
    row <- input$explore_species_rows_selected
    if(length(row) == 0) return()
    selected <- explore_results()[row, ]
    selected_record_from_explore(selected$record_name)
    updateSelectInput(session,"species", selected = selected$common_name)
    updateTabsetPanel(session, "main_tabs", selected = "species_info")
  })
  
  # CARRY CURRENT SPEICES INFORMATION SELECTIONS INTO SPECIES DOCUMENTS WHEN
  # TAB IS OPEN
  observeEvent(input$main_tabs, {
    req(input$species)
    if(input$main_tabs == "species_docs") {
      updateSelectInput(session, "species_docs", selected = input$species)
    }
  })
  
  ##############################################################################
  # SPECIES / DU SELECTION OBSERVERS
  ##############################################################################
  observe(
    {req(input$species)
      records <- species_master_app %>%
        filter(common_name == input$species)
      if(input$status != "All"){
        records <- records %>%
          filter(sara_status == input$status)
      }
      if(input$range != "All"){
        records <- records %>%
          filter(grepl(input$range, range_text,fixed = TRUE))
      }
      if(input$taxa != "All"){
        records <- records %>%
          filter(taxonomyId == input$taxa)
      }
      records <- records %>%
        arrange(record_name) %>%
        pull(record_name)
      updateSelectInput(session, "record", choices = records,
                        selected = if(length(records) > 0)
                          records[1] else character(0))})

  observeEvent(
    c(input$species_docs, input$status_docs, input$range_docs, input$taxa_docs),
    {records <- species_master_app %>%
        filter(common_name == input$species_docs)
      if(input$status_docs != "All"){
        records <- records %>%
          filter(sara_status == input$status_docs)
      }
      if(input$range_docs != "All"){
        records <- records %>%
          filter(grepl(input$range_docs,range_text,fixed = TRUE))
      }
    if(input$taxa_docs != "All"){
      records <- records %>%
        filter(taxonomyId == input$taxa_docs)
    }
      records <- records %>%
        arrange(record_name) %>%
        pull(record_name)
      updateSelectInput(session, "record_docs", choices = records,
                        selected = if(length(records) > 0)
                          records[1] else character(0))})
  
  ##############################################################################
  # SPECIES INFORMATION OUTPUTS
  ##############################################################################
  output$species_header <- renderUI({
    sp <- selected_species()
    species_url <- paste0(
      "https://species-registry.canada.ca/index-en.html#/species/",
      sp$id[1])
    img_file <- sp$local_image_file[1]
    has_image <- !is.na(img_file) &&
      file.exists(file.path("www", "images", img_file))
    info_box_width <- if (has_image) "550px" else "450px"
    
    info_box <- tags$div(
      style = paste0("background-color:#f8f9fa;
      border:1px solid #dee2e6;
      border-radius:10px;
      padding:12px;
      line-height:1.8;
      max-width:", info_box_width, ";"),
      
      tags$p(style = "margin-bottom:10px;",
        tags$a("View Species Profile on the SAR Registry",
               href = species_url,target = "_blank")),
      
      tags$div(tags$b("Designatable Unit: "), sp$record_name),
      tags$div(tags$b("SARA Status: "), sp$sara_status, style = "margin-top:4px;"),
      tags$div(tags$b("COSEWIC Status: "), sp$cosewic_status, style = "margin-top:4px;"),
      tags$div(tags$b("Range: "), sp$range_text, style = "margin-top:4px;"))
    
    if (!has_image) {
      tagList(
        h3(sp$common_name, style = "margin-top:0; margin-bottom:5px;"),
        tags$h4(tags$em(sp$scientific_name),
          style = "margin-top:0; margin-bottom:15px; color:#495057;"),
        info_box)
      
    } else {
      
      tagList(
        h3(sp$common_name, style = "margin-top:0; margin-bottom:5px;"),
        tags$h4(tags$em(sp$scientific_name),
                style = "margin-top:0; margin-bottom:15px; color:#495057;"),
        tags$div(style = "display:flex;
                 align-items:flex-start;
                 gap:25px;
                 flex-wrap:wrap;",
                 tags$div(style = "flex:0 0 auto;",
                          tags$div(
                            style = "display:flex;
                            flex-direction:column;
                            align-items:flex-start;",
                            tags$img(
                              src = paste0("images/", img_file),
                              style = "
                              max-width:300px;
                              max-height:220px;
                              object-fit:contain;
                              border-radius:8px;
                              display:block;"),
                            
                            if (
                              !is.na(sp$photoCopyright[1]) &&
                              sp$photoCopyright[1] != ""
                            ) {
                              tags$p(style = "font-size:11px;
                              color:grey;
                              margin-top:5px;
                                     margin-bottom:0;",
                                     sp$photoCopyright[1])})),
            tags$div(style = "flex:0 0 auto;", info_box)))
  }})
  
        
# description
output$description <- renderText({
  selected_profile() %>%
    pull(description)
})

# habitat
output$habitat <- renderText({
  selected_profile() %>%
    pull(habitat)
})

# biology
output$biology <- renderText({
  selected_profile() %>%
    pull(biology)
})

#population
output$population <- renderText({
  selected_profile() %>%
    pull(population)
})

# number of threats
output$n_threats <- renderText({
  n <- species_threats_app %>%
    filter(record_name == input$record) %>%
    distinct(threat_category) %>%
    nrow()
  paste(n, ifelse(n == 1, "Threat Category", "Threat Categories"))
  
})

# threats
output$profile_threats <- renderText({
  selected_profile() %>%
    pull(threats)
})

# threats
output$threats <- renderDT({
  dat <- species_threats_app %>%
    filter(record_name == input$record) %>%
    select(threat_category) %>%
    distinct()
  names(dat) <- ""
  datatable(dat, rownames = FALSE, options = list(pageLength = 10, dom = "t"))
})

##############################################################################
# RECOVERY PLANNING OUTPUTS
##############################################################################
output$recovery_coverage <- renderDT({
  dat <- recovery_coverage
  if(input$planning_status != "All"){
    dat <- dat %>%
      filter(sara_status == input$planning_status)}
  if(input$planning_range != "All"){
    dat <- dat %>%
      filter(grepl(input$planning_range, range_text, fixed = TRUE))}
  if(input$planning_taxa != "All"){
    dat <- dat %>%
      filter(taxonomyId == input$planning_taxa)}
  dat %>%
    mutate(`Recovery Strategy` = case_when(
      sara_status == "Special Concern" ~
        '<span class="badge-na">Not Required</span>',
      coalesce(has_recovery_strategy, FALSE) ~
        '<span class="badge-yes">Available</span>',
        TRUE ~ '<span class="badge-no">Not Available</span>'),
      
      `Action Plan` = case_when(
        sara_status == "Special Concern" ~
          '<span class="badge-na">Not Required</span>',
        coalesce(has_action_plan, FALSE) ~
          '<span class="badge-yes">Available</span>',
        TRUE ~'<span class="badge-no">Not Available</span>'),
      
      `Management Plan` = case_when(
        coalesce(has_management_plan, FALSE) ~
          '<span class="badge-yes">Available</span>',
        sara_status %in% c("Endangered", "Threatened", "Extirpated") ~
          '<span class="badge-na">Not Required</span>',
        TRUE ~ '<span class="badge-no">Not Available</span>'),
      
      compliance_status = case_when(
        compliance_status == "Up to Date" ~ "✅Up to Date",
        compliance_status == "Overdue" ~ "🔴 Report Overdue",
        compliance_status == "Not Yet Due" ~ "🟡 Not Yet Required",
        compliance_status %in% c("Not Applicable", "Not Required") ~
          "⚪ Not Required", TRUE ~ compliance_status)) %>%
    
    arrange(factor(compliance_status,
                   levels = c("🔴 Report Overdue","🟡 Not Yet Required",
                              "✅Up to Date", "⚪ Not Required")), record_name) %>%
    
    select(record_name, sara_status, `Recovery Strategy`, `Action Plan`,
           `Management Plan`, compliance_status) %>%
    
    rename(`Designatable Unit` = record_name,
           `SARA Status` = sara_status,
           `Progress Report Status` = compliance_status) %>%
    
    datatable(rownames = FALSE, width = "100%", escape = FALSE,
              options = list(pageLength = 10,
                             columnDefs = list(
                               list(className = "dt-center", targets = c(1, 2, 3, 4)),
                               list(width = "200px", targets = 0),
                               list(width = "110px", targets = 1),
                               list(width = "110px", targets = 2),
                               list(width = "110px", targets = 3),
                               list(width = "120px", targets = 4)))) %>%
    
    formatStyle(c("Recovery Strategy", "Action Plan", "Management Plan"),
                textAlign = "center") %>%
    
    formatStyle("Progress Report Status", target = "cell",
                backgroundColor = styleEqual(c(
                  "✅Up to Date", "🔴 Report Overdue", 
                  "🟡 Not Yet Required", "⚪ Not Required"),
                  c("#d4edda","#f8d7da", "#fff3cd", "#e9ecef")))
})

##############################################################################
# SPECIES DOCUMENT OUTPUTS
##############################################################################
output$documents <- renderDT({
  docs <- species_documents %>%
    filter(record_name == input$record_docs) %>%
    filter(!is.na(planning_document_type)) %>%
    mutate(attachment_date = as.Date(attachment_date),
      Document = paste0('<a href="',document_url,'" target="_blank">',
        attachment_title, '</a>'),
      planning_document_type = case_when(
        planning_document_type == "Recovery Strategy" ~
          "<span class='badge bg-primary'>Recovery Strategy</span>",
        planning_document_type == "Action Plan" ~
          "<span class='badge bg-success'>Action Plan</span>",
        planning_document_type == "Recovery Strategy & Action Plan" ~
          "<span class='badge bg-info text-dark'>Recovery Strategy & Action Plan</span>",
        planning_document_type == "Progress Report" ~
          "<span class='badge bg-warning text-dark'>Progress Report</span>",
        planning_document_type == "COSEWIC Assessment" ~
          "<span class='badge bg-secondary'>COSEWIC Assessment</span>",
        planning_document_type == "Management Plan" ~
          "<span class='badge bg-dark'>Management Plan</span>",
        TRUE ~ planning_document_type)) %>%
    arrange(desc(attachment_date)) %>%
    mutate(attachment_date = format( attachment_date,"%d %b %Y")) %>%
    select(planning_document_type, Document, attachment_date) %>%
    rename(`Document Type` = planning_document_type,
           `Publication Date` = attachment_date)
  datatable( docs,escape = FALSE,rownames = FALSE, options = list(
    columnDefs = list(
      list(width = "10%", targets = 0),
      list(width = "70%", targets = 1),
      list(width = "20%", targets = 2))))
})


# recovery document compliance
output$compliance_summary <- renderUI({
  comp <- selected_compliance()
  prog <- selected_progress_compliance()
  if(nrow(comp) == 0){return(tags$p("No compliance information available."))}
  if(nrow(selected_species()) > 0 &&
    !is.na(selected_species()$sara_status[1]) &&
    selected_species()$sara_status[1] == "Special Concern") {
    return(tags$div(style = "
                    background-color:#f8f9fa;
                    border:1px solid #dee2e6;
                    border-radius:10px;
                    padding:15px; ",
        tags$p( tags$b("Special Concern Species")),
        tags$p("Recovery Strategy and Progress Report requirements do not apply. Management planning requirements may apply under SARA.")))}
  
  # Recovery Strategy Compliance -------------------------------------------
  
  status <- comp$compliance_status[1]
  status_icon <- case_when(
    status == "On Time" ~ "🟢",
    status == "Late" ~ "🔴",
    TRUE ~ "⚪")
  
  status_colour <- case_when(
    status == "On Time" ~ "#198754",
    status == "Late" ~ "#dc3545",
    TRUE ~ "#6c757d")
  
  # Progress Report Compliance ---------------------------------------------
  
  prog_status <- if(nrow(prog) > 0)
    prog$compliance_status[1]
  else
    NA_character_
  
  prog_status_icon <- case_when(
    prog_status == "On Time" ~ "🟢",
    prog_status == "Late" ~ "🔴",
    prog_status == "Overdue" ~ "🟠",
    prog_status == "Not Yet Due" ~ "⚪",
    TRUE ~ "⚪" )
  
  prog_status_colour <- case_when(
    prog_status == "On Time" ~ "#198754",
    prog_status == "Late" ~ "#dc3545",
    prog_status == "Overdue" ~ "#fd7e14",
    prog_status == "Not Yet Due" ~ "#6c757d",
    TRUE ~ "#6c757d")
  
  tags$div(
    
    tags$div(tags$b("SARA Listing Date: "), format(as.Date(comp$listing_date[1]), "%d %b %Y")),
    tags$div(tags$b("Recovery Strategy Due: "), format(as.Date(comp$required_date[1]), "%d %b %Y")),
    tags$div(tags$b("First Recovery Strategy Published: "),
             format(as.Date(comp$first_recovery_strategy_date[1]), "%d %b %Y"),
             " ",
             tags$span(style = paste0(
               "color:", status_colour, "; font-weight:bold;"),
               paste(status_icon, status)),
      
      if(!is.na(comp$days_late[1]) && comp$days_late[1] > 0){
        paste0( " (",round(comp$days_late[1] / 365.25, 1), " years late)") }),
    
    if(
      !is.na(prog$recovery_strategy_date[1]) &&
      !is.na(comp$first_recovery_strategy_date[1]) &&
      prog$recovery_strategy_date[1] !=
      comp$first_recovery_strategy_date[1]){
      tags$div(tags$b("Current Recovery Strategy: "),
        format(as.Date(prog$recovery_strategy_date[1]), "%d %b %Y"))
    },
    tags$div(style = "margin-top:8px;"),
    tags$div(tags$b("Progress Report Due: "),
             ifelse(is.na(prog$progress_report_due_date[1]),
                    "Not Available",
                    format(as.Date(prog$progress_report_due_date[1]), "%d %b %Y"))),
    
    tags$div(tags$b("Most Recent Progress Report: "),
             ifelse(is.na(prog$latest_progress_report_date[1]),
                    "Not Available",
                    format(as.Date(prog$latest_progress_report_date[1]), "%d %b %Y")), " ",
             tags$span(style = paste0("color:", prog_status_colour,
                                      "; font-weight:bold;"),
                       paste(prog_status_icon, prog_status)),
             
             if(!is.na(prog$days_late[1]) &&
                prog$days_late[1] > 0 &&
                prog_status %in% c("Late", "Overdue")){
               paste0(" (",round(prog$days_late[1] / 365.25, 1)," years late)")}),
    
    tags$div(tags$b("Next Progress Report Due: "),
             ifelse(is.na(prog$next_progress_report_due_date[1]),
                    "Not Available",
                    format(as.Date(prog$next_progress_report_due_date[1]), "%d %b %Y"))))
})

output$documents_species_banner <- renderUI({
  req(input$species_docs)
  div(style = "
      background-color:#f8f9fa;
      border:1px solid #dee2e6;
      font-size: 24px;
      border-radius:10px;
      padding:15px;
      margin-bottom:15px;",
    tags$b("Viewing Documents for: "),
    tags$span(style = "color:#2c5aa0; font-weight:600;",strong(input$species_docs))
    )
})

##############################################################################
# THREAT SUMMARY OUTPUTS
##############################################################################
output$common_threats <- renderDT({
  dat <- species_threats_app %>%
    left_join(species_master_app %>%
                select(record_name, range_text, taxonomyId),by = "record_name")
  if(input$threat_range != "All"){
    dat <- dat %>%
      filter(grepl(input$threat_range, range_text, fixed = TRUE))}
  if(input$threat_taxa != "All"){
    dat <- dat %>%
      filter(taxonomyId == input$threat_taxa)}
  dat %>%
    distinct(record_name, threat_category) %>%
    count(threat_category,sort = TRUE, name = "Designatable Units") %>%
    rename(`Threat Category` = threat_category) %>%
    datatable(rownames = FALSE, 
              options = list(pageLength = 10, dom = "t",
                             ordering = FALSE,
                             columnDefs = list(list(width = "70%", targets = 0),
                                               list(width = "30%", targets = 1))))
})

# top species threats table
output$top_threat_species <- renderDT({
  dat <- species_threats_app %>%
    left_join(species_master_app %>%
        select(record_name, range_text, taxonomyId), by = "record_name")
  if(input$threat_range != "All"){
    dat <- dat %>%
      filter(grepl(input$threat_range,
                   range_text,fixed = TRUE))}
  if(input$threat_taxa != "All"){
    dat <- dat %>%
      filter(taxonomyId == input$threat_taxa)}
  dat %>%
    distinct(record_name, threat_category) %>%
    count(record_name, sort = TRUE,
          name = "Threat Categories") %>%
    slice_head(n = 10) %>%
    rename(`Designatable Units` = record_name) %>%
    datatable(rownames = FALSE, options = list( pageLength = 10,
                                                dom = "t",
                                                ordering = FALSE))
})

##############################################################################
# EXPLORE THREATS OUTPUTS
##############################################################################
output$explore_species <- renderDT({
  dat <- explore_results() %>%
    rename(`Common Name` = common_name,
           `Designatable Unit` = record_name,
           `SARA Status` = sara_status) %>%
  mutate(
    `SARA Status` = case_when(
      `SARA Status` == "Endangered" ~
        '<span class="status-endangered">Endangered</span>',
      
      `SARA Status` == "Threatened" ~
        '<span class="status-threatened">Threatened</span>',
      
      `SARA Status` == "Special Concern" ~
        '<span class="status-special">Special Concern</span>',
      
      `SARA Status` == "Extirpated" ~
        '<span class="status-extirpated">Extirpated</span>',
      
      TRUE ~ `SARA Status`))
  datatable(dat, rownames = FALSE, escape = FALSE,
            selection = list(mode = "single",target = "row"),
            options = list(pageLength = 10,
                           autowidth = TRUE,
                           columnDefs = list(
                             list(width = "35%", targets=0),
                             list(width = "40%", targets=1),
                             list(width = "15%", targets=2)
                           )))}, server = FALSE)

output$explore_species_count <- renderUI({
  matching <- nrow(explore_results())
  total_records <- species_master_app
  if(input$explore_range != "All"){
    total_records <- total_records %>%
      filter(grepl(input$explore_range, range_text, fixed = TRUE))
  }
  if(input$explore_taxa != "All"){
    total_records <- total_records %>%
      filter(taxonomyId == input$explore_taxa)
  }
  if(input$explore_status != "All"){
    total_records <- total_records %>%
      filter(sara_status == input$explore_status)
  }
  total_records <- n_distinct(total_records$record_name)
  prevalence <- round(100 * matching / total_records, 1)
  tagList(h3(input$explore_threat, style = "margin-bottom:5px;"),
          tags$p(style = "color:#6c757d; font-size:16px; margin-bottom:15px;",
                 paste0(matching, " of ", total_records, " species records (",
                        prevalence,"%)")))
})


##############################################################################
# PROVINCE SUMMARY OUTPUTS
##############################################################################
output$province_title <- renderText({
  if(input$province_range == "All"){
    "National Summary"
  } else {
    paste(input$province_range, "Summary")
  }
})

output$province_overview <- renderUI({
  dat <- province_species()
  n_records <- n_distinct(dat$record_name)
  n_endangered <- dat %>%
    filter(sara_status == "Endangered") %>%
    nrow()
  
  n_threatened <- dat %>%
    filter(sara_status == "Threatened") %>%
    nrow()
  
  n_extirpated <- dat %>%
    filter(sara_status == "Extirpated") %>%
    nrow()
  
  tags$ul(
    style = "font-size:16px;",
    tags$li(paste("DU records:", n_records)),
    tags$li(paste("Endangered:", n_endangered)),
    tags$li(paste("Threatened:", n_threatened)),
    tags$li(paste("Extirpated:", n_extirpated)))
})

output$province_status <- renderDT({
  province_species() %>%
    count(sara_status, name = "DU Records") %>%
    rename(`SARA Status` = sara_status) %>%
    datatable(rownames = FALSE, 
              options = list(dom = "t",
                             columnDefs = list(
                               list(width = "50%", targets = 0),
                               list(width = "50%", targets = 1)),
                             ordering = FALSE))
})

output$province_top_threats <- renderDT({
  dat <- species_threats_app %>%
    left_join(species_master_app %>%
                select(record_name, range_text, taxonomyId), by = "record_name")
  
  if(input$province_range != "All"){
    dat <- dat %>%
      filter(grepl(input$province_range, range_text,fixed = TRUE))}
  
  if(input$province_taxa != "All"){
    dat <- dat %>%
      filter(taxonomyId == input$province_taxa)}
  
  dat %>%
    distinct(record_name, threat_category) %>%
    count(threat_category, sort = TRUE, name = "DU Records") %>%
    rename(`Threat Category` = threat_category) %>%
    datatable(rownames = FALSE, options = list(dom = "t",
                                               columnDefs = list(
                                                 list(width = "70%", targets = 0),
                                                 list(width = "30%", targets = 1))))
})

output$province_species_table <- renderDT({
  province_species() %>%
    select(common_name, record_name, sara_status) %>%
    rename(`Common Name` = common_name,
           `Designatable Unit` = record_name,
           `SARA Status` = sara_status) %>%
    arrange(`Common Name`) %>%
    datatable(rownames = FALSE,options = list( pageLength = 10))
})

output$province_cards <- renderUI({
  dat <- province_species()
  n_endangered <- dat %>%
    filter(sara_status == "Endangered") %>%
    nrow()
  n_threatened <- dat %>%
    filter(sara_status == "Threatened") %>%
    nrow()
  n_extirpated <- dat %>%
    filter(sara_status == "Extirpated") %>%
    nrow()
  n_special_concern <- dat %>%
    filter(sara_status == "Special Concern") %>%
    nrow()
  fluidRow(column(3,summary_card(n_endangered, "Endangered","#f8d7da", "#f5c2c7")),
           column(3,summary_card(n_threatened, "Threatened","#fff3cd", "#ffecb5")),
           column(3, summary_card(n_special_concern,"Special Concern", "#d1ecf1","#bee5eb")),
           column(3,summary_card(n_extirpated, "Extirpated","#e2d9f3", "#d1c4e9")))
})

output$province_recovery_summary <- renderUI({
  dat <- province_recovery_summary()
  up_to_date <- sum(dat$n[dat$compliance_status == "Up to Date"], na.rm = TRUE)
  not_yet_due <- sum(dat$n[dat$compliance_status == "Not Yet Due"], na.rm = TRUE)
  overdue <- sum(dat$n[dat$compliance_status == "Overdue"], na.rm = TRUE)
  not_required <- sum(dat$n[dat$compliance_status %in%
                              c("Not Required", "Not Applicable")], na.rm = TRUE)
  div(class = "threat-card",
      fluidRow(column(3,div(class = "recovery-card",
                          style = "background:#d4edda;",
                          h2(up_to_date),
                          p("Up to Date"))),
             column(3,div(class = "recovery-card",
                          style = "background:#fff3cd;",
                          h2(not_yet_due),
                          p("Not Yet Required"))),
             column(3,div(class = "recovery-card",
                          style = "background:#f8d7da;",
                          h2(overdue),
                          p("Overdue"))),
             column(3, div(class = "recovery-card",
                           style = "background:#e9ecef;",
                           h2(not_required),
                           p("Not Required")))))})


}

# run app
shinyApp(ui = ui, server = server)
