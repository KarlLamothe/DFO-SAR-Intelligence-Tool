################################################################################
# Canadian Species at Risk Explorer
#
# Purpose:
# Interactive Shiny application for exploring Schedule 1 species listed under
# the Species at Risk Act (SARA). The application integrates:
#   • Species profile information
#   • Recovery planning documents
#   • Registry summaries
#
# Primary data source:
# Species at Risk Public Registry
# Additional publication sources:
#   • Canadian Science Advisory Secretariat (CSAS)
#   • Fisheries and Oceans Canada Science Publications
#   • Canadian Wildlife Service Publication Series
#
# The application integrates registry information with
# species-linked scientific publications to support
# species-at-risk research, recovery planning, and
# status assessment activities.
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
# species_documents:
#   Recovery planning and registry documents associated with each DU.
################################################################################

species_data <- readRDS("data/species_master.rds")
species_master <- species_data$data
scrape_date <- species_data$scrape_date
species_profiles <- readRDS("data/species_profiles.rds")
species_documents <- readRDS("data/species_documents.rds")
csas_publications <- readRDS("data/csas_publications.rds")
csas_species_links <- readRDS("data/csas_species_links.rds")
other_publications <- readRDS("data/all_science_publications.rds")
other_species_links <- readRDS("data/all_species_links.rds")

################################################################################
# APPLICATION DATASETS
################################################################################

target_status <- c("Endangered", "Threatened", "Extirpated", "Special Concern")

species_master_app <- species_master %>%
  filter(sara_status %in% target_status | cosewic_status %in% target_status) %>%
  mutate(sara_status = case_when(sara_status %in% c("No Data", NA_character_) ~ "No Status",
                                 TRUE ~ sara_status),
         listing_status = case_when(sara_status %in% target_status ~ "SARA Listed",
                                    currentGicDecisionId == "Pending" ~ "Under Consideration",
                                    currentGicDecisionId == "Not to list" ~ "Not Listed",
                                    currentGicDecisionId == "Refer back" ~ "Referred Back",
                                    cosewic_status %in% target_status ~ "COSEWIC Assessed",
                                    TRUE ~ "Other"))

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
# CSAS and SCIENCE SPECIES LOOKUP
################################################################################

csas_species_links <- csas_species_links %>%
  left_join(species_master %>%
              select(species_id = id,
                     common_name,
                     scientific_name),
            by = "species_id")

################################################################################
# RANGE, SARA STATUS, PANEL STYLE FILTER OPTIONS
################################################################################

range_choices <- c("All", sort(unique(
  unlist(strsplit(paste(species_master_app$range_text,
                        collapse = "; "),"; ")))))

taxa_choices <- c("Molluscs", unique(species_master_app$taxonomy_display))
taxa_choices <- c("All", sort(unique(taxa_choices)))

listing_status_levels <- c("SARA Listed", "Under Consideration", "COSEWIC Assessed", 
                           "Referred Back", "Not Listed")

sara_status_levels <- c("Extirpated", "Endangered", "Threatened",
                        "Special Concern", "No Status")

sara_choices <- c( "All", sara_status_levels)

listing_choices <- c("All", 
                     listing_status_levels[listing_status_levels %in%
                                             unique(species_master_app$listing_status)])

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
  selectInput("taxa", "Taxonomic Group", choices = taxa_choices, 
              selected = "All"),
  selectInput("listing_filter", "Listing Status", 
              choices = listing_choices, selected = "All"),
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
                  choices = taxa_choices,
                  selected = "All"),
      selectInput("listing_filter_docs", "Listing Status",
                  choices = listing_choices, selected = "All"),
    selectInput("status_docs", "SARA Status", choices = sara_choices),
    selectInput("range_docs","Range", choices = range_choices),
    selectInput("species_docs","Species", choices = sort(unique(species_master_app$common_name)) ),
    selectInput("record_docs", "Population / Designatable Unit",choices = NULL)))

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
                  choices = taxa_choices,
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
#   4. Province Summary
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
                           "Explore species profiles, recovery documents, CSAS publications, and scientific publications for SARA-listed species and COSEWIC-assessed species under consideration for listing.")),
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
                       tags$li("Use Recovery Documents to review recovery strategies, action plans, management plans, and progress reports."),
                       tags$li("Use CSAS Publications to review documents developed during Canadian Science Advisory Secretariat (CSAS) processes."),
                       tags$li("Use Scientific Publications to identify DFO Science and Canadian Wildlife Service reports related to species at risk."),
                       tags$li("Use Jurisdiction Summary to summarize species and recovery planning information by jurisdiction.")),
                     h4("Important Notes"),
                     tags$ul(tags$li("Data are refreshed periodically from the source API. Update frequency is not fixed and will depend on the availability of new information and maintenance capacity.")),
                     fluidRow(column(6,h4("Data Sources"),
                         tags$ul(
                           tags$li("Species at Risk Public Registry"),
                           tags$li("Species profile pages"),
                           tags$li("Recovery and planning documents"),
                           tags$li("CSAS publications"),
                           tags$li("DFO Science technical, manuscript, and data reports"),
                           tags$li("Canadian Wildlife Service technical reports, bulletins, and occasional papers"))),
                       column(6,h4("Application Coverage"),
                         tags$ul(
                           tags$li(
                             paste(n_distinct(species_master_app$record_name),
                               "species records")),
                           tags$li(paste(n_distinct(species_documents$document_id),
                               "Registry documents"))))))),
                  
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
                                    textOutput("profile_threats")))),
                  
                  # RECOVERY DOCUMENTS TAB
                  tabPanel(title = tagList(icon("file-lines"),"Recovery Documents"),
                           value = "species_docs",
                           br(),
                           fluidRow(species_documents_selector_ui, 
                                    column(10,
                                           uiOutput("documents_species_banner"),
                                           div(class = "threat-card",
                                           h3("Legislated Recovery Documents", style = "margin-top:0;"),
                                           div(style = "margin-bottom:15px;",
                                               downloadButton("download_document_zip",
                                                              icon("download"),
                                                              "Download All Recovery PDFs",
                                                              class = "btn-success")),
                                           br(),
                                           DTOutput("documents"),
                                           tags$p(style = "font-size:12px; color:gray;",
                                             tags$b("Document Notes: "),
                                             "Track Recovery Strategies, Action Plans, Management Plans, and Progress Reporting obligations for ", tags$i("Species at Risk Act"), " listed species. Efforts were made to remove 'Proposed' documents."
                                           ))))),
                  
                  # CSAS DOCUMENTS TAB
                  tabPanel(
                    title = tagList(icon("book"), "CSAS Publications"),
                    value = "csas_docs",
                    br(),
                    fluidRow(column(
                      width = 2,
                      div(class = "threat-card",
                          h4(icon("filter"), " Filters", class = "card-title"),
                          tags$p(style = "color:#6c757d; font-size:13px;",
                            "Filter species records by status, range, species, and DU."),
                          selectInput("taxa_csas", "Taxonomic Group",
                                      choices = taxa_choices,selected = "All"),
                          selectInput("listing_filter_csas", "Listing Status",
                                      choices = listing_choices, selected = "All"),
                          selectInput("status_csas", "SARA Status", choices = sara_choices),
                          selectInput("range_csas", "Range", choices = range_choices),
                          selectInput("species_csas", "Species",
                            choices = sort(unique(species_master_app$common_name))),
                          selectInput("record_csas", "Population / Designatable Unit",
                                      choices = NULL))),
                      column(
                        width = 10,
                        uiOutput("csas_species_banner"),
                        div(class = "threat-card",
                            h3("Canadian Science Advisory Secretariat (CSAS) Publications"),
                            tags$p(class = "text-muted",
                                   "Publications are currently linked at the species level and may include information relevant to multiple populations or designatable units."),
                            tags$p(style = "color:#6c757d;", textOutput("csas_publication_count")),
                            div(style = "margin-bottom:15px;",
                                downloadButton("download_csas_zip",icon("download"),
                                               "Download All CSAS Documents",class = "btn-success")), 
                            br(),
                            DTOutput("csas_documents"))))),
                  
                  # SCIENCE DOCUMENTS TAB
                  tabPanel(
                    title = tagList(icon("book"), "Scientific Publications"),
                    value = "science_docs",
                    br(),
                    fluidRow(column(
                      width = 2,
                      div(class = "threat-card",
                          h4(icon("filter"), " Filters", class = "card-title"),
                          tags$p(style = "color:#6c757d; font-size:13px;",
                                 "Filter species records by status, range, species, and DU."),
                          selectInput("taxa_science", "Taxonomic Group",
                                      choices = taxa_choices,selected = "All"),
                          selectInput("listing_filter_science", "Listing Status",
                                      choices = listing_choices, selected = "All"),
                          selectInput("status_science", "SARA Status", choices = sara_choices),
                          selectInput("range_science", "Range", choices = range_choices),
                          selectInput("species_science", "Species",
                                      choices = sort(unique(species_master_app$common_name))),
                          selectInput("record_science", "Population / Designatable Unit",
                                      choices = NULL))),
                      column(
                        width = 10,
                        uiOutput("science_species_banner"),
                        div(class = "threat-card",
                            h3("Scientific Publications"),
                            tags$p(class = "text-muted",
                                   "Publications include DFO Science reports, Canadian Wildlife Service technical reports, Wildlife Management Bulletins, Occasional Papers, and other government science publication series."),
                            tags$p(style = "color:#6c757d;", textOutput("science_publication_count")),
                            div(style = "margin-bottom:15px;",
                                downloadButton("download_science_zip",icon("download"),class = "btn-success",
                                               "Download All Science Publications")), 
                            br(),
                            DTOutput("science_documents"))))),
                  
                  # JURISDICTION SUMMARY TAB
                  tabPanel(
                    title = tagList(icon("map"),"Jurisdiction Summary"),
                    value = "prov_sum",
                    div(style = "max-width:1200px; margin-left:10px; padding-left:10px;",
                        br(),
                        fluidRow(province_selector_ui,
                        column(10,
                          h3(textOutput("province_title")),
                          fluidRow(uiOutput("province_cards")),
                          br(),
                          div(class = "threat-card",
                              h3("Species Records"),
                              DTOutput("province_species_table")))))))),
  # FOOTER INFORMATION
  hr(),
  tags$div(style = "font-size:12px;",
    class = "footer-card",
    "Canadian Species at Risk Explorer v3.4",
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
  # These reactives return the currently selected species record from each major
  # tab within the application.
  #
  # Species Information tab:
  #   input$record
  #
  # Species Documents tab:
  #   input$record_docs
  #
  # CSAS Publications tab:
  #   input$record_csas
  #
  # Each reactive returns a single-row record from species_master_app containing
  # the species metadata associated with the current Designatable Unit (DU).
  ##############################################################################
  # Currently selected species record from the Species Information tab
  selected_species <- reactive({
    species_master_app %>%
      filter(record_name == input$record)%>%
      slice(1)
  })
  
  # Currently selected species record from the Species Documents tab
  selected_species_docs <- reactive({
    species_master_app %>%
      filter(record_name == input$record_docs) %>%
      slice(1)
  })
  
  # Currently selected species record from the CSAS Publications tab
  selected_species_csas <- reactive({
    species_master_app %>%
      filter(record_name == input$record_csas) %>%
      slice(1)
  })
  
  # Currently selected species record from the Science Publications tab
  selected_species_science <- reactive({
    species_master_app %>%
      filter(record_name == input$record_science) %>%
      slice(1)
  })
  
  ##############################################################################
  # CSAS DOCUMENTS REACTIVES
  ##############################################################################
  # Retrieve CSAS publications associated with the currently selected species.
  #
  # Important:
  # CSAS publications are linked at the species level rather than the
  # Designatable Unit (DU) level.
  #
  # Workflow:
  #   1. Determine the scientific name associated with the selected DU.
  #   2. Find matching CSAS publication links.
  #   3. Join publication metadata and PDF URLs.
  #   4. Remove duplicate publication IDs.
  #   5. Return publications ordered from newest to oldest.
  ##############################################################################
  species_csas_docs <- reactive({
    req(input$record_csas)
    sp <- selected_species_csas()
    req(nrow(sp) > 0)
    sci_name <- sp$scientific_name[1]
    csas_species_links %>%
      filter(scientific_name == sci_name) %>%
      left_join(csas_publications %>% 
                  select(publication_id, pdf_url), by = "publication_id") %>%
      distinct(publication_id, .keep_all = TRUE) %>%
      arrange(desc(publication_year), publication_number)
  })
  
  ##############################################################################
  # SCIENCE DOCUMENTS REACTIVES
  ##############################################################################
  # Retrieve Science publications associated with the currently selected species.
  #
  # Workflow:
  #   1. Determine the scientific name associated with the selected DU.
  #   2. Find matching Science publication links.
  #   3. Join publication metadata and PDF URLs.
  #   4. Remove duplicate publication IDs.
  #   5. Return publications ordered from newest to oldest.
  ##############################################################################
  species_science_docs <- reactive({
    req(input$record_science)
    sp <- selected_species_science()
    req(nrow(sp) > 0)
    sci_name <- sp$scientific_name[1]
    other_species_links %>%
      filter(scientific_name == sci_name) %>%
      left_join(other_publications, by = "publication_id") %>%
      distinct(publication_id, .keep_all = TRUE)
  })

  ##############################################################################
  # SPECIES PROFILE REACTIVE
  ##############################################################################
  # Retrieves profile text for the currently selected Designatable Unit (DU).
  #
  # Some DUs lack profile text despite the species profile existing.
  # In these cases:
  #   1. Attempt DU-level matching first.
  #   2. If no profile text exists, fall back to another profile record from the
  #      same common name that contains descriptive information.
  #
  # This helps avoid empty Species Information pages when Registry profile text
  # is incomplete at the DU level.
  ##############################################################################
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
  # JURISDICTIONAL SUMMARY REACTIVES
  ##############################################################################
  # Supporting datasets used by the Province Summary tab.
  #
  # province_species():
  #   Species records matching the current provincial / taxonomic filters.
  #
  ##############################################################################
  province_species <- reactive({
    dat <- species_master_app
    if(input$province_range != "All"){
      dat <- dat %>%
        filter(grepl(input$province_range, range_text, fixed = TRUE))
    }
    if(input$province_taxa == "Molluscs"){
      dat <- dat %>%
        filter(grepl("^Molluscs", taxonomy_display))
    } else if(input$province_taxa != "All"){
      dat <- dat %>%
        filter(taxonomy_display == input$province_taxa)
    }
    dat
  })
  
  ##############################################################################
  # CASCADING FILTER OBSERVERS
  ##############################################################################
  # Filter controls are dependent on one another and must be updated dynamically
  # as the user narrows the available species records.
  #
  # Across the Species Information, Species Documents, and CSAS tabs:
  #
  #   Taxonomic Group
  #        ↓
  #   Listing Status
  #        ↓
  #   SARA Status
  #        ↓
  #   Range
  #        ↓
  #   Species
  #        ↓
  #   Designatable Unit (DU)
  #
  # These observers ensure that available selections remain valid as upstream
  # filters change.
  ##############################################################################
  ##############################################################################
  # SPECIES INFORMATION FILTERS
  ##############################################################################
  # Dynamic filters used by the Species Information tab.
  #
  # These observers update:
  #   • Available ranges
  #   • Available SARA statuses
  #   • Available species selections
  #
  # based on the current taxonomic, listing status, and range filters.
  ##############################################################################
  # dynamic range selector
  observe({
    req(input$taxa)
    dat <- species_master_app
    if(input$listing_filter != "All"){
      dat <- dat %>%
        filter(listing_status == input$listing_filter)
    }
    if(input$taxa == "Molluscs"){
      dat <- dat %>%
        filter(grepl("^Molluscs", taxonomy_display))
    } else if(input$taxa != "All"){
      dat <- dat %>%
        filter(taxonomy_display == input$taxa)
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
    if(input$listing_filter != "All"){
      dat <- dat %>%
        filter(listing_status == input$listing_filter)
    }
    if(input$taxa == "Molluscs"){
      dat <- dat %>%
        filter(grepl("^Molluscs", taxonomy_display))
    } else if(input$taxa != "All"){
      dat <- dat %>%
        filter(taxonomy_display == input$taxa)
    }
    updateSelectInput(session, "status",
                      choices = c("All", sara_status_levels[
                          sara_status_levels %in% unique(dat$sara_status)]))
  })
  
  # species selector
  observe({
    species_choices <- species_master_app
    if(input$listing_filter != "All"){
      species_choices <- species_choices %>%
        filter(listing_status == input$listing_filter)
    }
    if(input$status != "All"){
      species_choices <- species_choices %>%
        filter(sara_status == input$status)
    }
    if(input$range != "All"){
      species_choices <- species_choices %>%
        filter(grepl(input$range, range_text, fixed = TRUE))
    }
    if(input$taxa == "Molluscs"){
      species_choices <- species_choices %>%
        filter(grepl("^Molluscs", taxonomy_display))
    } else if(input$taxa != "All"){
      species_choices <- species_choices %>%
        filter(taxonomy_display == input$taxa)
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

  ##############################################################################
  # SPECIES DOCUMENT FILTERS
  ##############################################################################
  # Dynamic filters used by the Species Documents tab.
  #
  # Behaviour mirrors the Species Information tab but uses a separate set of
  # inputs so users can browse documents independently while preserving tab
  # synchronization behaviour.
  ##############################################################################
  # dynamic document range selector
  observe({
    req(input$taxa_docs)
    dat <- species_master_app
    if(input$listing_filter_docs != "All"){
      dat <- dat %>%
        filter(listing_status == input$listing_filter_docs)
    }
    if(input$taxa_docs == "Molluscs"){
      dat <- dat %>%
        filter(grepl("^Molluscs", taxonomy_display))
    } else if(input$taxa_docs != "All"){
      dat <- dat %>%
        filter(taxonomy_display == input$taxa_docs)
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
    if(input$listing_filter_docs != "All"){
      dat <- dat %>%
        filter(listing_status == input$listing_filter_docs)
    }
    
    if(input$taxa_docs == "Molluscs"){
      dat <- dat %>%
        filter(grepl("^Molluscs", taxonomy_display))
    } else if(input$taxa_docs != "All"){
      dat <- dat %>%
        filter(taxonomy_display == input$taxa_docs)
    }
    statuses <- c("All", sara_status_levels[
        sara_status_levels %in% unique(dat$sara_status)])
    updateSelectInput(session, "status_docs",
                      choices = statuses,
                      selected = if(
        !is.null(input$status_docs) &&
        input$status_docs %in% statuses) input$status_docs else "All")
  })
  
  # document species selector 
  observe({
    species_choices <- species_master_app
    if(input$listing_filter_docs != "All"){
      species_choices <- species_choices %>%
        filter(listing_status == input$listing_filter_docs)
    }
    if(input$status_docs != "All"){
      species_choices <- species_choices %>%
        filter(sara_status == input$status_docs)
    }
    if(input$range_docs != "All"){
      species_choices <- species_choices %>%
        filter(grepl(input$range_docs,range_text, fixed = TRUE))
    }
    if(input$taxa_docs == "Molluscs"){
      species_choices <- species_choices %>%
        filter(grepl("^Molluscs", taxonomy_display))
    } else if(input$taxa_docs != "All"){
      species_choices <- species_choices %>%
        filter(taxonomy_display == input$taxa_docs)
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
  # CSAS PUBLICATION FILTERS
  ##############################################################################
  # Dynamic filters used by the CSAS Publications tab.
  #
  # Publications are associated with species-level scientific names, but the
  # application preserves Designatable Unit (DU) selection for consistency with
  # other tabs.
  #
  # These observers update:
  #   • Species choices
  #   • DU choices
  #
  # while preserving valid selections whenever possible.
  ##############################################################################
  
  # CSAS species selector
  observe({
    species_choices <- species_master_app
    if(input$listing_filter_csas != "All"){
      species_choices <- species_choices %>%
        filter(listing_status == input$listing_filter_csas)
    }
    if(input$status_csas != "All"){
      species_choices <- species_choices %>%
        filter(sara_status == input$status_csas)
    }
    if(input$range_csas != "All"){
      species_choices <- species_choices %>%
        filter(grepl(input$range_csas, range_text, fixed = TRUE))
    }
    if(input$taxa_csas == "Molluscs"){
      species_choices <- species_choices %>%
        filter(grepl("^Molluscs", taxonomy_display))
    } else if(input$taxa_csas != "All"){
      species_choices <- species_choices %>%
        filter(taxonomy_display == input$taxa_csas)
    }
    choices <- sort(unique(species_choices$common_name))
    selected_species <- if(!is.null(input$species_csas) && input$species_csas %in% choices){
      input$species_csas
    } else if(length(choices) > 0){
      choices[1]
    } else {
      character(0)
    }
    updateSelectInput(session,  "species_csas", choices = choices, selected = selected_species)
  })
  
  # CSAS DU selector
  observeEvent(c(input$species_csas, input$status_csas,
                 input$range_csas, input$taxa_csas, input$listing_filter_csas),
    {
      req(input$species_csas)
      records <- species_master_app %>%
        filter(common_name == input$species_csas)
      if(input$listing_filter_csas != "All"){
        records <- records %>%
          filter(listing_status == input$listing_filter_csas)
      }
      if(input$status_csas != "All"){
        records <- records %>%
          filter(sara_status == input$status_csas)
      }
      if(input$range_csas != "All"){
        records <- records %>%
          filter(grepl(input$range_csas, range_text,  fixed = TRUE))
      }
      if(input$taxa_csas == "Molluscs"){
        records <- records %>% filter(grepl("^Molluscs", taxonomy_display))
      } else if(input$taxa_csas != "All"){
        records <- records %>% filter(taxonomy_display == input$taxa_csas)
      }
      records <- records %>%
        arrange(record_name) %>%
        pull(record_name)

      selected_record <- if(
        !is.null(input$record_csas) &&
        input$record_csas %in% records
      ){
        input$record_csas
      } else if(length(records) > 0){
        records[1]
      } else {
        character(0)
      }
      
      updateSelectInput(
        session,
        "record_csas",
        choices = records,
        selected = selected_record
      )
    })
  
  ##############################################################################
  # Science PUBLICATION FILTERS
  ##############################################################################
  # Dynamic filters used by the Science Publications tab.
  # These observers update:
  #   • Species choices
  #   • DU choices
  ##############################################################################
  # Science species selector
  observe({
    species_choices <- species_master_app
    if(input$listing_filter_science != "All"){
      species_choices <- species_choices %>%
        filter(listing_status == input$listing_filter_science)
    }
    if(input$status_science != "All"){
      species_choices <- species_choices %>%
        filter(sara_status == input$status_science)
    }
    if(input$range_science != "All"){
      species_choices <- species_choices %>%
        filter(grepl(input$range_science, range_text, fixed = TRUE))
    }
    if(input$taxa_science == "Molluscs"){
      species_choices <- species_choices %>%
        filter(grepl("^Molluscs", taxonomy_display))
    } else if(input$taxa_science != "All"){
      species_choices <- species_choices %>%
        filter(taxonomy_display == input$taxa_science)
    }
    choices <- sort(unique(species_choices$common_name))
    selected_species <- if(!is.null(input$species_science) && input$species_science %in% choices){
      input$species_science
    } else if(length(choices) > 0){
      choices[1]
    } else {
      character(0)
    }
    updateSelectInput(session,  "species_science", choices = choices, selected = selected_species)
  })
  
  # science DU selector
  observeEvent(c(input$species_science, input$status_science,
                 input$range_science, input$taxa_science, input$listing_filter_science),
               {
                 req(input$species_science)
                 records <- species_master_app %>%
                   filter(common_name == input$species_science)
                 if(input$listing_filter_science != "All"){
                   records <- records %>%
                     filter(listing_status == input$listing_filter_science)
                 }
                 if(input$status_science != "All"){
                   records <- records %>%
                     filter(sara_status == input$status_science)
                 }
                 if(input$range_science != "All"){
                   records <- records %>%
                     filter(grepl(input$range_science, range_text,  fixed = TRUE))
                 }
                 if(input$taxa_science == "Molluscs"){
                   records <- records %>% filter(grepl("^Molluscs", taxonomy_display))
                 } else if(input$taxa_science != "All"){
                   records <- records %>% filter(taxonomy_display == input$taxa_science)
                 }
                 records <- records %>%
                   arrange(record_name) %>%
                   pull(record_name)

                 selected_record <- if(
                   !is.null(input$record_science) &&
                   input$record_science %in% records
                 ){
                   input$record_science
                 } else if(length(records) > 0){
                   records[1]
                 } else {
                   character(0)
                 }
                 
                 updateSelectInput(
                   session,
                   "record_science",
                   choices = records,
                   selected = selected_record
                 )
               })
  
  ##############################################################################
  # CROSS-TAB DESIGNATABLE UNIT (DU) SYNCHRONIZATION
  ##############################################################################
  # Species Information, Species Documents, and CSAS Publications maintain
  # separate selector controls but represent different views of the same species
  # record.
  #
  # When a Designatable Unit (DU) is changed in any tab, the corresponding
  # species and DU selections are propagated to the other tabs to keep all views
  # synchronized.
  #
  # Synchronization occurs at the DU level using the selected record_name.
  ##############################################################################
  # CROSS TAB SYNCHRONIZATION HELPERS
  sync_species <- function(common_name, source_input){
    species_inputs <- c("species", "species_docs", "species_csas", "species_science")
    species_inputs <- setdiff(species_inputs, source_input)
    for(id in species_inputs){
      current <- isolate(input[[id]])
      if(!identical(current, common_name)){
      updateSelectInput(session, id, selected = common_name)}}}
  
  # species information
  observeEvent(input$species, {
    req(input$species)
    sync_species(input$species, source_input = "species")
    }, ignoreInit = TRUE)
  
  # species documents
  observeEvent(input$species_docs, {
    req(input$species_docs)
    sync_species(input$species_docs, source_input = "species_docs")
    }, ignoreInit = TRUE)
  
  # CSAS pubs
  observeEvent(input$species_csas, {
    req(input$species_csas)
    sync_species(input$species_csas, source_input = "species_csas")
    }, ignoreInit = TRUE)
  
  # science pubs
  observeEvent(input$species_science, {
    req(input$species_science)
    sync_species(input$species_science, source_input = "species_science")
    }, ignoreInit = TRUE)
  
  # DUs
  observeEvent(input$record, {
    sp <- species_master_app %>%
      filter(record_name == input$record) %>%
      slice(1)
    req(nrow(sp) > 0)
    #shared_du(input$record)
    sync_species(sp$common_name[1],
                 source_input = "species")
  }, ignoreInit = TRUE)
  
  # species docs
  observeEvent(input$record_docs, {
    sp <- species_master_app %>%
      filter(record_name == input$record_docs) %>%
      slice(1)
    req(nrow(sp) > 0)
    updateSelectInput(session, "record", selected = input$record_docs)
    sync_species(sp$common_name[1], source_input = "species_docs")
    }, ignoreInit = TRUE)
  
  # CSAS pubs
  observeEvent(input$record_csas, {
    sp <- species_master_app %>%
      filter(record_name == input$record_csas) %>%
      slice(1)
    req(nrow(sp) > 0)
    sync_species(sp$common_name[1], source_input = "species_csas")
    }, ignoreInit = TRUE)
  
  # science pubs
  observeEvent(input$record_science, {
    sp <- species_master_app %>%
      filter(record_name == input$record_science) %>%
      slice(1)
    req(nrow(sp) > 0)
    sync_species(sp$common_name[1], source_input = "species_science")
    }, ignoreInit = TRUE)
  
  ##############################################################################
  # SPECIES INFORMATION DU SELECTOR
  ##############################################################################
  # Updates available Designatable Units (DUs) for the currently selected
  # species after all Species Information tab filters have been applied.
  #
  # Existing selections are preserved whenever they remain valid.
  ##############################################################################
  observe({
    req(input$species)
    
    records <- species_master_app %>%
      filter(common_name == input$species)
    
    if(input$listing_filter != "All"){
      records <- records %>%
        filter(listing_status == input$listing_filter)
    }
    
    if(input$status != "All"){
      records <- records %>%
        filter(sara_status == input$status)
    }
    
    if(input$range != "All"){
      records <- records %>%
        filter(grepl(input$range, range_text, fixed = TRUE))
    }
    
    if(input$taxa == "Molluscs"){
      records <- records %>%
        filter(grepl("^Molluscs", taxonomy_display))
    } else if(input$taxa != "All"){
      records <- records %>%
        filter(taxonomy_display == input$taxa)
    }
    
    records <- records %>%
      arrange(record_name) %>%
      pull(record_name)
    
    selected_record <- if(
      !is.null(input$record) &&
      input$record %in% records
    ){
      input$record
    } else if(length(records) > 0){
      records[1]
    } else {
      character(0)
    }
    
    updateSelectInput(session, "record", choices = records, selected = selected_record)
  })

  ##############################################################################
  # SPECIES DOCUMENTS DU SELECTOR
  ##############################################################################
  # Updates available Designatable Units (DUs) for the currently selected
  # species within the Species Documents tab.
  #
  # Existing selections are preserved whenever they remain valid.
  ##############################################################################
  observeEvent(
    c(input$species_docs, input$status_docs, input$range_docs, input$taxa_docs,
      input$listing_filter_docs),
    {records <- species_master_app %>%
        filter(common_name == input$species_docs)
    if(input$listing_filter_docs != "All"){
      records <- records %>%
        filter(listing_status == input$listing_filter_docs)
    }
      if(input$status_docs != "All"){
        records <- records %>%
          filter(sara_status == input$status_docs)
      }
      if(input$range_docs != "All"){
        records <- records %>%
          filter(grepl(input$range_docs,range_text,fixed = TRUE))
      }
    if(input$taxa_docs == "Molluscs"){
      records <- records %>%
        filter(grepl("^Molluscs", taxonomy_display))
    } else if(input$taxa_docs != "All"){
      records <- records %>%
        filter(taxonomy_display == input$taxa_docs)
    }
      records <- records %>%
        arrange(record_name) %>%
        pull(record_name)

      selected_record <- if(
        !is.null(input$record_docs) &&
        input$record_docs %in% records
      ){
        input$record_docs
      } else if(length(records) > 0){
        records[1]
      } else {
        character(0)
      }
      
      updateSelectInput(
        session,
        "record_docs",
        choices = records,
        selected = selected_record
      )})
  
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
      tags$div(tags$b("SARA Schedule 1 Status: "), sp$sara_status, style = "margin-top:4px;"),
      tags$div(tags$b("Listing Status: "), sp$listing_status, style = "margin-top:4px;"),
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

#threats
output$profile_threats <- renderText({
  selected_profile() %>%
    pull(threats)
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

output$documents_species_banner <- renderUI({
  sp <- selected_species_docs()
  req(nrow(sp) > 0)
  div(style = "
      background-color:#f8f9fa;
      border:1px solid #dee2e6;
      font-size:24px;
      border-radius:10px;
      padding:15px;
      margin-bottom:15px;",
    tags$b("Viewing Documents for: "),
    tags$span(style = "color:#2c5aa0; font-weight:600;",
              strong(sp$record_name[1])))
})

##############################################################################
# CSAS PUBLICATIONS OUTPUTS
##############################################################################
output$csas_documents <- renderDT({
  req(species_csas_docs())
  species_csas_docs() %>%
    mutate(Title = ifelse(!is.na(pdf_url) & pdf_url != "",
                          paste0('<a href="', pdf_url, '" target="_blank">',
                                 title, '</a>'), title )) %>%
    transmute(Year = publication_year, Type = publication_type,
              Number = publication_number, Title )
  }, escape = FALSE, rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE))

output$csas_species_banner <- renderUI({
  sp <- selected_species_csas()
  req(nrow(sp) > 0)
  div(style = "
      background-color:#f8f9fa;
      border:1px solid #dee2e6;
      font-size:24px;
      border-radius:10px;
      padding:15px;
      margin-bottom:15px;",
    tags$b("Viewing Publications for: "),
    tags$span(style = "color:#2c5aa0; font-weight:600;", strong(sp$common_name[1])))
})

output$csas_publication_count <- renderText({
  req(species_csas_docs())
  paste(nrow(species_csas_docs()), "publications found")
})

##############################################################################
# SCIENCE PUBLICATIONS OUTPUTS
##############################################################################

output$science_documents <- renderDT({
  req(species_science_docs())
  species_science_docs() %>%
    mutate(Title = ifelse(!is.na(publication_url) &
                            publication_url != "",
                          paste0('<a href="', publication_url,
                                 '" target="_blank">', title, '</a>'), title)) %>%
    transmute(Source = publication_source, Series = publication_series,
              Year = publication_year, Title)
}, escape = FALSE,rownames = FALSE,
options = list(pageLength = 15, scrollX = TRUE, autoWidth = TRUE,
               columnDefs = list(
                 list(width = "120px", targets = 0),  # Source
                 list(width = "180px", targets = 1),  # Series
                 list(width = "70px", targets = 2)   # Year
               )))

output$science_species_banner <- renderUI({
  sp <- selected_species_science()
  req(nrow(sp) > 0)
  div(style = "
      background-color:#f8f9fa;
      border:1px solid #dee2e6;
      font-size:24px;
      border-radius:10px;
      padding:15px;
      margin-bottom:15px;",
      tags$b("Viewing Publications for: "),
      tags$span(style = "color:#2c5aa0; font-weight:600;", strong(sp$common_name[1])))
})

output$science_publication_count <- renderText({
  req(species_science_docs())
  paste(nrow(species_science_docs()), "publications found")
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

output$province_status <- renderDT({
  province_species() %>%
    mutate(sara_status = factor(sara_status, levels = sara_status_levels)) %>%
    count(sara_status, name = "DU Records") %>%
    arrange(sara_status) %>%
    rename(`SARA Status` = sara_status) %>%
    datatable(rownames = FALSE, 
              options = list(dom = "t",
                             columnDefs = list(
                               list(width = "50%", targets = 0),
                               list(width = "50%", targets = 1)),
                             ordering = FALSE))
})

output$province_species_table <- renderDT({
  province_species() %>%
    mutate(listing_status = factor(listing_status,levels = listing_status_levels)) %>%
    select(common_name, record_name, sara_status, listing_status) %>%
    rename(`Common Name` = common_name,
           `Designatable Unit` = record_name,
           `SARA Status` = sara_status,
           `Listing Status` = listing_status) %>%
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
  fluidRow(column(3,summary_card(n_extirpated, "Extirpated","#e2d9f3", "#d1c4e9")),
           column(3,summary_card(n_endangered, "Endangered","#f8d7da", "#f5c2c7")),
           column(3,summary_card(n_threatened, "Threatened","#fff3cd", "#ffecb5")),
           column(3, summary_card(n_special_concern,"Special Concern", "#d1ecf1","#bee5eb")))
})

################################################################################
# EXPORTING
################################################################################
output$download_document_zip <- downloadHandler(
  filename = function() {
    paste0(gsub("[^A-Za-z0-9]", "_", input$record_docs),  "_documents.zip")},
  content = function(file) {
    docs <- species_documents %>%
      filter(record_name == input$record_docs)
    tmpdir <- tempfile()
    dir.create(tmpdir)
    downloaded_files <- c()
    for(i in seq_len(nrow(docs))) {
      pdf_file <- file.path(tmpdir, paste0(gsub("[^A-Za-z0-9]", "_",
                                                docs$attachment_title[i]), ".pdf"))
      try(download.file(docs$document_url[i], pdf_file, mode = "wb", quiet = TRUE),
        silent = TRUE)
      
      if(file.exists(pdf_file)) {
        downloaded_files <- c(downloaded_files,  pdf_file)
      }
    }
    zip::zipr(zipfile = file, files = downloaded_files)
  }
)

output$download_csas_zip <- downloadHandler(
  filename = function() {
    paste0(gsub("[^A-Za-z0-9]", "_", input$species_csas), "_CSAS_publications.zip")},
  content = function(file) {
    docs <- species_csas_docs() %>%
      filter(!is.na(pdf_url))
    tmpdir <- tempfile()
    dir.create(tmpdir)
    downloaded <- c()
    for(i in seq_len(nrow(docs))) {
      pdf_file <- file.path(tmpdir, paste0(docs$publication_year[i], "_",
                                           gsub("[^A-Za-z0-9]", "_",
                                                docs$publication_number[i]),".pdf"))
      try(download.file(docs$pdf_url[i], pdf_file, mode = "wb", quiet = TRUE),
          silent = TRUE)
      if(file.exists(pdf_file))
        downloaded <- c(downloaded, pdf_file)
    }
    zip::zipr(file, downloaded)
  }
)

output$download_science_zip <- downloadHandler(
  filename = function() {
    paste0(gsub("[^A-Za-z0-9]", "_", input$species_science),
           "_science_publications.zip")},
  
  content = function(file) {
    docs <- species_science_docs() %>%
      filter(!is.na(publication_url))
    tmpdir <- tempfile()
    dir.create(tmpdir)
    downloaded <- c()
    for(i in seq_len(nrow(docs))) {
      pdf_file <- file.path(tmpdir, paste0(docs$publication_year[i], "_", i, ".pdf"))
      try(download.file(docs$publication_url[i], pdf_file, mode = "wb", quiet = TRUE),
        silent = TRUE)
      if(file.exists(pdf_file))
        downloaded <- c(downloaded, pdf_file)
    }
    zip::zipr(file, downloaded)
  })

}

# run app
shinyApp(ui = ui, server = server)
