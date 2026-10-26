################################################################################
# Freshwater SAR Species Explorer
#
# Purpose:
# Interactive Shiny application for exploring Schedule 1 freshwater fishes
# listed under the Species at Risk Act (SARA). The application integrates:
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

species_master <- readRDS("data/species_master.rds")
species_profiles <- readRDS("data/species_profiles.rds")
species_threats <- readRDS("data/species_threats.rds")
species_documents <- readRDS("data/species_documents.rds")
recovery_compliance <- readRDS("data/recovery_compliance.rds")
progress_compliance <- readRDS("data/progress_report_compliance.rds")

################################################################################
# APPLICATION DATASETS
# The application focuses on Schedule 1 freshwater fishes listed as:
# Endangered, Threatened, Extirpated.
# Species of Special Concern are currently excluded.
################################################################################

species_master_app <- species_master %>%
  filter(sara_status %in% c("Threatened", "Endangered","Extirpated"))

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
  filter(sara_status %in% c("Threatened", "Endangered", "Extirpated"))

################################################################################
# RANGE, SARA STATUS, PANEL STYLE FILTER OPTIONS
################################################################################

range_choices <- c("All", sort(unique(
  unlist(strsplit(paste(species_master_app$range_text,
                        collapse = "; "),"; ")))))

sara_choices <- c("All","Endangered","Threatened","Extirpated")

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
    h1(style = "margin:0;", value),
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
  left_join(
    species_documents %>%
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
            "Recovery Strategy",
            "Action Plan",
            "Progress Report",
            "Recovery Strategy & Action Plan"
          )
          
          dates <- as.Date(documentCreateDate[planning_idx])
          
          if(length(dates) == 0 || all(is.na(dates))) {
            as.Date(NA)
          } else {
            max(dates, na.rm = TRUE)
          }
        },
        
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
  mutate(
    latest_recovery_planning_document = as.Date(
      ifelse(is.infinite(latest_recovery_planning_document), NA,
        latest_recovery_planning_document),origin = "1970-01-01"),
    
    years_since_recovery_planning =
      round( as.numeric(Sys.Date() - latest_recovery_planning_document) / 365.25,
        1))

recovery_coverage <- recovery_coverage %>%
  left_join(
    progress_compliance %>%
      select(
        record_name,
        compliance_status,
        latest_progress_report_date
      ),
    by = "record_name"
  ) %>%
  mutate(
    
    latest_document_date = pmax(
      latest_recovery_planning_document,
      latest_progress_report_date,
      na.rm = TRUE
    ),
    
    latest_document_type = case_when(
      !is.na(latest_progress_report_date) &
        latest_progress_report_date == latest_document_date ~
        "Progress Report",
      
      TRUE ~ latest_document_type
    ),
    
    years_since_recovery_planning =
      round(
        as.numeric(
          Sys.Date() - latest_document_date
        ) / 365.25,
        1
      )
  )

recovery_coverage %>%
  filter(grepl("Eastern Sand Darter", record_name)) %>%
  select(
    record_name,
    latest_recovery_planning_document,
    latest_progress_report_date,
    latest_document_date,
    latest_document_type
  )


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
  wellPanel(style = panel_style,
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
  wellPanel(style = panel_style,
    selectInput("status_docs", "SARA Status", choices = sara_choices),
    selectInput("range_docs","Range", choices = range_choices),
    selectInput("species_docs","Species", choices = sort(unique(species_master_app$common_name)) ),
    selectInput("record_docs", "Population / Designatable Unit",choices = NULL)))

################################################################################
# THREAT EXPLORATION FILTER PANEL
################################################################################
explore_selector_ui <- column(
  width = 2,
  wellPanel(style = panel_style,
            selectInput("explore_threat", "Threat Group",choices = NULL),
            selectInput("explore_status", "SARA Status",
                choices = sara_choices, selected = "All"),
    selectInput("explore_range","Range",
                choices = range_choices)))

################################################################################
# RECOVERY PLANNING FILTER PANEL
################################################################################
recovery_planning_selector_ui <- column(
  width = 2,
  wellPanel(style = panel_style,
            selectInput("planning_status", "SARA Status", choices = sara_choices,
                        selected = "All"),
            selectInput("planning_range","Range",
                        choices = range_choices)))

################################################################################
# PROVINCE SUMMARY FILTER PANEL
################################################################################
province_selector_ui <- column(
  width = 2,
  wellPanel(style = panel_style,
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
  theme = bs_theme(version = 5, bootswatch = "cosmo"),
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
    "))
  ),
  
  # Main application title displayed at the top of every page.
  titlePanel(div(style = "font-size:36px;
                 font-weight:bold;
                 color:#2c3e50;",
                 "Freshwater SAR Species Explorer")),
  
  # Main navigation tabs
      tabsetPanel(id = "main_tabs",
                  
                  # DASHBOARD
                  tabPanel(
                     "Dashboard",
                     br(),
                     h3("About This Application"),
                     p("The Freshwater SAR Species Explorer provides access to species profiles, threat information, species documents, and recovery planning information for Schedule 1 freshwater fishes listed under the Species at Risk Act (SARA). This tool was developed to make information available through the Species at Risk Public Registry easier to access, explore, and summarize"),
                     h4("Important Notes"),
                     tags$ul(tags$li("Threat categories are screening-level classifications derived from Species at Risk Public Registry profile information and should not be interpreted as formal threat assessments."),
                             tags$li("Recovery planning metrics are based on Recovery Strategies, Action Plans, and Progress Reports identified within Registry planning documents.")),
                     fluidRow(column(6,h4("Data Sources"),
                         tags$ul(
                           tags$li("Species at Risk Public Registry"),
                           tags$li("Species profile pages"),
                           tags$li("Recovery and planning documents"))),
                       column(6,h4("Coverage"),
                         tags$ul(
                           tags$li(
                             paste(n_distinct(species_master_app$record_name),
                               "species records")),
                           tags$li(paste(n_distinct(species_documents$document_id),
                               "documents")),
                           tags$li(paste(n_distinct(species_threats_app$threat_category),
                               "threat groups")))))),
                  
                  # SPECIES INFORMATION
                  tabPanel("Species Information",
                           br(),
                           fluidRow(species_selector_ui,column(10,
                                    style = "margin-top: -8px;",
                                    uiOutput("species_header"),
                                    h3("Description"),
                                    textOutput("description"),
                                    br(),
                                    h3("Population"),
                                    textOutput("population"),
                                    br(),
                                    h3("Habitat"),
                                    textOutput("habitat"),
                                    br(),
                                    h3("Biology"),
                                    textOutput("biology"),
                                    br(),
                                    h3("Threats"),
                                    textOutput("profile_threats"),
                                    br(),
                                    h3(textOutput("n_threats")),
                                    DTOutput("threats"),
                                    tags$p(style = "font-style: italic; color: #666666;",
                                           "Threat groups indicate the presence of a threat within profile text and should be considered screening-level classifications rather than formal threat assessments.")
                                    ))),
                  
                  # SPECIES DOCUMENTS TAB
                  tabPanel("Species Documents",
                           br(),
                           fluidRow(species_documents_selector_ui, 
                                    column(10, 
                                           h4("Species Recovery Documents Compliance"),
                                           uiOutput("compliance_summary"),
                                           br(),
                                           h4("Legislated Recovery Documents"),
                                           DTOutput("documents"),
                                           tags$p(
                                             style = "font-size:12px; color:gray;",
                                             tags$b("Document Notes: "),
                                             "This table includes Recovery Strategies, Action Plans, Management Plans, Progress Reports, and COSEWIC Assessments..."
                                           )))),
                 
                   # RECOVERY PLANNING TAB
                  tabPanel("Recovery Planning",
                           div(style = "max-width:1200px; margin-left:10px;",
                               br(),
                               fluidRow(recovery_planning_selector_ui, 
                                        column(10,
                                               h3("Recovery Planning Coverage"),
                                               br(),
                                               uiOutput("planning_summary"),
                                               br(),
                                               DTOutput("recovery_coverage", width = "100%"),
                                               tags$p(
                                                 style = "font-style: italic; color: #666666;",
                                                 "Recovery planning coverage is based on recovery strategies, action plans, and associated progress reports identified within Species at Risk Public Registry planning documents."
                                                 ))))),
                  # THREAT SUMMARY TAB
                  tabPanel("Threat Summary",
                    div(style = "max-width:1200px; margin-left:10px;",
                        br(),
                        fluidRow(column(2,wellPanel(style = panel_style,
                          selectInput("threat_range", "Range",
                                      choices = range_choices, selected = "All"))),
                          column(10,
                                 fluidRow(column(6,h3("DUs with the Most Threat Categories",
                                                      style = "text-align:center;"),
                                                 DTOutput("top_threat_species")),
                                          column(6,h3("Most Common Threat Categories",
                                                      style = "text-align:center;"),
                                                 DTOutput("common_threats"))),
                                 tags$p(style = "font-style: italic; color: #666666;",
                                        "Threat frequencies are based on threat categories identified from Species at Risk Public Registry species profile information."
                                        ))))),
                  # EXPLORE THREATS TAB
                  tabPanel("Explore Threats",
                           div(style = "max-width:1200px; margin-left:10px;",
                               br(),
                               fluidRow(explore_selector_ui,
                                        column(10,
                                               h3(textOutput("explore_species_count")),
                                               DTOutput("explore_species"))
                                        ))),
                  # PROVINCIAL SUMMARY TAB
                  tabPanel("Province Summary",
                           div(style = "max-width:1200px; margin-left:10px;",
                               br(),
                               fluidRow(province_selector_ui,
                                        column(10,
                                               h3(textOutput("province_title")),
                                               fluidRow(uiOutput("province_cards")))),
                               br(),
                               fluidRow(column(6,
                                               h3("Species Records by SARA Status",
                                                  style="text-align:center;"),
                                               DTOutput("province_status")),
                                        column(6,
                                               h3("Most Common Threat Categories",
                                                  style="text-align:center;"),
                                               DTOutput("province_top_threats"))
                                        ),
                               br(),
                               h3("Species Records"),
                               DTOutput("province_species_table")))),
  # FOOTER INFORMATION
  hr(),
  tags$div(
    style = "
    font-size:12px;
    color:#6c757d;
    margin-top:20px;
    margin-bottom:20px;
  ",
    "Freshwater SAR Species Explorer v1.0",
    tags$br(),
    "Data sources: Species at Risk Public Registry, species profile pages, recovery planning documents, and associated public information.",
    tags$br(),
    paste("Application updated:", format(Sys.Date(), "%Y-%m-%d")),
    tags$br(),
    "Contact: Karl Lamothe, Fisheries and Oceans Canada"
  )
)

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
  # COMPLIANCE REACTIVE
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
                  select(record_name, range_text),by = "record_name") %>%
      filter(threat_category == input$explore_threat)
    if(input$explore_range != "All"){
      dat <- dat %>%
        filter(grepl(input$explore_range, range_text, fixed = TRUE))}
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
        filter(grepl(input$province_range, range_text,fixed = TRUE))}
    dat
  })
  
  ##############################################################################
  # FILTER UPDATE OBSERVERS
  ##############################################################################
  #Species selector
  observe({
    species_choices <- species_master_app
    if(input$status != "All"){
      species_choices <- species_choices %>%
        filter(sara_status == input$status)}
    if(input$range != "All"){
      species_choices <- species_choices %>%
        filter(grepl(input$range,range_text, fixed = TRUE))}
    updateSelectInput(session, "species",
                      choices = sort(unique(species_choices$common_name)))
  })
  
  # Document selector
  observe({
    species_choices <- species_master_app
    
    if(input$status_docs != "All"){
      species_choices <- species_choices %>%
        filter(sara_status == input$status_docs)}
    
    if(input$range_docs != "All"){
      species_choices <- species_choices %>%
        filter(grepl(input$range_docs, range_text, fixed = TRUE))}
    
    updateSelectInput(session, "species_docs",
                      choices = sort(unique(species_choices$common_name)))
  })
  
  # Threat selector
  observe({
    updateSelectInput(session, "explore_threat",
                      choices = sort(
                        unique(species_threats_app$threat_category)))
  })
  
  ##############################################################################
  # NAVIGATION OBSERVERS
  ##############################################################################
  observeEvent(input$explore_species_rows_selected, {
    row <- input$explore_species_rows_selected
    if(length(row) == 0) return()
    selected <- explore_results()[row, ]
    selected_record_from_explore(selected$record_name)
    updateSelectInput(session,"species", selected = selected$common_name)
    updateTabsetPanel(session, "main_tabs", selected = "Species Information")
  })
  
  ##############################################################################
  # SPECIES / DU SELECTION OBSERVERS
  ##############################################################################
  observeEvent(input$species, {
    records <- species_master_app %>%
      filter(common_name == input$species) %>%
      arrange(record_name) %>%
      pull(record_name)
    target_record <- selected_record_from_explore()
    if(!is.null(target_record) && target_record %in% records){
      updateSelectInput(session,"record",choices = records,
                        selected = target_record)
      selected_record_from_explore(NULL)
    } else {
      updateSelectInput(session, "record", choices = records,
                        selected = records[1])
    }
  })

  observeEvent(input$species_docs, {
    records <- species_master_app %>%
      filter(common_name == input$species_docs)
    if(input$range_docs != "All"){
      records <- records %>%
        filter(grepl(input$range_docs, range_text, fixed = TRUE))}
    records <- records %>%
      arrange(record_name) %>%
      pull(record_name)
    updateSelectInput(session, "record_docs",choices = records,selected = records[1])
  })
  
  ##############################################################################
  # TAB SYNCHRONIZATION OBSERVERS
  ##############################################################################
  observeEvent(input$species, {
    updateSelectInput(session, "species_docs",selected = input$species)
  })
  
  observeEvent(input$species_docs, {
    updateSelectInput(session, "species", selected = input$species_docs)
  })
  
  observeEvent(input$record_docs, {
    updateSelectInput(session,"record", selected = input$record_docs)
  })
  
  observeEvent(input$record, {
    updateSelectInput(session,"record_docs", selected = input$record)
  })
  
  observeEvent(input$range, {
    updateSelectInput(session, "range_docs",selected = input$range)
  })
  
  observeEvent(input$range_docs, {
    updateSelectInput(session, "range", selected = input$range_docs)
  })
  
  ##############################################################################
  # SPECIES INFORMATION OUTPUTS
  ##############################################################################
  output$species_header <- renderUI({
    sp <- selected_species()
    species_url <- paste0(
      "https://species-registry.canada.ca/index-en.html#/species/", sp$id[1])
    img_file <- sp$local_image_file[1]
    if(is.na(img_file) || !file.exists(file.path(
      "www","images",img_file))){
      img_file <- "no_image_available.jpg"
    }
    tagList(fluidRow(column(3, tags$img(src = paste0("images/", img_file),
            style = "
            width:100%;
            max-height:200px;
            object-fit:contain;
            background-color:white;
            border-radius:8px;
            border:1px solid #d9d9d9;
          "),
          if (
            !is.na(sp$photoCopyright[1]) &&
            sp$photoCopyright[1] != "") {
            tags$p(style = 
            "font-size:11px;
             color:grey; 
            margin-top:5px;",
                   sp$photoCopyright[1])
          }
          
        ),
        column(9, h2(sp$common_name),
               tags$h4(tags$em(sp$scientific_name)),
               tags$p(tags$a("View Species Profile on the SAR Registry",
                   href = species_url, target = "_blank")),
               tags$p(strong("Designatable Unit: "),sp$record_name),
               tags$p(strong("SARA Status: "),sp$sara_status),
               tags$p(strong("COSEWIC Status: "),sp$cosewic_status),
               tags$p(strong("Range: "), sp$range_text))))})
  
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
  paste(n, "Threat Categories")
  
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
output$planning_summary <- renderUI({
  dat <- recovery_coverage
  
  if(input$planning_status != "All"){
    dat <- dat %>%
      filter(sara_status == input$planning_status)}
  
  if(input$planning_range != "All"){
    dat <- dat %>%
      filter(grepl(input$planning_range, range_text, fixed = TRUE))}
  
  med <- median(dat$years_since_recovery_planning, na.rm = TRUE)
  old <- sum(dat$years_since_recovery_planning > 10, na.rm = TRUE)
  old_word <- if(old <= 10){
    c("Zero","One","Two","Three","Four", "Five",
      "Six","Seven","Eight","Nine","Ten")[old + 1]
  } else {
    as.character(old)
  }
  tags$div(
    style = "
    background-color:#f8f9fa;
    border-left:5px solid #0d6efd;
    padding:15px;
    margin-bottom:15px;
    border-radius:4px;",
    
    tags$p(style="margin-bottom:8px;",
           paste0( "The median age of the most recent recovery planning document is ",
                   round(med,1)," years.")),
    
    tags$p(style="margin-bottom:0;", paste0(old_word,
        " designatable units have not had a recovery planning document published in over 10 years.")))
})

# recovery strategy coverage table
output$recovery_coverage <- renderDT({
  dat <- recovery_coverage
  
  if(input$planning_status != "All"){
    dat <- dat %>%
      filter(sara_status == input$planning_status)}
  
  if(input$planning_range != "All"){
    dat <- dat %>%
      filter(grepl(input$planning_range, range_text, fixed = TRUE))}
  
  dat %>%
    mutate(`Recovery Strategy` =
             ifelse(coalesce(has_recovery_strategy, FALSE), "✅","❌"),
           
           `Action Plan` =
             ifelse(coalesce(has_action_plan, FALSE), "✅","❌") ,
           
           latest_document_date =
             format(latest_document_date, "%b %d, %Y"),
           
           compliance_status = case_when(
             compliance_status == "Up to Date" ~ "✅Up to Date",
             compliance_status == "Overdue" ~ "🔴 Report Overdue",
             compliance_status == "Not Yet Due" ~ "🟡 Not Yet Required")) %>%
    
    arrange(desc(years_since_recovery_planning),
            is.na(years_since_recovery_planning)) %>%
    select(record_name,
           `Recovery Strategy`,
           `Action Plan`,
           compliance_status,
           latest_document_type,
           latest_document_date,
           years_since_recovery_planning) %>%
  
    rename(
      `Designatable Unit` = record_name,
      `Progress Report Status` = compliance_status,
      `Latest Document Type` = latest_document_type,
      `Latest Document Date` = latest_document_date,
      `Years Since Recovery Document` = years_since_recovery_planning) %>%
    
    datatable(rownames = FALSE, width = "100%",
              options = list(pageLength = 10,
                             columnDefs = list(
                               list(className = "dt-center", targets = c(1, 2)),
                               list(width = "220px", targets = 0),  # DU
                               list(width = "150px", targets = 1),  
                               list(width = "150px", targets = 2),   
                               list(width = "250px", targets = 3), 
                               list(width = "190px", targets = 4),  
                               list(width = "180px", targets = 5),
                               list(width = "250px", targets = 6))
                               )) %>%
    
    formatStyle("Years Since Recovery Document",
                backgroundColor = styleInterval(c(5, 10),
                                                c("#d4edda","#fff3cd","#f8d7da")))%>%
    formatStyle(c("Recovery Strategy",
                  "Action Plan"), textAlign = "center")%>%
    
    formatStyle("Progress Report Status", target = "cell",
                backgroundColor = styleEqual(c("✅Up to date","🔴 Report Overdue",
                                               "🟡 Not Yet Required"),
                                             c("#d4edda",  
                                               "#f8d7da",  
                                               "#fff3cd")))
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
  
  tags$div(style = "
      background-color:#f8f9fa;
      border:1px solid #dee2e6;
      border-radius:10px;
      padding:15px;
      margin-bottom:20px;
      max-width:650px;
      box-shadow:0 1px 3px rgba(0,0,0,0.08);
    ",
    
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
    
    if(prog$recovery_strategy_date[1] !=comp$first_recovery_strategy_date[1]){
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

##############################################################################
# THREAT SUMMARY OUTPUTS
##############################################################################
output$common_threats <- renderDT({
  dat <- species_threats_app %>%
    left_join(species_master_app %>%
                select(record_name, range_text),by = "record_name")
  if(input$threat_range != "All"){
    dat <- dat %>%
      filter(grepl(input$threat_range, range_text, fixed = TRUE))}
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
        select(record_name, range_text), by = "record_name")
  if(input$threat_range != "All"){
    dat <- dat %>%
      filter(grepl(input$threat_range,
                   range_text,fixed = TRUE))}
  
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
           `SARA Status` = sara_status)
  datatable(dat, rownames = FALSE, 
            selection = list(mode = "single",target = "row"),
            options = list(pageLength = 10))}, server = FALSE)

output$explore_species_count <- renderText({
  paste(nrow(explore_results()), "Species Records")})


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
                select(record_name, range_text), by = "record_name")
  
  if(input$province_range != "All"){
    dat <- dat %>%
      filter(grepl(input$province_range, range_text,fixed = TRUE))}
  
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
  fluidRow(column(3,summary_card(n_records, "DUs","white", "#dee2e6")),
           column(3,summary_card(n_endangered, "Endangered","#f8d7da", "#f5c2c7")),
           column(3,summary_card(n_threatened, "Threatened","#fff3cd", "#ffecb5")),
           column(3,summary_card(n_extirpated, "Extirpated","#e2d9f3", "#d1c4e9")))
})

}

# run app
shinyApp(ui = ui, server = server)
