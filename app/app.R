library(dplyr)
library(shiny)
library(DT)
library(shinydashboard)
library(bslib)
library(ggplot2)
library(stringr)

# load data
species_master <- readRDS("data/species_master.rds")
species_profiles <- readRDS("data/species_profiles.rds")
species_threats <- readRDS("data/species_threats.rds")
species_documents <- readRDS("data/species_documents.rds")

dashboard_theme <- theme_bw() +
  theme(axis.title   = element_text(size=24, family="sans", colour="black"),
        axis.text.x  = element_text(size=20, family="sans", colour="black"),
        axis.text.y  = element_text(size=20, family="sans", colour="black"),
        strip.text   = element_text(size=20, family="sans", colour="black"),
        plot.title   = element_text(size=24, family="sans", colour="black"),
        panel.border = element_rect(colour="black"),
        legend.position = "none")

# app specific data
species_master_app <- species_master %>%
  filter(sara_status %in% c("Threatened", "Endangered","Extirpated"))

species_master_app <- species_master_app %>%
  mutate(image_file = paste0(common_name %>%
                               tolower() %>%
                               gsub(" ", "_", .),".jpg"))

species_threats_app <- species_threats %>%
  filter(sara_status %in% c("Threatened", "Endangered", "Extirpated"))

# derived recovery data dataset
recovery_coverage <- species_master_app %>%
  distinct(record_name) %>%
  left_join(species_documents %>%
  group_by(record_name) %>%
    summarise(has_recovery_strategy = any(grepl(
      "Recovery Strategy",documentTitle, ignore.case = TRUE)),
      has_action_plan = any(primary_document_type == "Action Plan") |
        any(grepl("Action Plan", documentTitle, ignore.case = TRUE)),
      has_management_plan =any(
        grepl("Management Plan", documentTitle, ignore.case = TRUE)),
      latest_recovery_planning_document =suppressWarnings(
        max(as.Date(documentCreateDate[grepl(
          "Recovery Strategy|Action Plan",documentTitle,ignore.case = TRUE)]),
          na.rm = TRUE)), .groups = "drop"),by = "record_name") %>%
  mutate(latest_recovery_planning_document = as.Date(
      ifelse(is.infinite(latest_recovery_planning_document), NA,
             latest_recovery_planning_document), origin = "1970-01-01"),
    years_since_recovery_planning =
      round(as.numeric(Sys.Date() -
            as.Date(latest_recovery_planning_document,
                    origin = "1970-01-01")) / 365.25,1))

# user interface
ui <- fluidPage(
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
    "))
  ),
  titlePanel(
    div(style = "
      font-size:36px;
      font-weight:bold;
      color:#2c3e50;
    ",
      "Freshwater SAR Species Explorer")),
  sidebarLayout(
    sidebarPanel(width=3,
      selectInput("status", "SARA Status", choices = c(
          "All",
          "Endangered",
          "Threatened",
          #"Special Concern",
          "Extirpated")),
      selectInput(
        "species",
        "Species",
        choices = sort(unique(species_master_app$common_name))),
      selectInput(
        "record",
        "Population / Designatable Unit",
        choices = NULL)),
    mainPanel(
      tabsetPanel(id = "main_tabs",
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
        tabPanel(
          "Species Information",
          br(),
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
        ),
        
        tabPanel(
          "Species Documents",
          br(),
          h3(textOutput("n_documents")),
          DTOutput("documents")),
        
        tabPanel(
          "Recovery Planning",
          br(),
          h3("Recovery Planning Coverage"),
          br(),
          uiOutput("planning_summary"),
          br(),
          DTOutput("recovery_coverage", width = "100%"),
          tags$p(
            style = "font-style: italic; color: #666666;",
            "Recovery planning coverage is based on recovery strategies, action plans, and associated progress reports identified within Species at Risk Public Registry planning documents."
          )),
        
        tabPanel(
          "Threat Summary",
          br(),
          fluidRow(column(6,h3("DUs with the Most Threat Categories",
                               style = "text-align:center;"),
                          DTOutput("top_threat_species")),
                   column(6,h3("Most Common Threat Categories",
                               style = "text-align:center;"),
                          DTOutput("common_threats"))),
          tags$p(style = "font-style: italic; color: #666666;",
                 "Threat frequencies are based on threat categories identified from Species at Risk Public Registry species profile information."
          )),

        tabPanel("Explore Threats",
          br(),
          selectInput("explore_status","SARA Status",
                      choices = c("All","Endangered","Threatened","Extirpated"),
            selected = "All"),
          selectInput("explore_threat", "Threat Group",choices = NULL),
          br(),
          h3(textOutput("explore_species_count")),
          DTOutput("explore_species"))
      )
    )
  ),
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

# server
server <- function(input, output, session){
  selected_species <- reactive({
    species_master_app %>%
      filter(record_name == input$record)%>%
      slice(1)
  })
  
  selected_record_from_explore <- reactiveVal(NULL)
  
  observe({species_choices <- species_master_app
    if(input$status != "All"){
      species_choices <- species_choices %>%
        filter(sara_status == input$status)}
    updateSelectInput(
      session,"species", choices = sort(unique(species_choices$common_name)))
  })
  
  explore_results <- reactive({
    dat <- species_threats_app %>%
      filter(threat_category == input$explore_threat)
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


  observeEvent(input$explore_species_rows_selected, {
    row <- input$explore_species_rows_selected
    if(length(row) == 0) return()
    selected <- explore_results()[row, ]
    selected_record_from_explore(selected$record_name)
    updateSelectInput(session,"species", selected = selected$common_name)
    updateTabsetPanel(session, "main_tabs", selected = "Overview")
  })
  
  observe({
    updateSelectInput(session, "explore_threat",
                      choices = sort(
                        unique(species_threats_app$threat_category)))
  })
  
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

  ######################################################
  # outputs
  ######################################################
  output$species_header <- renderUI({
    sp <- selected_species()
    species_url <- paste0(
      "https://species-registry.canada.ca/index-en.html#/species/", sp$id[1])
    img_file <- sp$local_image_file[1]
    if(is.na(img_file) || !file.exists(file.path(
      "www","images",img_file))){
      img_file <- "no_image_available.jpg"
    }
    tagList(
      fluidRow(
        column(3, tags$img(
          src = paste0("images/", img_file),
            style = "
            width:100%;
            max-height:200px;
            object-fit:contain;
            background-color:white;
            border-radius:8px;
            border:1px solid #d9d9d9;
          "
          ),
          if (
            !is.na(sp$photoCopyright[1]) &&
            sp$photoCopyright[1] != ""
          ) {
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
               tags$p(strong("COSEWIC Status: "),sp$cosewic_status))),
      tags$hr())})
  
# description
output$description <- renderText({
  selected_profile() %>%
    pull(description)
})

output$habitat <- renderText({
  selected_profile() %>%
    pull(habitat)
})

output$n_threats <- renderText({
  n <- species_threats_app %>%
    filter(record_name == input$record) %>%
    distinct(threat_category) %>%
    nrow()
  paste(n, "Threat Categories")
  
})

output$profile_threats <- renderText({
  selected_profile() %>%
    pull(threats)
})

output$planning_summary <- renderUI({
  med <- median(recovery_coverage$years_since_recovery_planning, na.rm = TRUE)
  old <- sum(recovery_coverage$years_since_recovery_planning > 10, na.rm = TRUE)
  old_word <- c("Zero", "One", "Two", "Three", "Four",
                "Five", "Six", "Seven", "Eight", "Nine", "Ten")[old + 1]
  tags$ul(style = "font-size:16px;",
    tags$li(paste("Median years since publication of the most recent recovery planning document publications: ", round(med, 1))),
    tags$li(paste(old_word," DUs have not had a recovery planning document published in over 10 years.")))
})

output$recovery_coverage <- renderDT({
  recovery_coverage %>%
    mutate(`Recovery Strategy or Progress Report Complete` =
             ifelse(coalesce(has_recovery_strategy, FALSE), "Yes", "No"),
           
           `Action Plan Complete` =
             ifelse(coalesce(has_action_plan, FALSE), "Yes", "No")) %>%
    
    arrange(desc(years_since_recovery_planning),
            is.na(years_since_recovery_planning)) %>%
    select(record_name,
           `Recovery Strategy or Progress Report Complete`,
           `Action Plan Complete`,
           latest_recovery_planning_document,
           years_since_recovery_planning) %>%
    rename(`Designatable Unit` = record_name,
           `Most Recent Planning Document` =
             latest_recovery_planning_document,
           `Years Since Recovery Planning Document` =
             years_since_recovery_planning) %>%
    datatable(rownames = FALSE, options = list(pageLength = 10,
                                               autoWidth = TRUE))%>%
    formatStyle("Years Since Recovery Planning Document",
                backgroundColor = styleInterval(c(5, 10),
                                                c("#d4edda","#fff3cd","#f8d7da")))
})

output$threats <- renderDT({
  dat <- species_threats_app %>%
    filter(record_name == input$record) %>%
    select(threat_category) %>%
    distinct()
  names(dat) <- ""
  datatable(dat, rownames = FALSE, options = list(pageLength = 10, dom = "t"))
})

output$documents <- renderDT({
  docs <- species_documents %>%
    filter(record_name == input$record) %>%
    mutate(documentCreateDate = as.Date(documentCreateDate),
           Document = paste0('<a href="',document_url,'" target="_blank">',
                             documentTitle,'</a>')) %>%
    arrange(desc(documentCreateDate)) %>%
    select(primary_document_type,Document,documentCreateDate) %>%
    rename(
      `Document Type` = primary_document_type,
      `Document Date` = documentCreateDate)
  datatable( docs,escape = FALSE,rownames = FALSE)
})

output$n_documents <- renderText({
  n <- species_documents %>%
    filter(record_name == input$record) %>%
    distinct(document_id) %>%
    nrow()
  paste(n, "Documents")
})

output$biology <- renderText({
  selected_profile() %>%
    pull(biology)
})

output$population <- renderText({
  selected_profile() %>%
    pull(population)
})

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

output$top_threat_species <- renderDT({
  species_threats_app %>%
    distinct(record_name, threat_category) %>%
    count(record_name, sort = TRUE, name = "Threat Categories") %>%
    slice_head(n = 10) %>%
    rename(`Designatable Units` = record_name) %>%
    datatable( rownames = FALSE,options = list(pageLength = 10,dom = "t",
                                               ordering = FALSE))
})

output$common_threats <- renderDT({
  species_threats_app %>%
    distinct(record_name, threat_category) %>%
    count(threat_category, sort = TRUE, name = "Designatable Units") %>%
    rename(`Threat Category` = threat_category) %>%
    datatable( rownames = FALSE, options = list(pageLength = 10,dom = "t",
                                                ordering = FALSE,
                                                columnDefs = list(
                                                  list(width = "70%", targets = 0),
                                                  list(width = "30%", targets = 1)
                                                )
    ))
})


}

# run app
shinyApp(ui = ui, server = server)
