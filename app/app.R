library(dplyr)
library(shiny)
library(DT)
library(shinydashboard)
library(bslib)
library(ggplot2)

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
  filter(sara_status %in% c("Threatened","Endangered", "Extirpated")) %>%
  mutate(threat_group = case_when(
    threat_category %in% c(
        "Urbanization", "Residential and Commercial Development") ~ "Urban Development",
    threat_category %in% c(
      "Agriculture", "Agricultural and Forestry Effluents") ~ "Agriculture",
    threat_category %in% c(
        "Barriers", "Transportation and Service Corridors") ~ "Connectivity",
    threat_category %in% c(
      "Contaminants", "Sedimentation") ~ "Water Quality",
    TRUE ~ threat_category))

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
                     p("This application provides access to species profiles, threat information, and recovery planning documents for Schedule 1 freshwater fishes listed under the Species at Risk Act (SARA)."),
                     h4("Data Sources"),
                     tags$ul(
                       tags$li("Species at Risk Public Registry"),
                       tags$li("Species profile pages"),
                       tags$li("Recovery and planning documents")),
                     h4("Coverage"),
                     tags$ul(
                       tags$li(
                         paste(
                           n_distinct(species_master_app$record_name),
                           "species records")),
                       tags$li(
                         paste(
                           n_distinct(species_documents$document_id),
                           "documents")),
                       tags$li(
                         paste(
                           n_distinct(species_threats_app$threat_group),
                           "threat groups"))),
                     hr(),
                     fluidRow(
                       column(3, valueBoxOutput("species_box")),
                       column(3, valueBoxOutput("document_box")),
                       column(3, valueBoxOutput("threat_box")),
                       column(3, valueBoxOutput("doctype_box"))),
                     hr(),
                     fluidRow(column(6, h3("Fish Taxa by SARA Status",
                                           style = "text-align:center;"),
                                     plotOutput("status_plot", height = "300px")),
                              column(6, h3("Most Common Threat Categories",
                                           style = "text-align:center;"),
                                     plotOutput("threat_plot", height = "300px")))),
        tabPanel(
          "Overview",
          br(),
          uiOutput("species_header"),
          h3("Description"),
          textOutput("description"),
          h3("Population"),
          textOutput("population"),
          h3("Habitat"),
          textOutput("habitat"),
          h3("Biology"),
          textOutput("biology")
        ),
        
        tabPanel(
          "Threats",
          br(),
          h3(textOutput("n_threats")),
          DTOutput("threats"),
          br(),
          h3("Threat Information"),
          textOutput("profile_threats")),
        
        tabPanel(
          "Documents",
          br(),
          h3(textOutput("n_documents")),
          DTOutput("documents")),
        
        tabPanel("Explore",
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
  )
)

# server
server <- function(input, output, session){
  selected_species <- reactive({
    species_master_app %>%
      filter(record_name == input$record)
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
      filter(threat_group == input$explore_threat)
    if(input$explore_status != "All"){
      dat <- dat %>%
        filter(sara_status == input$explore_status)}
    dat %>%
      distinct(common_name, record_name, sara_status)
    
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
                        unique(species_threats_app$threat_group)))
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
               tags$p(strong("Designatable Unit: "),sp$record_name),
               tags$p(strong("SARA Status: "),sp$sara_status),
               tags$p(strong("COSEWIC Status: "),sp$cosewic_status))),
      tags$hr())})
  
  output$species_box <- renderValueBox({
    valueBox(
      value = n_distinct(species_master_app$record_name),
      subtitle = "Species Records", color = "blue")
  })
  
  output$document_box <- renderValueBox({
    valueBox(value = n_distinct(species_documents$document_id),
             subtitle = "Documents", color = "blue")
  })
  
  output$threat_box <- renderValueBox({
    valueBox(value = n_distinct(species_threats_app$threat_group),
             subtitle = "Threat Groups",color = "blue")
  })
  
  output$doctype_box <- renderValueBox({
    valueBox(value = n_distinct(species_documents$primary_document_type),
      subtitle = "Document Types", color = "blue")
  })
  
  output$status_plot <- renderPlot({
    species_master_app %>%
      count(sara_status) %>%
      ggplot(aes(x = reorder(sara_status, n), y = n)) +
      geom_col(fill="#2C7FB8") + coord_flip() +
      guides(fill = "none") +
      labs(y="Number of Taxa")+
      dashboard_theme + 
      theme(axis.title.y = element_blank())
  })
  
  output$threat_plot <- renderPlot({
    species_threats_app %>%
      distinct(record_name, threat_group) %>%
      count(threat_group, sort = TRUE) %>%
      slice_head(n = 5) %>%
      ggplot(aes(x = reorder(threat_group, n), y = n)) +
      geom_col(fill = "#2C7FB8") +
      coord_flip() +
      labs(x = NULL, y = "Number of Taxa", title = NULL) +
      dashboard_theme +
      theme(axis.title.x = element_blank())
  })
  
# description
output$description <- renderText({
  species_profiles %>%
    filter(record_name == input$record) %>%
    pull(description)
})

output$habitat <- renderText({
  species_profiles %>%
    filter(record_name == input$record) %>%
    pull(habitat)
})

output$n_threats <- renderText({
  n <- species_threats_app %>%
    filter(record_name == input$record) %>%
    distinct(threat_group) %>%
    nrow()
  paste(n, "Threat Categories")
  
})

output$profile_threats <- renderText({
  species_profiles %>%
    filter(record_name == input$record) %>%
    pull(threats)
})

output$threats <- renderDT({
  species_threats_app %>%
    filter(record_name == input$record) %>%
    select(threat_group) %>%
    distinct() %>%
    rename(`Threat Group` = threat_group) %>%
    datatable(options = list( pageLength = 10, dom = "t"), rownames = FALSE)
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
  species_profiles %>%
    filter(record_name == input$record) %>%
    pull(biology)
})

output$population <- renderText({
  species_profiles %>%
    filter(record_name == input$record) %>%
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



}

# run app
shinyApp(ui = ui, server = server)
