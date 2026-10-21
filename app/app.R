library(dplyr)
library(shiny)
library(DT)

# load data
species_master <- readRDS("data/species_master.rds")
species_profiles <- readRDS("data/species_profiles.rds")
species_threats <- readRDS("data/species_threats.rds")
species_documents <- readRDS("data/species_documents.rds")

# user interface
ui <- fluidPage(
  titlePanel("Freshwater SAR Species Explorer"),
  sidebarLayout(
    sidebarPanel(
      selectInput("species", "Species",
        choices = sort(unique(species_master$common_name))),
      selectInput(
        "record",
        "Population / Designatable Unit",
        choices = NULL
      )),
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Overview",
          br(),
          tableOutput("overview"),
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
          DTOutput("threats")),
        tabPanel(
          "Documents",
          br(),
          h3(textOutput("n_documents")),
          DTOutput("documents")
        )
      )
    )
  )
)

# server
server <- function(input, output, session){
  selected_species <- reactive({
    species_master %>%
      filter(record_name == input$record)
  })

  observe({
    records <- species_master %>%
      filter(common_name == input$species) %>%
      arrange(record_name) %>%
      pull(record_name)
    updateSelectInput(session, "record", choices = records, selected = records[1])
  })

# output
output$overview <- renderTable({
  selected_species() %>%
    select(
      record_name,
      common_name,
      scientific_name,
      sara_status,
      cosewic_status)
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
  n <- species_threats %>%
    filter(record_name == input$record) %>%
    distinct(threat_category) %>%
    nrow()
  paste(n, "Threat Categories")
  
})

output$threats <- renderDT({
  species_threats %>%
    filter(record_name == input$record) %>%
    select(threat_category) %>%
    distinct() %>%
    datatable(options = list( pageLength = 10, dom = "t"), rownames = FALSE)
})

output$documents <- renderDT({
  docs <- species_documents %>%
    filter(record_name == input$record) %>%
    mutate(
      Document = paste0(
        '<a href="',
        document_url,
        '" target="_blank">',
        documentTitle,
        '</a>'
      )) %>%
    select(primary_document_type, Document,documentCreateDate)
  datatable(docs, escape = FALSE)
})

output$n_documents <- renderText({
  n <- species_documents %>%
    filter(record_name == input$record) %>%
    distinct(document_id) %>%
    nrow()
  paste(n, "Documents")
})

output$population <- renderText({
  species_profiles %>%
    filter(record_name == input$record) %>%
    pull(population)
})

}

shinyApp(
  ui = ui,
  server = server
)
