source("scripts/00_setup.R")

# load data
species_master <- read.csv("data_processed/freshwater_species_master.csv")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Helpers
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# retrieve documents
get_document_page <- function(skip = 0, top = 100){
  payload <- list(count = TRUE,search = "*",searchMode = "all",
                  queryType = "full",skip = skip,top = top)
  response <- POST(url = document_endpoint,
                   add_headers(
                     `api-key` = api_key,
                     `Content-Type` = "application/json"),
                   body = payload, encode = "json")
  content_text <- content(response,"text",encoding = "UTF-8")
  jsonlite::fromJSON(content_text)$value
}

get_primary_document_type <- function(x){
  priority <- c(
    "RECOVERY_STRATEGY",
    "ACTION_PLAN",
    "MANAGEMENT_PLAN",
    "COSEWIC_ASSESSMENTS_AND_STATUS_REPORTS",
    "COSEWIC_ANNUAL_REPORTS",
    "RESPONSE_STATEMENTS",
    "CRITICAL_HABITAT_ORDERS",
    "RESIDENCE_DESCRIPTIONS",
    "CONSULTATION_DOCUMENTS")
  hit <- priority[
    priority %in% x
  ]
  if(length(hit) == 0){
    return(NA_character_)
  }
  hit[1]
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# download entire index
documents_raw <- purrr::map_dfr(
  seq(0, 3800, by = 100),
  get_document_page)

attachment_info <- purrr::map_dfr(
  seq_len(nrow(documents_raw)),
  function(i){
    attachments <- parse_attachments(
      documents_raw$attachments[[i]])
    if(is.null(attachments)){
      return(
        tibble(
          document_id = documents_raw$id[i],
          attachment_title = NA_character_,
          attachment_date = NA_character_,
          file = NA_character_,
          html = NA_character_))}
    attachments <- attachments %>%
      mutate(
        html = if(!"html" %in% names(.)) NA_character_ else html,
        file = if(!"file" %in% names(.)) NA_character_ else file)
    attachments %>%
      transmute(
        document_id = documents_raw$id[i],
        attachment_title = attachmentTitle,
        attachment_date = attachmentPublicationDate,
        file, html)})

attachment_info <- attachment_info %>%
  mutate(attachment_title = clean_html(attachment_title))

# build urls
file_base <- "https://ecprccsarstacct.z9.web.core.windows.net/files/"
attachment_info <- attachment_info %>%
  mutate(document_url = paste0(file_base, file))

species_documents <- documents_raw %>%
  left_join(attachment_info, by = c("id" = "document_id")) %>%
  rename(document_id = id) %>%
  unnest_longer(species, values_to = "species_id") %>%
  filter(species_id %in% species_master$id) %>%
  left_join(species_master %>%
              select(id,record_name,common_name,scientific_name,
                     sara_status,cosewic_status), by = c("species_id" = "id")) %>%
  mutate(primary_document_type = 
           purrr::map_chr(documentTypeId, get_primary_document_type))

species_documents %>%
  filter(grepl("Atlantic Whitefish", documentTitle)) %>%
  select(
    documentTitle,
    attachment_title,
    attachment_date,
    document_url
  )

species_documents <- species_documents %>%
  mutate(across(c(
    documentTitle,
    documentDescription),
    clean_html))

species_documents <- species_documents %>%
  mutate(across(where(is.character), clean_html))

# build document type summary
document_types <- species_documents %>%
  select(document_id, documentTypeId) %>%
  unnest_longer(documentTypeId)

# species document summary
species_document_summary <- species_documents %>%
  distinct(species_id, document_id) %>%
  count(species_id, name = "n_documents") %>%
  left_join(species_master, by = c("species_id" = "id"))

# checks
document_types %>%
  count(documentTypeId, sort = TRUE) %>%
  print(n = 33)

species_documents %>%
  count(primary_document_type,sort = TRUE)

species_master %>%
  anti_join(species_documents %>% distinct(species_id), 
            by = c("id" = "species_id")) %>%
  count(sara_status,sort = TRUE)

species_documents <- species_documents %>%
  mutate(
    documentTypeId = purrr::map_chr(
      documentTypeId,
      ~ paste(.x, collapse = "; ")))

# export
names(species_documents)
head(species_documents)
species_documents_export <- species_documents %>%
  select(species_id,record_name,common_name,scientific_name,sara_status,
         cosewic_status,document_id,documentTypeId,primary_document_type,documentTitle,
         documentDescription,documentCreateDate,attachment_title,
         attachment_date,document_url)
head(species_documents_export)

# change to sentence case rather than all caps
species_documents_export <- species_documents_export %>%
  mutate(across(c(
    documentTypeId, primary_document_type),
    ~ .x %>%
      str_replace_all("_", " ") %>%
      str_to_title()))
head(species_documents_export)
head(species_document_summary)

species_documents_export <- species_documents_export %>%
  mutate(
    planning_document_type = case_when(
      
      grepl(
        "Report on the Progress|Progress Towards the Implementation|Implementation Report",
        attachment_title,
        ignore.case = TRUE
      ) ~ "Progress Report",
      
      primary_document_type == "Cosewic Assessments And Status Reports" ~
        "COSEWIC Assessment",
      
      grepl("Recovery Strategy and Action Plan", attachment_title,
            ignore.case = TRUE) ~ "Recovery Strategy & Action Plan",
      
      grepl("Recovery Strategy", attachment_title,
            ignore.case = TRUE) ~ "Recovery Strategy",
      
      grepl("Action Plan", attachment_title,
            ignore.case = TRUE) ~ "Action Plan",
      
      grepl("Management Plan", attachment_title,
            ignore.case = TRUE) ~ "Management Plan",
      
      TRUE ~ NA_character_
    ))

# remove proposed docs if there is a final version
species_documents_export <- species_documents_export %>%
  mutate(
    base_title = attachment_title %>%
      gsub("\\s*\\[Proposed\\]", "", .) %>%
      gsub("\\s*\\(Proposed\\)", "", .) %>%
      gsub("\\s*\\[Final Version\\]", "", .)
  ) %>%
  group_by(record_name, base_title) %>%
  filter(
    !(
      grepl("\\[Proposed\\]|\\(Proposed\\)",
            attachment_title,
            ignore.case = TRUE) &
        any(
          !grepl("\\[Proposed\\]|\\(Proposed\\)",
                 attachment_title,
                 ignore.case = TRUE)
        )
    )
  ) %>%
  ungroup() %>%
  select(-base_title)

write_csv(species_documents_export, "data_processed/species_documents.csv")
write_csv(species_document_summary, "data_processed/species_document_summary.csv")
