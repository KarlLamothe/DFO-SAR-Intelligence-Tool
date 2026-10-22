# List of packages being used
list.of.packages <- c("tidyverse","rvest","xml2","httr","DBI","RSQLite","readr",
                      "janitor","lubridate","stringr","glue","jsonlite","DT","shiny")

# Identify packages in the list that are not on the computer
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

# Install packages in "new.packages"
if(length(new.packages)) install.packages(new.packages); rm(list.of.packages); rm(new.packages)

# packages
library(tidyverse)      
library(rvest)  
library(xml2)      
library(httr)   
library(DBI)    
library(RSQLite)  
library(janitor)      
library(lubridate) 
library(stringr) 
library(glue)  
library(jsonlite)  
library(DT)  
library(shiny)  
library(readr)  

suppressPackageStartupMessages({
  library(tidyverse)      
  library(rvest)  
  library(xml2)      
  library(httr)   
  library(DBI)    
  library(RSQLite)  
  library(janitor)      
  library(lubridate)
  library(shiny)  
  library(stringr) 
  library(glue) 
  library(jsonlite)  
  library(readr)  
  library(DT)  
  
})

options(scipen=999) # Remove scientific notation

#Helper functions
# remove html tags
remove_html <- function(x) {
  gsub("<.*?>", "", x)
}

# extract species profile
extract_profile <- function(species_id, profile_obj){
  
  if(length(profile_obj) == 0){
    
    return(
      tibble(
        id = species_id,
        description = NA_character_,
        population = NA_character_,
        habitat = NA_character_,
        biology = NA_character_,
        threats = NA_character_,
        critical_habitat = NA_character_,
        federal_protection = NA_character_,
        other_protection = NA_character_
      )
    )
    
  }
  
  profile_json <- jsonlite::fromJSON(
    profile_obj[[1]]
  )
  
  tibble(
    id = species_id,
    description = profile_json$description,
    population = profile_json$population,
    habitat = profile_json$habitat,
    biology = profile_json$biology,
    threats = profile_json$threats,
    critical_habitat = profile_json$criticalHabitat,
    federal_protection = profile_json$federalProtection,
    other_protection = profile_json$otherProtection
  )
  
}

# clean html
clean_html <- function(x){
  x <- stringr::str_remove_all(x, "<.*?>")
  stringr::str_squish(x)
}

# get api
api_key <- Sys.getenv("SAR_API_KEY")
endpoint <- Sys.getenv("SAR_ENDPOINT")
document_endpoint <- Sys.getenv("SAR_DOCUMENT_ENDPOINT")

# parse attachments
parse_attachment <- function(x){
  if(length(x) == 0){
    return(NULL)
  }
  parsed <- purrr::map(x, ~ tryCatch(
      jsonlite::fromJSON(.x),
      error = function(e) NULL))
  parsed <- parsed[!purrr::map_lgl(parsed, is.null)]
  if(length(parsed) == 0){
    return(NULL)
  }
  dates <- purrr::map_chr(parsed, ~ .x$attachmentPublicationDate)
  parsed[[which.max(as.Date(dates))]]
}
parse_attachment(attachments)
