# List of packages being used
list.of.packages <- c("tidyverse","rvest","xml2","httr","DBI","RSQLite",
                      "janitor","lubridate","stringr","glue")

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

suppressPackageStartupMessages({
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
})

options(scipen=999) # Remove scientific notation

############################################################################# 
#APP THEME (on-screen)
#############################################################################
theme_app <- theme_bw() +
  theme(axis.title   = element_text(size=18, family="sans", colour="black"),
        axis.text.x  = element_text(size=14, family="sans", colour="black"),
        axis.text.y  = element_text(size=14, family="sans", colour="black"),
        strip.text   = element_text(size=15, family="sans", colour="black"),
        plot.title   = element_text(size=20, family="sans", colour="black"),
        panel.border = element_rect(colour="grey60", linewidth = 0.5),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "grey90",linewidth = 0.4),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.text = element_text(size = 14),
        legend.key.width = unit(1.5, "cm"))

# Apply it app-wide
theme_set(theme_app)

#############################################################################
#EXPORT THEME (smaller text)
#############################################################################
theme_export <- theme_bw() +
  theme(axis.title   = element_text(size=11,   family="sans", colour="black"),
        axis.text.x  = element_text(size=9,    family="sans", colour="black"),
        axis.text.y  = element_text(size=9,    family="sans", colour="black"),
        strip.text   = element_text(size=10,   family="sans", colour="black"),
        plot.title   = element_text(size=12,   family="sans", colour="black"),
        panel.border = element_rect(colour="black"),
        legend.position = "bottom",
        legend.text = element_text(size = 9),
        legend.key.width = grid::unit(1.2, "cm"))