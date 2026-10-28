################################################################################
# Canadian Species at Risk Explorer
# Launch Application
# Purpose:
# Starts the Shiny application.
################################################################################

#install.packages('shiny')
#install.packages('dplyr')
#install.packages('DT')
#install.packages('bslib')

library(shiny)
library(dplyr)
library(DT)
library(bslib)

runApp("app/app.R")
