# if you do not have these packages installed on your computer, you need to remove
# the hashtags and install them first, the load them using library()

#install.packages('shiny')
#install.packages('dplyr')
#install.packages('DT')
#install.packages('shinydashboard')
#install.packages('bslib')

library(shiny)
library(dplyr)
library(DT)
library(shinyDashboard)
library(bslib)

runApp("app/app.R")
