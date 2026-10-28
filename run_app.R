################################################################################
# Canadian Species at Risk Explorer
# Launch Application
# if you don't have the shiny, dplyr, DT, and bslib packages already on your 
# computer, you'll need to remove the hashtags and install those packages for
# the first time using the app. Then, you run the four codes starting with 
# 'library' to load those packages. After that, you run the runApp function to 
# open the application.
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
