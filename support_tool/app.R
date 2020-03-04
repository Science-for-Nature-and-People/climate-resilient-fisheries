
# Clear
rm(list = ls())

# Setup
################################################################################

# Packages
library(shiny)
library(shinythemes)
library(tidyverse)
library(RColorBrewer)

# Read data
# datadir <- "data" # for actual app
datadir <- "support_tool/data" # when testing
codedir <- "code"
textdir <- "app_text"


# User interface
################################################################################

# User interface
ui <- navbarPage("Climate-resilient fisheries", position="fixed-top",
                 
  # Panels
  tabPanel("Overview"),
  tabPanel("Data"),
  tabPanel("Resilience checklist"), 
  tabPanel("Resilience enhancing tools")

  
)


# Server
################################################################################

# Server
server <- function(input, output){
  

}

shinyApp(ui = ui, server = server)
