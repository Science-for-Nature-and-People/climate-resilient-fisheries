
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
datadir <- "data" # for actual app
# datadir <- "support_tool/data" # when testing
codedir <- "code"
textdir <- "app_text"


# User interface
################################################################################

# User interface
ui <- navbarPage("Climate-resilient fisheries",
                 
  # Overview
  tabPanel("Overview",
           mainPanel(includeHTML(file.path("app_text", "01_overview.html")))),
  
  # Data portal
  navbarMenu("Data portal",
             tabPanel("Oceanography"),
             tabPanel("Fisheries"), 
             tabPanel("Other indicators")),
  
  # Resilience checklist
  tabPanel("Resilience checklist"), 
  
  # Resilience enhancing tools
  tabPanel("Resilience enhancing tools")

  
)


# Server
################################################################################

# Server
server <- function(input, output){
  

}

shinyApp(ui = ui, server = server)
