# 00-load-libraries.R
# Loads common libraries used in SNAPP-CRF
# case study analysis
# W. Friedman 2/7/2022

library(here)
library(ggplot2)
library(RColorBrewer)
library(janitor)
library(googlesheets4)
library(kableExtra)
library(broom)
library(ggpubr)
library(psych)
library(corrplot)
library(cluster)
library(viridis)
library(gridExtra)
library(conflicted)
library(tidyverse)

conflict_prefer("select", "dplyr") # (need to use raster::select for rasters)
conflict_prefer("filter", "dplyr")
conflict_prefer("combine", "dplyr")






