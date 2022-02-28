# 00-load-libraries.R
# Loads all libraries used in clustering analysis for SNAPP-CRF
# case study clustering and heatmpa analyses
# M. Fletcher 2/15/2022


library(tidyverse)
library(janitor)
library(here)
library(googlesheets4)
library(kableExtra)
library(broom)
library(ggpubr)
library(psych)
library(corrplot)

# Plot colors
library(viridis)
library(RColorBrewer)

# For hiearchcial clustering
library(ggdendro)
library(ggfortify)
library(cluster)

# To use melt function
library(reshape2)

# For graph aesthetics
library(viridis)
library(plotly)
library(patchwork)

# For heatmaps
library(grid)
library(ComplexHeatmap)