# 01-build-data.R
# Script that loads and formats data for analysis
# W. Friedman // 2023

# Setup
################################################################################

# Clear workspace
# rm(list = ls())

# Load libraries
source(here("case_study_analysis","make_figures","00-load-libraries.R"))

# Directories 
datadir <- here("case_study_analysis","clean_data")

# Read data
data_orig <- readRDS(file.path(datadir, "case_study_attribute_score_data.Rds"))

# Build data
data <- data_orig %>% 
  # Simplify
  select(case_study, dimension, attribute, score, importance) %>% 
  # Scale scores by attribute
  group_by(attribute) %>% 
  mutate(score_scaled=scale(score) %>% as.numeric()) %>% 
  ungroup() %>% 
  mutate(attrib_dim = str_trunc(dimension,1,ellipsis=""), 
         attrib_dim = paste(attrib_dim, attribute, sep = ":")) %>% 
  mutate(importance_numeric = recode(importance, 
                                     "high" = 3,
                                     "medium" = 2,
                                     "low" = 1))

# Convert data to matrix
data_mat <- data %>% 
  # Simplify
  select(case_study, attribute, score_scaled) %>% 
  # Spread
  spread(key = attribute, value = score_scaled) %>% 
  # Make a matrix by taking case study to column name
  column_to_rownames(var = "case_study") %>% 
  # Convert to matrix
  as.matrix() 

# Perform cluster analysis
clusters <- data_mat %>%     
  # Calculate distance matrix
  stats::dist() %>% 
  # Calculate cluster
  stats::hclust(method = "ward.D2")

# Attribute order
attr_order <- data %>% 
  group_by(dimension, attribute) %>% 
  summarize(score_avg=mean(score)) %>% 
  ungroup() %>% 
  arrange(dimension, score_avg)

# Order data
case_studies_ordered <- clusters$labels[clusters$order]
data_ordered <- data %>% 
  # Order case studies
  mutate(case_study=factor(case_study, levels=case_studies_ordered))  %>% 
  # Order attributes
  mutate(attribute=factor(attribute, levels=attr_order$attribute))
