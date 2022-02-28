# 01-load-data.R
# Example script for getting started
library(here) 
source(here("case_study_analysis","00-load-libraries.R")) 

load(here("case_study_analysis", "clean_data","attribute_table_clean.Rdata"))
case_summaries <- read_csv(here("case_study_analysis","clean_data","case_summaries.csv"))