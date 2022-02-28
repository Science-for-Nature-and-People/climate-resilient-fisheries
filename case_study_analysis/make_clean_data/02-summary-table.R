# read and format case study summary table
library(here)
source(here("case_study_analysis","00-load-libraries.R"))

case_summaries <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/10ZcIdYASKeE-GKs7zC37YC4gANjNgFNb2mhBLptgGyU/edit#gid=0",
                             sheet = 1,
                             range = "A2:Z19",
                             col_names = TRUE,
                             na = c("NA","na","NAN","nan",""),
                             guess_max = 10000,
                             trim_ws = TRUE,
                             col_types = "c") %>%
  as_tibble() %>%
  clean_names() %>% 
  select(case_study, case_study_author, fishery_scale_small_or_large, fishery_location) %>% 
  rename(case_study_desc = case_study, 
         case_study = case_study_author) %>% 
  mutate(case_study = str_replace(case_study, "-", "_"))

case_summaries %>% 
  write_csv(here("case_study_analysis","clean_data","case_summaries.csv"))
