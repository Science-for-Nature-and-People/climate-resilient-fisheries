
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)
library(googledrive)
library(googlesheets4)

# Directories
indir <- "case_study_analysis/make_clean_data/raw"
outdir <- "case_study_analysis/clean_data"

# Read files
files2merge <- list.files(indir) %>% sort()


# Merge data
################################################################################

# Column name
col_names <- c("topic", "subsection", "question_id", "question", LETTERS[1:12], "other", "description", "dont_know", "not_relevant", "info_quality", "notes")

# Loop through files and merge
x <- files2merge[3]
data_orig <- purrr::map_df(files2merge, function(x){
  
  # Read data
  fdata_orig <- readxl::read_excel(file.path(indir, x), sheet="S2_ContextualDescription", na=c("NA"))
  
  # Number of columns and column names
  ncols <- ncol(fdata_orig)
  if(ncols==length(col_names)){
    col_names_use <- col_names
  }else{
    col_names_use <- c(col_names, rep("extra", ncols-length(col_names)) %>% make.unique())
  }
  
  # Format data
  fdata <- fdata_orig %>% 
    # Column name
    setNames(col_names_use) %>% 
    # Remove header
    slice(10:nrow(.)) %>% 
    # Add filename
    mutate(filename=x) %>% 
    # Arrange
    select(filename, everything())
  
})

# Format data
################################################################################

# Format data
data <- data_orig %>% 
  # Add author names
  mutate(authors=recode(filename,           
                        "Case Study Bering Sea.xlsx"="Hollowed/Kasperski",                                                    
                        "Chris Golden_Case Study Rubric.xlsx"="Golden",                                           
                        "Dickey-Collas_EUR_case study template.xlsx"="Dickey-Collas",                                    
                        "Eurich_KiribatiClam_Case_Study.xlsx"="Eurich",                                           
                        "Free_Dungeness_crab.xlsx"="Free",                                                      
                        "Kleisner_Cunningham_JuanFernandez_Case Study Template.xlsx"="Kleisner/Cunningham",                    
                        "Lau_PNG_Madang_Reef_Case Study Template.xlsx"="Lau",                                  
                        "Mangubhai_Friedman_Fiji_Case_Study.xlsx"="Mangubhai/Friedman",                                       
                        "Mar 19_Copy of Yuga and Gaku@Japan SNAPP Case Study  - March 19, 10_03 AM.xlsx"="Yuga/Gaku",
                        "Mason_Iceland_groundfish_Case Study Template.xlsx"="Mason",                             
                        "Mills_Lobster_Case Study Template.xlsx"="Mills",                                        
                        "Pecl_Tasmania rock lobster_case study template.xlsx"="Pecl",                           
                        "PRIMARY Burden Pacific sardine case study.xlsx"="Burden",                               
                        "Primary_Updated_Aguion_Barnacles_Case Study.xlsx"="Aguion",                             
                        "Schmidt_case study template.xlsx"="Schmidt",                                         
                        "Tokunaga_JapaneseSpinyLobster_Case Study Template.xlsx"="Tokunaga",                   
                        "Updated Westfall_AtlanticandGulfPLLFishery_Case Study.xlsx"="Westfall",                
                        "Zhao_Moorea_coral_reef_fishery_Case_Study.xlsx"="Zhao")) %>% 
  # Add case studies
  mutate(case_study=recode(authors,
                           "Aguion"="Galacia stalked barnacles",
                           "Burden"="US West Coast Pacific sardine",
                           "Dickey-Collas"="NE Atlantic pelagic",
                           "Eurich"="Kiribati giant clam",
                           "Free"="California Dungeness crab",
                           "Mangubhai/Friedman"="Fiji nearshore",
                           "Golden"="Madagascar reef fish",
                           "Hollowed/Kasperski"="Bering Sea groundfish",
                           "Yuga/Gaku"="Japan common squid",
                           "Kleisner/Cunningham"="Juan Fernandez Islands",
                           "Lau"="Madang reef fish",
                           "Mason"="Iceland groundfish",
                           "Mills"="Maine lobster",
                           "Pecl"="Tasmania rock lobster",
                           "Schmidt"="Senegalese small pelagics",
                           "Tokunaga"="Japanese spiny lobster",
                           "Westfall"="US Atlantic pelagic longline",
                           "Zhao"="Moorea coral reef")) %>% 
  # Arrange
  select(filename, case_study, authors, everything())

# Inspect
table(data$authors)
table(data$case_study)


# Export data
################################################################################

# Export data
write.csv(data, file=file.path(outdir, "case_study_contextual_descriptions.csv"), row.names=F)


