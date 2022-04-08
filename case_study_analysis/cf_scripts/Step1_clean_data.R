
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)
library(googledrive)
library(googlesheets4)

# Directories
indir <- "case_study_analysis/cf_scripts/data/raw"
outdir <- "case_study_analysis/cf_scripts/data"
plotdir <- "case_study_analysis/cf_scripts/figures"

# Read files
files2merge <- list.files(indir) %>% sort()


# Merge data
################################################################################

# Column names
colnames_use <- c("dimension", "domain", "question_id", "attribute", "definition", "options", 
                  "score", "score_desc", "dont_know", "not_relevant", "data_quality", "importance", 
                  "mechanism", "mech_apply_yn", "mech_apply_notes", "notes", "references") # Only Kanae's has the reference column

# Loop through files and merge
x <- files2merge[1]
data_orig <- purrr::map_df(files2merge, function(x){
  
  # Read data
  fdata <- readxl::read_excel(file.path(indir, x), sheet="UPDATED_S4_ResilienceAttributes", na=c("NA")) %>% 
    # Add column names
    setNames(colnames_use) %>% 
    # Add filename
    mutate(filename=x) %>% 
    # Simplify
    select(filename, dimension, domain, attribute, score, score_desc, dont_know, not_relevant, 
           data_quality, importance, mech_apply_yn, mech_apply_notes, notes) %>% 
    # Reduce to attributes
    filter(!is.na(attribute) & dimension!="Dimensions")
  
})


# Format data
################################################################################

# To-do list
# Which files are up to date?
# Fix those lingering issues after reducing to only updated data

# Format data
data <- data_orig %>% 
  # Convert to numeric
  mutate(score=as.numeric(score)) %>% 
  # Format "don't know" column
  mutate(dont_know=recode(dont_know,
                          "Don't Know"="Don't know",
                          "Don't know 3?"="Don't know",
                          "Don't know about the Yellow tail."="Don't know",
                          "Don't Know; prob varies greatly by species and population size"="Don't know",
                          "Don't know:"="Don't know")) %>% 
  # Add author names
  mutate(authors=recode(filename, 
                        "Burden Pacific sardine case study.xlsx"="Burden",                                        
                        "Case Study Bering Sea.xlsx"="Hollowed",                                                    
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
                        "PRIMARY Updated Aguion_Barnacles_Case Study.xlsx"="Aguion",                              
                        "Primary_Updated_Aguion_Barnacles_Case Study.xlsx"="Aguion",                             
                        "Schmidt_case study template.xlsx"="Schmidt",                                         
                        "Tokunaga_JapaneseSpinyLobster_Case Study Template.xlsx"="Tokunaga",                   
                        "Updated Westfall_AtlanticandGulfPLLFishery_Case Study.xlsx"="Westfall",                
                        "Updated_Aguion_Barnacles_Case Study.xlsx"="Aguion",                                
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
                           "Hollowed"="Bering Sea groundfish",
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
  select(filename, authors, everything())
  
# Inspect
str(data)

# Inspect
table(data$authors)
table(data$case_study)
table(data$dimension)
table(data$domain)
unique(data$attribute)
table(data$dont_know)
table(data$not_relevant)
table(data$data_quality) # E-No data is not formatted consistently
table(data$importance) # "not sure" looks wrong
table(data$mech_apply_yn)
sort(unique(data$filename))


# Export data
################################################################################

# Export data
saveRDS(data, file=file.path(outdir, "case_study_attribute_score_data.Rds"))




