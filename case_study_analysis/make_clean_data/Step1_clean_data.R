
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

# Column names
colnames_use <- c("dimension", "domain", "question_id", "attribute", "definition", "options", 
                  "score", "score_desc", "dont_know", "not_relevant", "quality", "importance", 
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
           quality, importance, mech_apply_yn, mech_apply_notes, notes) %>% 
    # Reduce to attributes
    filter(!is.na(attribute) & dimension!="Dimensions")
  
})


# Format data
################################################################################

# Format data
data <- data_orig %>% 
  # Convert to numeric
  mutate(score=as.numeric(score)) %>% 
  # Format dimension
  mutate(dimension=recode(dimension,
                          "Governance-management"="Governance",
                          "Social-economic"="Socio-economic")) %>% 
  # Format attribute
  mutate(attribute=recode(attribute,
                          "Flexible and agile infrstructure"="Flexible and agile infrastructure")) %>% 
  # Format importance
  mutate(importance=ifelse(importance=="not sure", NA, importance)) %>% 
  # Format data quality
  mutate(quality=recode(quality, 
                        "adequate data are available"="A - adequate and reliable data/information",
                        "adequate data is available"="A - adequate and reliable data/information",
                        "C - solely expert judgement; fairly confident"="C - fairly confident that the answer provided reflects the true state of the system",
                        "D - solely expert judgment; not confident"="D - not confident that the answer provided reflects the true state of the system",
                        "NA - Not relevant in this system"="NA",
                        "expert judgement, high confidence"="C - fairly confident that the answer provided reflects the true state of the system",
                        "E - No data/information; no basis for expert judgement"="E - No data"),
         quality=ifelse(quality=="NA", NA, quality)) %>% 
  # Format "don't know" column
  mutate(dont_know=recode(dont_know,
                          ":"="Don't know",
                          "Don't Know"="Don't know",
                          "Don't know 3?"="Don't know",
                          "Don't know about the Yellow tail."="Don't know",
                          "Don't Know; prob varies greatly by species and population size"="Don't know",
                          "Don't know:"="Don't know")) %>% 
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
                        "Yuga and Gaku SNAPP Case Study_.xlsx"="Yuga/Gaku",
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
                           "Aguion"="Galicia stalked barnacle",
                           "Burden"="US West Coast Pacific sardine",
                           "Dickey-Collas"="NE Atlantic small pelagics",
                           "Eurich"="Kiribati giant clam",
                           "Free"="California Dungeness crab",
                           "Mangubhai/Friedman"="Fiji nearshore",
                           "Golden"="Madagascar nearshore",
                           "Hollowed/Kasperski"="Alaska Bering Sea groundfish",
                           "Yuga/Gaku"="Hokkaido set-net",
                           "Kleisner/Cunningham"="Juan Fernandez Islands demersal",
                           "Lau"="Madang reef fish",
                           "Mason"="Iceland groundfish",
                           "Mills"="Maine American lobster",
                           "Pecl"="Tasmania rock lobster",
                           "Schmidt"="Senegal small pelagics",
                           "Tokunaga"="Mie spiny lobster",
                           "Westfall"="US Atlantic and Gulf migratory pelagics",
                           "Zhao"="Moorea reef fish")) %>% 
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
table(data$not_relevant) # something bizarro here
table(data$quality)
table(data$importance)
table(data$mech_apply_yn)
sort(unique(data$filename))


# Export data
################################################################################

# Export data
saveRDS(data, file=file.path(outdir, "case_study_attribute_score_data.Rds"))

# For Meghan's data - Chris, I was getting an error running it as is
# save(data, file = here("case_study_analysis","mf_clustering_analysis","data","case_study_attribute_score_data.Rdata"))
# data %>% write_csv(here("case_study_analysis","mf_clustering_analysis","data","case_study_attribute_score_data.csv"))


