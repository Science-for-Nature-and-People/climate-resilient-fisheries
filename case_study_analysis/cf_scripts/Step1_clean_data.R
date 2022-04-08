
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

# Add author names
# Add case study names
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
                          "Don't know:"="Don't know"))

# Inspect
str(data)

# Inspect
table(data$dimension)
table(data$domain)
unique(data$attribute)
table(data$dont_know)
table(data$not_relevant)
table(data$data_quality) # E-No data is not formatted consistently
table(data$importance) # "not sure" looks wrong
table(data$mech_apply_yn)



# Export data
################################################################################

# Export data
saveRDS(data, file=file.path(outdir, "case_study_attribute_score_data.Rds"))




