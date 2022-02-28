# 02-standardize-scores.R
# Reads in attribute table
# Produces an attribute table with standardized scores. 
# 
# Within each case, and within each dimension; what are the high and low “ranking” values. Not the absolute score.
# ! NOT CURRENTLY IN USE.
# W. Friedman 2/7/2022

library(here)
source(here("00-load-libraries.R"))
load(here("data","attribute_table_clean.Rdata")) # from 01-prep-attribute-tables.Rmd


# within each CASE_STUDY, within each DIMENSION, create ranked scores
# values are a measure of relative attribute within that particular system
attribs_std <- attribs %>% group_by(case_study, dimension) %>% 
  mutate(score_std = scale(score)) %>% 
  ungroup() %>% 
  select(dimension:reference,attribute_id, score, score_std) %>% 
  clean_names()


#ggplot(attribs_std, aes(x= score, y = score_std, color = dimension)) + 
#  geom_point()
