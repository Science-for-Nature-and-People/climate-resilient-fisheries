# get climate related answers from case study templates
library(tidyverse)
library(here)

# read clean data file
# ! note that when people scored F = 'f' it reads in as FALSE. 

dat <- read_csv(here("case_study_analysis","clean_data","case_study_contextual_descriptions.csv"))
dat

# 2.6.4.1.
"Which of the following climate disturbances are projected to alter the future of the fishery and surrounding ecosystem?
A. Ocean warming
B. Ocean acidification
C. Frequency and/or severity of coral bleaching
D. Frequency and/or severity of marine heatwaves
E. Frequency and/or severity of extreme El Nino-Southern Oscillation events
F. Frequency and/or severity of large storm events
G. Ocean cooling
H. Loss of sea ice
I. Sea level rise
J. Increase or decrease in upwelling
K. Changes in ocean current patterns
L. Other (please specify):"

climate <- dat %>% 
  filter(question_id == "2.6.4.1.") %>% 
  select(case_study, A:L) %>% 
  mutate(across(A:L, ~as.character(.))) %>% 
  pivot_longer(!case_study, names_to = "response") %>% 
  mutate(value_new = if_else(!is.na(value), 1, 0)) %>% 
  mutate(response_new = recode(response, 
                               "A" = "ocean_warming", 
                               "B" = "ocean_acidification",
                               "C" = "coral_bleaching",
                               "D" = "marine_heatwaves",
                               "E" = "ENSO",
                               "F" = "storms",
                               "G" = "ocean_cooling",
                               "H" = "sea_ice_loss",
                               "I" = "sea_level_rise",
                               "J" = "upwelling",
                               "K" = "ocean_currents",
                               "L" = "other")) %>% 
  select(case_study, response_new, value_new) %>% 
  pivot_wider(values_from = "value_new", names_from = "response_new")

climate
climate %>% filter(response == "F")

climate %>% write_csv(here("case_study_analysis","clean_data",
                           "q2_6_4_1_shocks.csv"))
