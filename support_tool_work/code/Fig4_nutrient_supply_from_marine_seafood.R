
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)
library(countrycode)

# Directories
plotdir <- "support_tool_work/figures"
genusdir <- "/Users/cfree/Dropbox/Chris/UCSB/projects/nutrient_endowment/data/genus/processed/"
appdatadir <- "support_tool/data"

# Read GENUS data
nut_cntry_yr_orig <- readRDS(file.path(genusdir, "genus_nutrient_supplies_by_cntry_year.Rds"))
nut_cntry_age_sex_2011_orig <- readRDS(file.path(genusdir, "genus_nutrient_supplies_by_age_sex_2011.Rds"))
food_cntry_yr_orig <- readRDS(file.path(genusdir, "genus_edible_food_by_cntry_year.Rds"))
nut_cntry_food_2011_orig <- readRDS(file.path(genusdir, "genus_nutrient_supplies_by_cntry_food_2011.Rds"))


# Build data
################################################################################

# All food groups
food_groups <- food_cntry_yr_orig %>% 
  select(food) %>% 
  unique()

# Marine food groups
marine_seafood_groups <- c("Cephalopods", 
                           "Crustaceans", 
                           "Molluscs; Other",
                           "Demersal Fish", 
                           "Pelagic Fish",
                           "Marine Fish; Other",
                           "Fish; Body Oil",
                           "Fish; Liver Oil")

# Other aquatic food groups
other_seafood_groups <- c("Aquatic Plants", 
                          "Aquatic Animals; Others", 
                          "Freshwater Fish")

# Build data
pdiet_seafood_cntry_yr <- food_cntry_yr_orig %>% 
  # Classify food group as seafood or not-seafood
  mutate(seafood=ifelse(food %in% marine_seafood_groups, "yes", "no")) %>% 
  # Summarize contribution of seafood vs. non-seafood
  group_by(iso3_use, country_use, year) %>% 
  summarize(total_g_person_day=sum(g_person_day, na.rm=T),
            seafood_g_person_day=sum(g_person_day[seafood=="yes"], na.rm=T),
            total_g_person_day=ifelse(total_g_person_day==0, NA, total_g_person_day),
            seafood_g_person_day=ifelse(total_g_person_day==0, NA, seafood_g_person_day)) %>% 
  mutate(prop_seafood=seafood_g_person_day/total_g_person_day) %>% 
  ungroup()

# Example plot
cntry <- "Colombia"
g <- ggplot(pdiet_seafood_cntry_yr %>% filter(country_use==cntry), aes(x=year, y=prop_seafood)) +
  geom_line() +
  labs(x="", y="Proportion of diet\nfrom marine seafood") +
  theme_bw()
g  

# Build data: proportion of nutrients from seafood
################################################################################

# Build data
pnutrient_seafood_cntry_2011 <- nut_cntry_food_2011_orig %>% 
  # Classify food group as seafood or not-seafood
  mutate(seafood=ifelse(food %in% marine_seafood_groups, "yes", "no")) %>% 
  # Summarize contribution of seafood vs. non-seafood
  group_by(iso3_use, country_use, nutrient, units_long, units_short) %>% 
  summarize(total_amt_person_day=sum(value_med, na.rm=T),
            seafood_amt_person_day=sum(value_med[seafood=="yes"], na.rm=T)) %>% 
  mutate(prop_seafood=seafood_amt_person_day/total_amt_person_day) %>% 
  ungroup()


# Example plot
cntry <- "Ghana"
g <- ggplot(pnutrient_seafood_cntry_2011 %>% filter(country_use==cntry), 
            aes(x=reorder(nutrient, prop_seafood), y=prop_seafood)) +
  geom_bar(stat="identity") +
  labs(x="", y="Proportion of nutrient intake\nfrom marine seafood") +
  coord_flip() +
  theme_bw()
g 

# Export data
saveRDS(pnutrient_seafood_cntry_2011, 
        file=file.path(appdatadir, "GENUS_2011_pnutrient_seafood_by_country.Rds"))
saveRDS(pdiet_seafood_cntry_yr, 
        file=file.path(appdatadir, "GENUS_1960_2011_pdiet_seafood_by_country.Rds"))

# Build plot
################################################################################

cntry <- "Colombia"

# Theme
my_theme <- theme(axis.text=element_text(size=8),
                  axis.title=element_text(size=10),
                  plot.title=element_text(size=10),
                  legend.text=element_text(size=6))


# Seafood consumption time series
g1 <- ggplot(pdiet_seafood_cntry_yr %>% filter(country_use==cntry),
             aes(x=year, y=seafood_g_person_day)) +
  geom_line() +
  labs(x="Year", y="Daily per capita marine seafood supply\n(grams per person per day)") +
  theme_bw() + my_theme
g1  

# Importance to nutrient intake
g2 <- ggplot(pnutrient_seafood_cntry_2011 %>% filter(country_use==cntry), 
            aes(x=reorder(nutrient, prop_seafood), y=prop_seafood)) +
  geom_bar(stat="identity") +
  labs(x="", y="Percent of nutrient intake\nfrom marine seafood") +
  scale_y_continuous(labels = scales::percent) +
  # scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  coord_flip() +
  theme_bw() + my_theme
g2 

# Merge plots
g <- gridExtra::grid.arrange(g1, g2, ncol=2)

# Export plot
ggsave(g, filename=file.path(plotdir, "Fig4_nutrient_supply_from_marine_seafood.png"), 
       width=6.5, height=3, units="in", dpi=600)
