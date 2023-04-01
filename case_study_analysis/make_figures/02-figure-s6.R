# 02-figure-s6.R
# Code to create figure S6
# W. Friedman // 2023

# Setup
################################################################################

# Clear workspace
rm(list = ls())

# Load libraries
source(here("case_study_analysis","make_figures","00-load-libraries.R"))
library(ggbeeswarm) # install.packages("ggbeeswarm")

# Directories
plotdir <- here("case_study_analysis","make_figures", "plots")

# Build data
source(here("case_study_analysis","make_figures","01-build-data.R"))

# Plot data
################################################################################

# Make figure s6 ----

group1 <- case_studies_ordered[1:3]
group2 <- case_studies_ordered[4:7]
group3 <- case_studies_ordered[8:11]
group4 <- case_studies_ordered[12:14]
group5 <- case_studies_ordered[15:18]

attrib_by_dim <- data %>% 
  select(dimension, attribute) %>% 
  distinct() %>% 
  arrange(dimension, attribute)

data_ord <- data %>% 
  mutate(group = case_when(case_study %in% group1 ~ '1',
                           case_study %in% group2 ~ '2',
                           case_study %in% group3 ~ '3',
                           case_study %in% group4 ~ '4',
                           case_study %in% group5 ~ '5')) %>% 
  mutate(attribute = factor(attribute, levels= attrib_by_dim$attribute)) %>% 
  group_by(dimension, case_study) %>% 
  mutate(mean_score = mean(score, na.rm=T))


case_palette = (qualpal(18, colorspace=list(h=c(0,360), s=c(0.3,1), l=c(0.2,0.8))))$hex

# FIGURE S6: 
data_ord %>% 
  ggplot(aes(x = group, y = score, colour = case_study))+
  geom_boxplot(alpha = 1, fill = "gray95", colour = "gray60", outlier.shape = NA, lwd = 0.3)+
  geom_beeswarm(cex = .6, alpha = 0.5, shape = 21, size = 1, 
                fill = "gray60", colour = "gray40", show.legend = F, lwd = 0.1) +
  geom_point(aes(x=group, y = mean_score, fill = case_study, shape = case_study), colour = "gray30",
             size = 2.5, lwd = 0.01, alpha = 1)+
  scale_fill_manual(values = case_palette, name = "Case study")+
  scale_shape_manual(values = rep(seq(21,25),4), name = "Case study")+
  facet_wrap(~dimension, ncol = 3)+
  theme_bw()+
  theme(legend.position = "right")


ggsave(here(plotdir,"Figure_S6_score_boxplot.png"), 
       height = 5, width = 10)

