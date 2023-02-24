
# Packages
library(ggplot2)
library(tidyverse)
library(here)
library(janitor)
# Directories
datadir <- "case_study_analysis/clean_data"
plotdir <- "case_study_analysis/lz_scripts"

# Read data
data_orig <- readRDS(file.path(datadir, "case_study_attribute_score_data.Rds"))


# Build data for typology
################################################################################

data <- data_orig %>% 
  # Simplify and convert importance to numeric
  select(case_study, domain, dimension, attribute, score, importance) %>% 
  mutate(importance=stringr::str_to_sentence(importance),
         importance=factor(importance, levels=c("Low", "Medium", "High"))) %>% 
  mutate(importance_numeric= as.numeric(as.factor(importance)))



# within each CASE_STUDY, within each DIMENSION, create ranked importance
# values are a measure of relative attribute within that particular system
data<- data %>% group_by(case_study, dimension) %>% 
  mutate(imp_stand = scale(importance_numeric)) %>% 
  ungroup() 
#clean column name
data$imp_stand<- as.numeric(data$imp_stand)

# Calculate average attribute score, variance  and average attribute importance and variance 

median_scaled_importance_df <- data %>%
  group_by(domain, dimension, attribute) %>% 
  summarize(imp_stand_median=median(imp_stand, na.rm=T)) %>% 
  ungroup() 

 S1<- ggplot(data = median_scaled_importance_df, aes(x = domain, y = imp_stand_median)) + geom_boxplot()+
  geom_point(color='black', shape=21, aes(fill=domain), alpha=.75) + 
  #scale_fill_manual(values = c( "Ecological" = "#72B077", "Governance" = "#C25866", "Socio-economic" = "#D6B65D")) + 
 # scale_size_continuous(range = c(2, 10)) +  scale_x_continuous(limits = c(1.5, 3), breaks = c(2, 2.32, 3))+ scale_y_continuous(limits = c(2, 3.8), breaks = c(2, 2.84, 3))+
  geom_text(aes(label = attribute), size = 2, vjust = .5, hjust = 1.1) +  
  theme_bw() + my_theme +  guides(size = "none")+
  xlab("domain") +
  ylab("median scaled numeric importance no jitter") +
 geom_hline(aes(yintercept= 0))
#add back in geom_text(aes(label = attribute), size = 2, vjust = 1, hjust = .5) to see attribute labels. Version here saved without labels and labels added in manually afterwards 
ggsave(S1, filename=file.path(plotdir, "S1.png"), 
       width=6, height=3, units="in", dpi=600)



