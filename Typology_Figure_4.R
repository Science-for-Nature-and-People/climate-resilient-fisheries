
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(ggplot2)
library(tidyverse)
library(here)
# Directories
datadir <- "case_study_analysis/clean_data"
plotdir <- "case_study_analysis/lz_scripts"

# Read data
data_orig <- readRDS(file.path(datadir, "case_study_attribute_score_data.Rds"))


# Build data for typology
################################################################################

data <- data_orig %>% 
  # Simplify and convert importance to numeric
  select(case_study, dimension, attribute, score, importance) %>% 
  mutate(importance=stringr::str_to_sentence(importance),
         importance=factor(importance, levels=c("Low", "Medium", "High"))) %>% 
  mutate(importance_numeric= as.numeric(as.factor(importance)))


# Calculate average attribute score, variance  and average attribute importance and variance 

score_average <- data %>%
  group_by(dimension, attribute) %>% 
  summarize(score_avg=mean(score, na.rm=T)) %>% 
  ungroup() 

score_variance <- data %>%
  group_by(dimension, attribute) %>% 
  summarize(score_var=(var(score, na.rm=T))) %>% 
  ungroup()

importance_average <- data %>%
  group_by(dimension, attribute) %>% 
  summarize(importance_avg=mean(importance_numeric, na.rm=T)) %>% 
  ungroup()  

importance_variance<- data %>% 
  group_by(dimension, attribute) %>% 
  summarize(importance_var=var(importance_numeric, na.rm=T)) %>% 
  ungroup()

#combine into one cross-case dataset
typology_data <- full_join(score_average, score_variance)
typology_data<- full_join(typology_data, importance_average)
typology_data<- full_join(typology_data, importance_variance)


#add precision columns where precision = 1/ variance


typology_data<- typology_data %>% 
  rename(Dimension= dimension) %>% 
  mutate(precision_score = (1/score_var)) %>% 
  mutate(precision_importance = (1/importance_var)) %>%  arrange(precision_importance) %>% 
  #if attribute is in position 1 through 9 (of low precision it is in the upper quartile importance variability for the 38 attributes) 
  mutate(importance_case_dependency = ifelse(precision_importance > 1.70, "not particularly case-dependent", "upper-quartile case-dependent"))

write_csv(typology_data, here("case_study_analysis", "lz_scripts", "typology_data.csv"))





#######################################################3
#create theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.title=element_text(size=7),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=7),
                   strip.text=element_text(size=7),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.position="bottom",
                   legend.background = element_rect(fill=alpha('blue', 0)))

mean(typology_data$importance_avg) #2.323099
mean(typology_data$score_avg)#2.843567


################ add overall average score for attributes and overall average importance for attributes to figure. 

quad<- ggplot(data = typology_data, aes(x = importance_avg, y = score_avg, size = precision_importance)) + 
  geom_point(color='black', shape=21, aes(fill=Dimension), alpha=.75) + 
  scale_fill_manual(values = c( "Ecological" = "#72B077", "Governance" = "#C25866", "Socio-economic" = "#D6B65D")) + 
  scale_size_continuous(range = c(2, 10)) +  scale_x_continuous(limits = c(1.5, 3), breaks = c(2, 2.32, 3))+ scale_y_continuous(limits = c(2, 3.8), breaks = c(2, 2.84, 3))+
  #geom_text(aes(label = attribute), size = 2, vjust = 1, hjust = .5) +  
  theme_bw() + my_theme +  guides(size = "none")+
    xlab("Cross-case average attribute importance") +
  ylab("Cross-case average attribute score") +
  geom_hline(yintercept = mean(typology_data$score_avg), linetype = 'dashed', col = 'grey') + geom_vline(xintercept = mean(typology_data$importance_avg), linetype = 'dashed', col = 'grey') +
  theme(panel.background = element_rect(fill='transparent'), #transparent panel background
        plot.background = element_rect(fill='transparent', color=NA))

#add back in geom_text(aes(label = attribute), size = 2, vjust = 1, hjust = .5) to see attribute labels. Version here saved without labels and labels added in manually afterwards 
ggsave(quad, filename=file.path(plotdir, "typology_without_labels.png"), 
       width=5.5, height=4.5, units="in", dpi=600)
