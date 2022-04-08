
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(ggplot2)
library(tidyverse)

# Directories
datadir <- "case_study_analysis/cf_scripts/data"
plotdir <- "case_study_analysis/cf_scripts/figures"

# Read data
data_orig <- read.csv(file.path(datadir, "attribute_table_clean.csv"), as.is=T)


# Build data
################################################################################

# Domain attribute key
att_key <- data_orig %>% 
  select(dimension, domain, attribute) %>% 
  unique()

# Format data
data <- data_orig %>% 
  # Recode case studies
  mutate(case_study=recode(case_study,
                           "Aguion"="Galacia stalked barnacles",
                           "Burden"="US West Coast Pacific sardine",
                           "Dickey_Collas"="NE Atlantic pelagic",
                           "Eurich"="Kiribati giant clam",
                           "Free"="California Dungeness crab",
                           "Friedman"="Fiji nearshore",
                           "Golden"="Madagascar reef fish",
                           "Hollowed"="Bering Sea groundfish",
                           "Kisara"="Japan common squid",
                           "Kleisner"="Juan Fernandez Islands",
                           "Lau"="Madang reef fish",
                           "Mason"="Iceland groundfish",
                           "Mills"="Maine lobster",
                           "Pecl"="Tasmania rock lobster",
                           "Tokunaga"="Japanese spiny lobster",
                           "Westfall"="US Atlantic pelagic longline",
                           "Zhao"="Moorea coral reef")) %>% 
  # Simplify
  select(case_study, attribute, score) %>% 
  # Expand for all attributes
  complete(attribute, case_study, fill=list(score=NA)) %>% 
  # Add dimension/domian
  left_join(att_key, by="attribute")

# Average score by case study
stats_case <- data %>% 
  group_by(case_study) %>% 
  summarize(score_avg=mean(score, na.rm=T)) %>% 
  ungroup() %>% 
  arrange(score_avg)

# Average score by case study
stats_attr <- data %>% 
  group_by(dimension, attribute) %>% 
  summarize(score_avg=mean(score, na.rm=T)) %>% 
  ungroup() %>% 
  # Arrange
  arrange(dimension, score_avg)

# Order data
data1 <- data %>% 
  # Order case studies and attributes
  mutate(attribute=factor(attribute, levels=stats_attr$attribute),
         case_study=factor(case_study, levels=stats_case$case_study)) %>% 
  # Order score
  mutate(score=as.character(score), 
         score=ifelse(is.na(score), "NA", score),
         score=recode(score, "NA"="Not provided"),
         score=factor(score, levels=c("Not provided", "1", "2", "3", "4")))

# Number of case studies
n_cases <- n_distinct(data$case_study)

# Importance stats
stats_score <- data1 %>% 
  # Summarize
  group_by(dimension, attribute, score) %>% 
  summarise(n=n()) %>% 
  ungroup() %>% 
  # Calculate prop
  mutate(prop=n/n_cases)


# Plot data
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.title=element_text(size=8),
                   plot.tag=element_text(size=8),
                   axis.title.y=element_blank(),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=8),
                   strip.text=element_text(size=8),
                   plot.title=element_text(size=10),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.background = element_rect(fill=alpha('blue', 0)))


# Plot data
g1 <- ggplot(data1, aes(y=attribute, x=case_study, fill=score, size=importance)) +
  facet_grid(dimension~., space="free_y", scale="free_y") +
  geom_point() +
  # Labels
  labs(x="", y="", tag="A") +
  # Legend
  scale_fill_manual(name="Score", values=c("grey80", RColorBrewer::brewer.pal(4, "Blues"))) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
g1 

# Plot data
g2 <- ggplot(stats_score, aes(y=factor(attribute, levels=stats_attr$attribute), x=prop, fill=score)) +
  facet_grid(dimension~., space="free_y", scale="free_y") +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Proportion of case studies\n \n \n \n ", y="", tag="B") +
  # Legend
  scale_fill_manual(name="Score", values=c("grey80", RColorBrewer::brewer.pal(4, "Blues"))) +
  guides(fill = guide_legend(title.position="top")) +
  # Theme
  theme_bw() + my_theme +
  theme(axis.text.y=element_blank(),
        legend.key.size = unit(0.3, "cm"),
        legend.position="bottom")
g2

# Merge data
g <- gridExtra::grid.arrange(g1, g2, ncol=2, widths=c(0.7, 0.3))
g

# Export plot
ggsave(g, filename=file.path(plotdir, "Fig4_attribute_scores.png"), 
       width=6.5, height=5, units="in", dpi=600)

