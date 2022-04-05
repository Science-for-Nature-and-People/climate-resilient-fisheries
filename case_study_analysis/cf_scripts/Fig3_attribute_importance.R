
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

n_cases <- n_distinct(data_orig$case_study)

# Importance stats
stats_imp <- data_orig %>% 
  # Complete importance
  complete(attribute, case_study, fill=list(importance="Not provided")) %>%
  # Fill dimensions
  group_by(attribute) %>% 
  mutate(dimension=dimension %>% na.omit() %>% unique()) %>% 
  # Summarize
  group_by(domain, dimension, attribute, importance) %>% 
  summarise(n=n(),) %>% 
  ungroup() %>% 
  # Calculate prop
  mutate(prop=n/n_cases) %>% 
  # Order importance
  mutate(importance=factor(importance, levels=c("Not provided", "Low", "Medium", "High")))

# Average importance by attribute
stats_imp_ord <- stats_imp %>% 
  filter(importance=="High") %>% 
  arrange(dimension, prop)


# Plot data
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.title=element_text(size=7),
                   axis.title.y=element_blank(),
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

# Plot data
g <- ggplot(stats_imp, aes(y=factor(attribute, levels=stats_imp_ord$attribute), x=prop, fill=importance)) +
  facet_grid(dimension~., space="free_y", scale="free_y") +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Proportion of case studies", y="") +
  # Legend
  scale_fill_manual(name="Importance", values=c("grey80", RColorBrewer::brewer.pal(3, "Blues"))) +
  # Theme
  theme_bw() + my_theme
g

# Export plot
ggsave(g, filename=file.path(plotdir, "Fig3_attribute_importance.png"), 
       width=4.5, height=4.5, units="in", dpi=600)

