
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(ggplot2)
library(tidyverse)

# Directories
datadir <- "case_study_analysis/clean_data"
plotdir <- "case_study_analysis/cf_scripts/figures"

# Read data
data_orig <- readRDS(file.path(datadir, "case_study_attribute_score_data.Rds"))


# Build data
################################################################################

# Build data
data <- data_orig %>% 
  # Simplify
  select(authors, case_study, dimension, attribute, quality, importance, score) %>% 
  # Gather
  gather(key="type", value="value", 5:ncol(.)) %>%
  mutate(type=stringr::str_to_sentence(type),
         type=factor(type, levels=c("Quality", "Importance", "Score"))) %>% 
  # Reduce
  filter(!is.na(value))


# Plot data
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=5),
                   axis.title=element_blank(),
                   strip.text=element_text(size=6),
                   axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"))
                   # Legend
                   #legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot(data, aes(x=type, y=attribute)) +
  facet_grid(dimension~authors, space="free_y", scales="free_y") +
  geom_tile() +
  # Labels
  labs(x="", y="") +
  # Theme
  theme_bw() + my_theme
g

# Export plot
ggsave(g, filename=file.path(plotdir, "case_study_completeness.png"), 
       width=8.5, height=4.5, units="in", dpi=600)


