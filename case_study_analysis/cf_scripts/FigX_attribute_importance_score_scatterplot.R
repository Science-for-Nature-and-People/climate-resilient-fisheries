
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

# Format data
data <- data_orig %>% 
  # Simplify
  select(case_study, dimension, attribute, score, importance) %>% 
  # Order importance
  mutate(importance_num=recode(importance,
                               "low"=1, 
                               "medium"=2,
                               "high"=3) %>% as.numeric())

# Calculate averag
data_avg <- data %>% 
  # Summarize
  group_by(dimension, attribute) %>% 
  summarize(score_avg=mean(score),
            importance_avg=mean(importance_num)) %>% 
  ungroup()




# Plot data
################################################################################

# Theme
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
                   legend.position="none",
                   legend.background = element_rect(fill=alpha('blue', 0)))


# Plot data
g <- ggplot(data_avg, aes(x=importance_avg, y=score_avg, fill=attribute)) +
  facet_wrap(~dimension) +
  geom_point(pch=21, size=3) +
  ggrepel::geom_text_repel(aes(label=attribute, color=attribute), size=2,
                           max.overlaps = 30) +
  # Labels
  labs(x="Importance", y="Score") +
  # Axes
  scale_x_continuous(breaks=1:3, labels=c("Low", "Moderate", "High"), lim=c(1,3)) +
  scale_y_continuous(breaks=1:4, lim=c(0.5, 4.5)) +
  # Theme
  theme_bw() + my_theme
g

# Export plot
ggsave(g, filename=file.path(plotdir, "FigX_attribute_importance_score_scatterplot.png"), 
       width=6.5, height=2.5, units="in", dpi=600)





