
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
  select(case_study, dimension, attribute, score)

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
data_ordered <- data %>% 
  # Order case studies and attributes
  mutate(attribute=factor(attribute, levels=stats_attr$attribute),
         case_study=factor(case_study, levels=stats_case$case_study)) %>% 
  # Order score
  mutate(score=as.character(score), 
         score=recode_factor(score, 
                             "1"="Very low", 
                             "2"="Low", 
                             "3"="Moderate", 
                             "4"="High"))
         # score=factor(score, levels=c("1", "2", "3", "4"))




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
g1 <- ggplot(data_ordered, aes(y=attribute, x=case_study, fill=score)) +
  facet_grid(dimension~., space="free_y", scale="free_y") +
  geom_tile() +
  # Labels
  labs(x="", y="", tag="A") +
  # Legend
  scale_fill_manual(name="Strength", values=RColorBrewer::brewer.pal(4, "Blues")) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
g1 







# Export plot
ggsave(g, filename=file.path(plotdir, "Fig5_case_study_clustering.png"), 
       width=6.5, height=5, units="in", dpi=600)

