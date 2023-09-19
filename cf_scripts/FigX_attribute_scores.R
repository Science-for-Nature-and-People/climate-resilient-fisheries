
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

# Number of cases
n_cases <- n_distinct(data_orig$case_study)

# Build data
data <- data_orig %>% 
  # Simplify
  select(case_study, dimension, attribute, score) %>% 
  # Summarize
  group_by(dimension, attribute, score) %>% 
  summarise(n=n()) %>% 
  ungroup() %>% 
  # Calculate prop
  group_by(dimension, attribute) %>% 
  mutate(prop=n/n_cases) %>% 
  ungroup() %>%
  # Order score
  mutate(score=as.character(score), 
         score=recode_factor(score, 
                             "1"="Very low", 
                             "2"="Low", 
                             "3"="Moderate", 
                             "4"="High"),
         # score=factor(score, levels=c("1", "2", "3", "4")),
         score_num=as.numeric(score))
  

# Average quality by attribute
data_ord <- data %>% 
  group_by(dimension, attribute) %>% 
  summarize(score_avg=sum(prop*score_num)) %>% 
  ungroup() %>% 
  arrange(dimension, score_avg)

# Order data
data_ordered <- data %>% 
  mutate(attribute=factor(attribute, data_ord$attribute))


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
g <- ggplot(data_ordered, aes(y=attribute, x=prop, fill=score)) +
  facet_grid(dimension~., space="free_y", scale="free_y") +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Percent of case studies", y="") +
  scale_x_continuous(labels = scales::percent) +
  # Legend
  scale_fill_manual(name="Score", values=c(RColorBrewer::brewer.pal(4, "Blues"))) +
  # Theme
  theme_bw() + my_theme
g

# Export plot
ggsave(g, filename=file.path(plotdir, "FigX_attribute_scores.png"), 
       width=4.5, height=4.5, units="in", dpi=600)

