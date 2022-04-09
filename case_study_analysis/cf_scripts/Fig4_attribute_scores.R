
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
         score=ifelse(is.na(score), "NA", score),
         score=recode(score, "NA"="Not provided"),
         score=factor(score, levels=c("Not provided", "1", "2", "3", "4")))

# Number of case studies
n_cases <- n_distinct(data_ordered$case_study)

# Importance stats
stats_score <- data_ordered %>% 
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
g1 <- ggplot(data_ordered, aes(y=attribute, x=case_study, fill=score)) +
  facet_grid(dimension~., space="free_y", scale="free_y") +
  geom_tile() +
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
g2 <- ggplot(stats_score, aes(y=attribute, x=prop, fill=score)) +
  facet_grid(dimension~., space="free_y", scale="free_y") +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Percent of case studies\n \n \n \n ", y="", tag="B") +
  scale_x_continuous(labels = scales::percent) +
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

