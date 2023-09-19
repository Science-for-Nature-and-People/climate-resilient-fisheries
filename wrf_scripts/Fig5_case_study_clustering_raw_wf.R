# wf modifying cf's script 30-jun 

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(ape)
library(ggdendro)
library(ggplot2)
library(tidyverse)

# Directories
datadir <- "case_study_analysis/clean_data"
plotdir <- "case_study_analysis/cf_scripts/figures"

# Read data
data_orig <- readRDS(file.path(datadir, "case_study_attribute_score_data.Rds"))


# Cluster analysis
################################################################################

# Build data
data <- data_orig %>% 
  # Simplify
  select(case_study, dimension, attribute, score) %>% 
  # Scale scores by attribute
  group_by(attribute) %>% 
  mutate(score_scaled=scale(score) %>% as.numeric()) %>% 
  ungroup() %>% 
  # Format score
  mutate(score_chr=as.character(score), 
         score_chr=recode_factor(score_chr, 
                             "1"="Very low", 
                             "2"="Low", 
                             "3"="Moderate", 
                             "4"="High"))

# Convert data to matrix
data_mat <- data %>% 
  # Simplify
  select(case_study, attribute, score) %>% 
  # Spread
  spread(key = attribute, value = score) %>% 
  # Make a matrix by taking case study to column name
  column_to_rownames(var = "case_study") %>% 
  # Convert to matrix
  as.matrix() 

# Perform cluster analysis
clusters <- data_mat %>%     
  # Calculate distance matrix
  stats::dist() %>% 
  # Calculate cluster
  stats::hclust(method = "ward.D2")

# Attribute order
attr_order <- data %>% 
  group_by(dimension, attribute) %>% 
  summarize(score_avg=mean(score)) %>% 
  ungroup() %>% 
  arrange(dimension, score_avg)

# Order data
case_studies_ordered <- clusters$labels[clusters$order]
data_ordered <- data %>% 
  # Order case studies
  mutate(case_study=factor(case_study, levels=case_studies_ordered)) %>% 
  # Order attributes
  mutate(attribute=factor(attribute, levels=attr_order$attribute))
  

# Plot data
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.title=element_text(size=8),
                   plot.tag=element_text(size=8),
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
                   legend.background = element_rect(fill=scales::alpha('blue', 0)))


# Plot data
g1 <- ggplot(data_ordered, aes(y=attribute, x=case_study, fill=score_chr)) +
  facet_grid(dimension~., space="free_y", scale="free_y") +
  geom_tile() +
  # Labels
  labs(x="", y="", tag="A") +
  # Legend
  scale_fill_manual(name="Strength", values=RColorBrewer::brewer.pal(4, "Blues")) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position="top",
        legend.margin = margin(-5,-5,0,-5),
        axis.title=element_blank(),
        axis.text.x=element_blank())
g1 

# Plot clusters
g2 <- ggdendro::ggdendrogram(clusters, rotate = F, theme_dendro = FALSE) +
  # Labels
  labs(x="", y="\n\n\n\n\n\n\n\n\n", tag="B") +
  # Theme
  theme_bw() + my_theme +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
g2

# Merge
g <- gridExtra::grid.arrange(g1, g2, heights=c(0.6, 0.4))
g

# Export plot
ggsave(g, filename=file.path(plotdir, "Fig5_case_study_clustering_raw.png"), 
       width=6.5, height=7, units="in", dpi=600)

