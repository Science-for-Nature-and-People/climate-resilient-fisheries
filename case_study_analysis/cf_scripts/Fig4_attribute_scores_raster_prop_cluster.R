
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

# Cluster analysis
################################################################################

# Build data
data1 <- data_orig %>% 
  # Simplify
  select(case_study, dimension, attribute, score) %>% 
  # Scale scores by attribute
  group_by(attribute) %>% 
  mutate(score_scaled=scale(score) %>% as.numeric()) %>% 
  ungroup()

# Convert data to matrix
data_mat <- data1 %>% 
  # Simplify
  select(case_study, attribute, score_scaled) %>% 
  # Spread
  spread(key = attribute, value = score_scaled) %>% 
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

# Megan's version
# My version is a bit cleaner and produces the same results
if(F){
  data_mat_mf <- data_orig %>% 
    # Simplify
    select(case_study, attribute, score) %>% 
    # Spread
    pivot_wider(names_from = attribute, values_from = score) %>% 
    # Make a matrix by taking case study to column name
    column_to_rownames(var = "case_study") %>% 
    # Scale data
    mutate(across(everything(),~scale(.x))) %>% 
    # Repalve NAs with zeroes (Megan wrote this, I don't think we need it)
    mutate(across(everything(), ~replace_na(.x, 0))) %>% # REVIEW
    # Convert to matrix
    as.matrix() 
  
  score_hc <- data_mat_mf %>% 
    # Calculate distance matrix
    stats::dist() %>% 
    # Calculate cluster
    stats::hclust(method = "ward.D2")
  
  # Experimental plots
  plot(score_hc)
  plot(as.phylo(score_hc), type = "unrooted", cex = 0.6,
       no.margin = TRUE)
  ggdendro::ggdendrogram(score_hc, rotate = F, theme_dendro = FALSE)
  
  # Plot a more finalized verson for the synthesisi paper
  plot(score_hc, ylab="", yaxt="n", sub="", main="")
  
}


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

# Plot bars
g1 <- ggplot(stats_score, aes(y=attribute, x=prop, fill=score)) +
  facet_grid(dimension~., space="free_y", scale="free_y") +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Percent of case studies", y="", tag="A") +
  scale_x_continuous(labels = scales::percent) +
  # Legend
  scale_fill_manual(name="Score", values=RColorBrewer::brewer.pal(4, "Blues")) +
  guides(fill = guide_legend(title.position="top")) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position="none")
g1

# Plot raster
g2 <- ggplot(data_ordered, aes(y=attribute, x=case_study, fill=score)) +
  facet_grid(dimension~., space="free_y", scale="free_y") +
  geom_tile() +
  # Add lines
  geom_vline(xintercept = c(3.5, 11.5, 14.5),
             color="black", linetype="dashed") + #
  geom_vline(xintercept = c(7.5),
             color="black", linetype="solid") + #
  # Labels
  labs(x=" \n ", y="", tag="B") +
  # Legend
  scale_fill_manual(name="Score", values=RColorBrewer::brewer.pal(4, "Blues")) +
  # Theme
  theme_bw() + my_theme +
  theme(axis.text=element_blank(),
        legend.position = "none")
g2

# Plot clusters
g3 <- ggdendro::ggdendrogram(clusters, rotate = F, theme_dendro = FALSE) +
  # Add lines
  geom_vline(xintercept = c(3.5, 7.5, 11.5, 14.5), 
             color="blue", linetype=c("dashed", "solid", "dashed", "dashed")) +
  # Labels
  labs(x="", y="", tag="C") +
  # Cluster number
  annotate(geom="text", x=c(2, 5.6, 9.6, 13, 16.6), y=12.5, label=1:5, color="blue", size=2.8) + # -17 for beneath
  coord_cartesian(ylim=c(0, NA), clip="off") +
  # Theme
  theme_bw() + my_theme +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        plot.margin = margin(0, 0.75, 0, 0.2, "cm"))
g3

# Blank plot
g_blank <- ggplot() + theme_void()
g_blank <- cowplot::get_legend(g1 +theme(legend.position = c(0.55, 0.9), 
                                         legend.direction = "horizontal"))


# Merge data
layout_matrix <- matrix(c(1,2,
                          3,4), byrow=T, ncol=2)
g <- gridExtra::grid.arrange(g1, g2, g_blank, g3, 
                             layout_matrix=layout_matrix, heights=c(0.6, 0.4))
g

# Export plot
ggsave(g, filename=file.path(plotdir, "Fig4_attribute_scores_raster_prop_clusters.png"), 
       width=6.5, height=6.5, units="in", dpi=600)

