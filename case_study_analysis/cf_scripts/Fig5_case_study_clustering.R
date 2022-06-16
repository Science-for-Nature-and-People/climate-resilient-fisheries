
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


# Build data
################################################################################

# # Format data
# data <- data_orig %>% 
#   # Simplify
#   select(case_study, dimension, attribute, score)
# 
# # Average score by case study
# stats_case <- data %>% 
#   group_by(case_study) %>% 
#   summarize(score_avg=mean(score, na.rm=T)) %>% 
#   ungroup() %>% 
#   arrange(score_avg)
# 
# # Average score by case study
# stats_attr <- data %>% 
#   group_by(dimension, attribute) %>% 
#   summarize(score_avg=mean(score, na.rm=T)) %>% 
#   ungroup() %>% 
#   # Arrange
#   arrange(dimension, score_avg)
# 
# # Order data
# data_ordered <- data %>% 
#   # Order case studies and attributes
#   mutate(attribute=factor(attribute, levels=stats_attr$attribute),
#          case_study=factor(case_study, levels=stats_case$case_study)) %>% 
#   # Order score
#   mutate(score=as.character(score), 
#          score=recode_factor(score, 
#                              "1"="Very low", 
#                              "2"="Low", 
#                              "3"="Moderate", 
#                              "4"="High"))
#          # score=factor(score, levels=c("1", "2", "3", "4"))


# Cluster analysis
################################################################################

# Build data
data <- data_orig %>% 
  # Simplify
  select(case_study, dimension, attribute, score) %>% 
  # Scale scores by attribute
  group_by(attribute) %>% 
  mutate(score_scaled=scale(score) %>% as.numeric()) %>% 
  ungroup()

# Convert data to matrix
data_mat <- data %>% 
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

# Order data
case_studies_ordered <- clusters$labels[clusters$order]
data_ordered <- data %>% 
  # Order case studies
  mutate(case_study=factor(case_study, levels=case_studies_ordered))
  
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
g1 <- ggplot(data_ordered, aes(y=attribute, x=case_study, fill=score_scaled)) +
  facet_grid(dimension~., space="free_y", scale="free_y") +
  geom_tile() +
  # Labels
  labs(x="", y="", tag="A") +
  # Legend
  scale_fill_gradient2(name="Strength\n(centered and scaled)", 
                       midpoint=0, low="darkred", high="navy") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
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
ggsave(g, filename=file.path(plotdir, "Fig5_case_study_clustering.png"), 
       width=6.5, height=7, units="in", dpi=600)

