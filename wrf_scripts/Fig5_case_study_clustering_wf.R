
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(ape)
library(ggdendro)
library(ggplot2)
library(tidyverse)
library(here)
library(RColorBrewer)

# Directories
datadir <- "case_study_analysis/clean_data"
plotdir <- "case_study_analysis/wrf_scripts/plots"

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
  mutate(case_study=factor(case_study, levels=case_studies_ordered))  %>% 
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


# Plot data - WF changed fill from score_scaled to score
g1 <- ggplot(data_ordered, aes(y=attribute, x=case_study, fill=factor(score))) +
  facet_grid(dimension~., space="free_y", scale="free_y") +
  geom_tile() +
  # Labels
  labs(x="", y="", tag="A") +
  # Legend
  scale_fill_manual(name="Score", values=RColorBrewer::brewer.pal(4, "Blues"), na.value = "black") +
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
g <- gridExtra::grid.arrange(g1, g2, 
                             heights=c(0.6, 0.4))
g

# Export plot
ggsave(g, filename=file.path(plotdir, "Fig5_case_study_clustering_wf.png"), 
       width=6.5, height=7, units="in", dpi=600)


# NEW plots based on lily's boxplots ----
library(RColorBrewer)
library(qualpalr)

group1 <- case_studies_ordered[1:3]
group2 <- case_studies_ordered[4:7]
group3 <- case_studies_ordered[8:11]
group4 <- case_studies_ordered[12:14]
group5 <- case_studies_ordered[15:18]

attrib_by_dim <- data %>% 
  select(dimension, attribute) %>% 
  distinct() %>% 
  arrange(dimension, attribute)

data_ord <- data %>% 
  mutate(group = case_when(case_study %in% group1 ~ 'group1',
                           case_study %in% group2 ~ 'group2',
                           case_study %in% group3 ~ 'group3',
                           case_study %in% group4 ~ 'group4',
                           case_study %in% group5 ~ 'group5')) %>% 
  mutate(attribute = factor(attribute, levels= attrib_by_dim$attribute)) %>% 
  group_by(dimension, case_study) %>% 
  mutate(mean_score = mean(score, na.rm=T))
  

# install.packages("ggbeeswarm")
#library(ggbeeswarm)

#getPalette = colorRampPalette(brewer.pal(12, "Paired"))
#case_palette = sample(getPalette(18),18, replace = F, set.seed(1234))
case_palette = (qualpal(18, colorspace=list(h=c(0,360), s=c(0.3,1), l=c(0.2,0.8))))$hex

# FIGURE 5: 
data_ord %>% 
  ggplot(aes(x = group, y = score, colour = case_study))+
  geom_boxplot(alpha = 1, fill = "gray95", colour = "gray60", outlier.shape = NA, lwd = 0.3)+
  geom_beeswarm(cex = .6, alpha = 0.5, shape = 21, size = 1, 
                fill = "gray60", colour = "gray40", show.legend = F, lwd = 0.1) +
  geom_point(aes(x=group, y = mean_score, fill = case_study, shape = case_study), colour = "gray30",
              size = 2.5, lwd = 0.01, alpha = 1)+
  scale_fill_manual(values = case_palette)+
  scale_shape_manual(values = rep(seq(21,25),4))+
  facet_wrap(~dimension, ncol = 3)+
  theme_bw()+
  theme(legend.position = "right", 
        axis.title.x = element_blank())
  

ggsave(here("case_study_analysis","wrf_scripts","plots","Fig6_case_study_groups.png"), 
       height = 5, width = 10)

# v2: facet by group
data_ord %>% 
  ggplot(aes(x = dimension, y = score, colour = case_study))+
  geom_boxplot(alpha = 1, fill = "gray95", colour = "gray60", outlier.shape = NA, lwd = 0.3)+
  geom_beeswarm(cex = .6, alpha = 0.5, shape = 21, size = 1, 
                fill = "gray60", colour = "gray40", show.legend = F, lwd = 0.1) +
  geom_point(aes(x=dimension, y = mean_score, fill = case_study, shape = case_study), colour = "gray30",
             size = 2.5, lwd = 0.01, alpha = 1)+
  scale_fill_manual(values = case_palette)+
  scale_shape_manual(values = rep(seq(21,25),4))+
  facet_wrap(~group, ncol = 5)+
  theme_bw()+
  scale_x_discrete("Dimension", labels = c("Ecological" = "Ecol",
                                           "Governance" = "Gov", 
                                           "Socio-economic" = "Soc-Econ"))+
  theme(legend.position = "right", 
        axis.title.x = element_blank())


ggsave(here("case_study_analysis","wrf_scripts","plots","Fig6_case_study_groups_v2.png"), 
       height = 5, width = 10)

# v3: facet by domain
# need to think through data_ord
