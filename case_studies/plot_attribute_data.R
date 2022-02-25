
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(ggplot2)
library(tidyverse)

# Directories
datadir <- "case_studies/data"
plotdir <- "case_studies/figures"

# Read data
data_orig <- read.csv(file.path(datadir, "attribute_table_clean.csv"), as.is=T)


# Attribute scores
################################################################################

# Average score by case study
stats_case <- data_orig %>% 
  group_by(case_study) %>% 
  summarize(score_avg=mean(score)) %>% 
  ungroup() %>% 
  arrange(score_avg)

# Average score by case study
stats_attr <- data_orig %>% 
  group_by(dimension, attribute) %>% 
  summarize(score_avg=mean(score)) %>% 
  ungroup() %>% 
  # Arrange
  arrange(dimension, score_avg)

# Order data
data1 <- data_orig %>% 
  mutate(attribute=factor(attribute, levels=stats_attr$attribute),
         case_study=factor(case_study, levels=stats_case$case_study))

# Theme
my_theme <-  theme(axis.text=element_text(size=7),
                   axis.title=element_blank(),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=8),
                   strip.text=element_text(size=8),
                   plot.title=element_text(size=10),
                   axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot(data1, aes(y=attribute, x=case_study, fill=score)) +
  facet_grid(dimension~., space="free_y", scale="free_y") +
  geom_tile() +
  # Labels
  labs(x="", y="") +
  # Legend
  scale_fill_gradientn(name="Score",
                       colors=RColorBrewer::brewer.pal(9, "Blues")) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + my_theme
g  

# Export
ggsave(g, filename=file.path(plotdir, "case_study_attribute_scores.png"), 
       width=5, height=5, units="in", dpi=600)


# Attribute scores
################################################################################

n_cases <- n_distinct(data_orig$case_study)

# Importance stats
stats_score <- data_orig %>% 
  # Complete importance
  mutate(score=as.character(score)) %>% 
  complete(attribute, case_study, fill=list(score="Not provided")) %>%
  # Fill dimensions
  group_by(attribute) %>%
  mutate(dimension=dimension %>% na.omit() %>% unique()) %>%
  # Summarize
  group_by(domain, dimension, attribute, score) %>% 
  summarise(n=n()) %>% 
  ungroup() %>% 
  # Calculate prop
  mutate(prop=n/n_cases) %>% 
  # Order importance
  mutate(score=factor(score, levels=c("Not provided", "1", "2", "3", "4")))

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
                   legend.position="bottom",
                   legend.background = element_rect(fill=alpha('blue', 0)))


# Plot data
g1 <- ggplot(data1, aes(y=attribute, x=case_study, fill=score)) +
  facet_grid(dimension~., space="free_y", scale="free_y") +
  geom_tile() +
  # Labels
  labs(x="", y="", tag="A") +
  # Legend
  scale_fill_gradientn(name="Score",
                       colors=RColorBrewer::brewer.pal(9, "Blues")) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
g1 

# Plot data
g2 <- ggplot(stats_score, aes(y=factor(attribute, levels=stats_attr$attribute), x=prop, fill=score)) +
  facet_grid(dimension~., space="free_y", scale="free_y") +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Proportion of case studies", y="", tag="B") +
  # Legend
  scale_fill_manual(name="Score", values=c("grey80", RColorBrewer::brewer.pal(4, "Blues"))) +
  guides(fill = guide_legend(title.position="top")) +
  # Theme
  theme_bw() + my_theme +
  theme(axis.text.y=element_blank(),
        legend.key.size = unit(0.3, "cm"))
g2

# Merge data
g <- gridExtra::grid.arrange(g1, g2, ncol=2, widths=c(0.7, 0.3))
g

ggsave(g, filename=file.path(plotdir, "case_study_attribute_importance_v2.png"), 
       width=6.5, height=4.5, units="in", dpi=600)

# Attribute importance
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

my_theme <-  theme(axis.text=element_text(size=6),
                   axis.title=element_text(size=8),
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

ggsave(g, filename=file.path(plotdir, "case_study_attribute_importance.png"), 
       width=4.5, height=4.5, units="in", dpi=600)


# Attribute data quality
################################################################################


# Importance stats
stats_qual <- data_orig %>% 
  # Fix data quality errors
  mutate(quality=recode(quality, "E - No data"="No data")) %>% 
  # Complete importance
  complete(attribute, case_study, fill=list(quality="Not provided")) %>%
  # Fill dimensions
  group_by(attribute) %>% 
  mutate(dimension=dimension %>% na.omit() %>% unique()) %>% 
  # Summarize
  group_by(domain, dimension, attribute, quality) %>% 
  summarise(n=n(),) %>% 
  ungroup() %>% 
  # Calculate prop
  mutate(prop=n/n_cases) %>% 
  # Order quality
  mutate(quality=factor(quality, levels=c("Not provided",
                                          "No data", "Low", "Fair", "Good", "Excellent")))

# Average importance by attribute
stats_qual_ord <- stats_qual %>% 
  filter(quality=="Excellent") %>% 
  arrange(dimension, prop)

my_theme <-  theme(axis.text=element_text(size=6),
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
                   legend.position="bottom",
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot(stats_qual, aes(y=factor(attribute, levels=stats_qual_ord$attribute), x=prop, fill=quality)) +
  facet_grid(dimension~., space="free_y", scale="free_y") +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Proportion of case studies", y="") +
  # Legend
  scale_fill_manual(name="Quality", values=c("grey80", RColorBrewer::brewer.pal(5, "Blues"))) +
  # Theme
  theme_bw() + my_theme
g

ggsave(g, filename=file.path(plotdir, "case_study_attribute_quality.png"), 
       width=4.5, height=4.5, units="in", dpi=600)

