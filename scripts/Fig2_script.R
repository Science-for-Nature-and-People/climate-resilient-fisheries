
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
load_required_packages <- function() {
  packages <- c("ggplot2", "tidyverse", "RColorBrewer", "gridExtra", "scales", "stringr")
  for (pkg in packages) {
    if (pkg %in% rownames(installed.packages())) {
      library(pkg, character.only = TRUE)
    } else {
      message(paste("Installing and loading package:", pkg))
      install.packages(pkg)
      library(pkg, character.only = TRUE)
    }
  }
}
load_required_packages()


# Directories
datadir <- "clean_data"
plotdir <- "figures"

# Read data
data_orig <- readRDS(file.path(datadir, "case_study_attribute_score_data.Rds"))


# Build data
################################################################################

# Number of cases
n_cases <- n_distinct(data_orig$case_study)

# Build data
data1 <- data_orig %>% 
  # Simplify
  select(case_study, dimension, attribute, importance) %>% 
  # Summarize
  group_by(dimension, attribute, importance) %>% 
  summarise(n=n()) %>% 
  ungroup() %>% 
  # Calculate prop
  group_by(dimension, attribute) %>% 
  mutate(prop=n/n_cases) %>% 
  ungroup() %>%
  # Order importance
  mutate(importance=stringr::str_to_sentence(importance),
         importance=factor(importance, levels=c("Low", "Medium", "High")),
         importance_num=as.numeric(importance))
  

# Average quality by attribute
data_ord1 <- data1 %>% 
  group_by(dimension, attribute) %>% 
  summarize(importance_avg=sum(prop*importance_num)) %>% 
  ungroup() %>% 
  arrange(dimension, importance_avg)

# Order data
data_ordered1 <- data1 %>% 
  mutate(attribute=factor(attribute, data_ord1$attribute))


# Build data
data2 <- data_orig %>% 
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
data_ord2 <- data2 %>% 
  group_by(dimension, attribute) %>% 
  summarize(score_avg=sum(prop*score_num)) %>% 
  ungroup() %>% 
  arrange(dimension, score_avg)

# Order data
data_ordered2 <- data2 %>% 
  mutate(attribute=factor(attribute, data_ord2$attribute))


# Plot data
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.title=element_text(size=7),
                   axis.title.y=element_blank(),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=7),
                   strip.text=element_text(size=7),
                   plot.tag = element_text(size=7),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.position="bottom",
                   legend.key.size = unit(0.3, "cm"),
                   legend.margin = margin(-4,0,0,0),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g1 <- ggplot(data_ordered1, aes(y=attribute, x=prop, fill=importance)) +
  facet_grid(dimension~., space="free_y", scale="free_y") +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Percent of case studies", y="", tag="A") +
  scale_x_continuous(labels = scales::percent) +
  # Legend
  scale_fill_manual(name="Importance", values=c(RColorBrewer::brewer.pal(3, "Blues")),
                    guide = guide_legend(reverse = TRUE)) +
  # Theme
  theme_bw() + my_theme
g1

# Plot data
g2 <- ggplot(data_ordered2, aes(y=attribute, x=prop, fill=score)) +
  facet_grid(dimension~., space="free_y", scale="free_y") +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Percent of case studies", y="", tag="B") +
  scale_x_continuous(labels = scales::percent) +
  # Legend
  scale_fill_manual(name="Score", values=c(RColorBrewer::brewer.pal(4, "Blues")),
                    guide = guide_legend(reverse = TRUE)) +
  # Theme
  theme_bw() + my_theme
g2

# Merge
g <- gridExtra::grid.arrange(g1, g2, nrow=1)
g

# Export plot
ggsave(g, filename=file.path(plotdir, "Figure_2.png"), 
       width=6.5, height=4.5, units="in", dpi=600)

