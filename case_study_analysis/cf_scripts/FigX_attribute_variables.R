
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

# Build importance data
data_imp <- data_orig %>% 
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

# Build score data
data_sco <- data_orig %>% 
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

# Build quality data
data_qual <- data_orig %>% 
  # Simplify
  select(case_study, dimension, attribute, quality) %>% 
  # Summarize
  group_by(dimension, attribute, quality) %>% 
  summarise(n=n()) %>% 
  ungroup() %>% 
  # Calculate prop
  group_by(dimension, attribute) %>% 
  mutate(prop=n/n_cases) %>% 
  ungroup() %>% 
  # Order quality
  mutate(quality=recode(quality, 
                        "A - adequate and reliable data/information"="Excellent",
                        "B - limited data/information and expert judgement"="Good",
                        "C - fairly confident that the answer provided reflects the true state of the system"="Fair",
                        "D - not confident that the answer provided reflects the true state of the system"="Low",
                        "E - No data"="No data"),
         quality=factor(quality, levels=c("No data", "Low", "Fair", "Good", "Excellent"))) %>% 
  # Add numeric quality
  mutate(quality_num=as.numeric(quality))


# Average importance by attribute
data_ord <- data_imp %>% 
  group_by(dimension, attribute) %>% 
  summarize(importance_avg=sum(prop*importance_num)) %>% 
  ungroup() %>% 
  arrange(dimension, importance_avg)

# Order data
data_imp_ordered <- data_imp %>% 
  mutate(attribute=factor(attribute, data_ord$attribute))
data_sco_ordered <- data_sco %>% 
  mutate(attribute=factor(attribute, data_ord$attribute))
data_qual_ordered <- data_qual %>% 
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
                   plot.tag=element_text(size=7),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.position="bottom",
                   legend.key.size = unit(0.2, "cm"),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot importance
g1 <- ggplot(data_imp_ordered, aes(y=attribute, x=prop, fill=importance)) +
  facet_grid(dimension~., space="free_y", scale="free_y") +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Percent of case studies", y="", tag="A") +
  scale_x_continuous(labels = scales::percent) +
  # Legend
  scale_fill_manual(name="Importance", values=c(RColorBrewer::brewer.pal(3, "Blues")),
                    guide=guide_legend(title.position = "top")) +
  # Theme
  theme_bw() + my_theme
g1

# Plot score
g2 <- ggplot(data_sco_ordered, aes(y=attribute, x=prop, fill=score)) +
  facet_grid(dimension~., space="free_y", scale="free_y") +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Percent of case studies", y="", tag="B") +
  scale_x_continuous(labels = scales::percent) +
  # Legend
  scale_fill_manual(name="Score", values=c(RColorBrewer::brewer.pal(4, "Blues")),
                    guide=guide_legend(title.position = "top")) +
  # Theme
  theme_bw() + my_theme + 
  theme(axis.text.y=element_blank())
g2

# Plot quality
g3 <- ggplot(data_qual_ordered, aes(y=attribute, x=prop, fill=quality)) +
  facet_grid(dimension~., space="free_y", scale="free_y") +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Percent of case studies", y="", tag="C") +
  scale_x_continuous(labels = scales::percent) +
  # Legend
  scale_fill_manual(name="Data quality", values=c(RColorBrewer::brewer.pal(6, "Blues")),
                    guide=guide_legend(title.position = "top")) +
  # Theme
  theme_bw() + my_theme +
  theme(axis.text.y=element_blank())
g3

# Merge
g <- gridExtra::grid.arrange(g1, g2, g3, nrow=1, widths=c(0.45, 0.55/2, 0.55/2))
g

# Export plot
ggsave(g, filename=file.path(plotdir, "FigX_attribute_variables.png"), 
       width=7.5, height=4.5, units="in", dpi=600)

