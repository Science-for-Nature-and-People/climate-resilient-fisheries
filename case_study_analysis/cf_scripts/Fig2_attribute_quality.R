
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
  mutate(quality=ifelse(is.na(quality), "Not provided", quality),
         quality=recode(quality, 
                        "A - adequate and reliable data/information"="Excellent",
                        "B - limited data/information and expert judgement"="Good",
                        "C - fairly confident that the answer provided reflects the true state of the system"="Fair",
                        "D - not confident that the answer provided reflects the true state of the system"="Low",
                        "E - No data"="No data"),
         quality=factor(quality, levels=c("Not provided",
                                          "No data", "Low", "Fair", "Good", "Excellent")))

# Average importance by attribute
data_ord <- data %>% 
  filter(quality=="Excellent") %>% 
  arrange(dimension, prop)

# Order data
data_ordered <- data %>% 
  mutate(attribute=factor(attribute, data_ord$attribute))


# Stats for manuscript
################################################################################

# % Complete
data %>% 
  count(dimension, quality) %>% 
  group_by(dimension) %>% 
  mutate(ntot=sum(n)) %>% 
  ungroup() %>% 
  mutate(perc=100-(n/ntot*100)) %>% 
  filter(quality=="Not provided")

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
                   legend.key.size = unit(0.4, "cm"),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot(data_ordered, aes(y=attribute, x=prop, fill=quality)) +
  facet_grid(dimension~., space="free_y", scale="free_y") +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Percent of case studies", y="") +
  scale_x_continuous(labels = scales::percent) +
  # Legend
  scale_fill_manual(name="Data quality", values=c("grey80", RColorBrewer::brewer.pal(5, "Blues"))) +
  # Theme
  theme_bw() + my_theme
g

# Export plot
ggsave(g, filename=file.path(plotdir, "Fig2_attribute_quality.png"), 
       width=4.5, height=4.5, units="in", dpi=600)

