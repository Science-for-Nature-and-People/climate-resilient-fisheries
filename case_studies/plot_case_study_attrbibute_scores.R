
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
datafile <- "Free_Dungeness_crab.xlsx"
data_orig <- readxl::read_excel(file.path(datadir, datafile), sheet=6, skip = 5, na="NA")


# Format data
################################################################################

# Format data
data <- data_orig %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(dimension=dimensions, 
         domain=new_domain, 
         attribute=resilience_attribute, 
         score=score_1_4,
         quality=information_quality, 
         importance=importance_of_attribute_in_study_system_high_medium_low, 
         work_as_described=does_the_mechanism_work_as_described_in_this_fishery_system) %>% 
  # Simplify
  select(dimension, domain, attribute, score, quality, importance, work_as_described) %>% 
  # Remove spacer rows
  filter(!is.na(attribute)) %>% 
  # Format scores
  mutate(score=as.numeric(score)) %>% 
  # Format dimension
  mutate(dimension=recode_factor(dimension, 
                                 "Ecological"="Ecological",
                                 "Socio-economic"="Social-economic",
                                 "Governance-management"="Goverance")) %>% 
  # Format data quality
  mutate(quality=ifelse(quality=="NA - Not relevant in this system", NA, quality),
         quality=recode_factor(quality,
                               "E - No data/information; no basis for expert judgement"="No data",
                               "D - not confident that the answer provided reflects the true state of the system"="Low",
                               "C - fairly confident that the answer provided reflects the true state of the system"="Fair",
                               "B - limited data/information and expert judgement"="Good",
                               "A - adequate and reliable data/information"="Excellent")) %>% 
  # Format importance
  mutate(importance=stringr::str_to_sentence(importance),
         importance=factor(importance, levels=c("Low", "Medium", "High"))) %>% 
  # Format attribute
  mutate(attribute=ifelse(work_as_described=="no", paste0(attribute, "*"), attribute)) %>% 
  # Arrange
  arrange(dimension, domain, score) %>% 
  # Set attribute order
  mutate(attribute=factor(attribute, levels=attribute))

# Inspect
str(data)
table(data$dimension)
table(data$quality)

# Plot data
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=8),
                   axis.title=element_text(size=9),
                   legend.text=element_text(size=8),
                   legend.title=element_text(size=9),
                   strip.text=element_text(size=9),
                   plot.title=element_text(size=10),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"))

# Plot data
g <- ggplot() +
  # Facet
  facet_grid(dimension~., space="free", scales="free_y") +
  # Plot points
  geom_segment(data=data, mapping=aes(y=attribute, yend=attribute, x=0.5, xend=score, color=domain, alpha=quality)) +
  geom_point(data=data, mapping=aes(x=score, y=attribute, color=domain, alpha=quality, size=importance), inherit.aes = F) +
  # Limits
  scale_x_continuous(lim=c(0.5, 4.3), breaks=1:4, labels=c("1\n(none)", "2\n(low)", "3\n(medium)", "4\n(high)")) +
  # Labels
  labs(x="Attribute score", y="", title="CA Dungeness crab resilience score card") +
  # Bars
  # geom_bar(stat="identity", color="black", lwd=0.2) +
  # Legend
  scale_alpha_ordinal(name="Data quality") +
  scale_color_discrete(name="Domain") +
  scale_size_ordinal(name="Importance") +
  guides(color = guide_legend(order = 1), size = guide_legend(order = 2), alpha = guide_legend(order = 3)) +
  # Theme
  theme_bw() + my_theme
g

# Export plot
ggsave(g, filename=file.path(plotdir, "case_study_attribute_scores_dcrab.png"), 
       width=6.5, height=6.5, units="in", dpi=600)

