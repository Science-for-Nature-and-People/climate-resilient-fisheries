# Clear workspace
rm(list = ls())

# Setup
################################################################################
#Load packages
load_required_packages <- function() {
  packages <- c("tidyverse", "stringr", "ggplot2", "here")
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

# Build data for typology
################################################################################

data <- data_orig %>% 
  # Simplify and convert importance to numeric
  select(case_study, dimension, attribute, score, importance) %>% 
  mutate(importance=stringr::str_to_sentence(importance),
         importance=factor(importance, levels=c("Low", "Medium", "High"))) %>% 
  mutate(importance_numeric= as.numeric(as.factor(importance)))

# Calculate average attribute score, variance, and average attribute importance and variance 
score_average <- data %>%
  group_by(dimension, attribute) %>% 
  summarize(score_avg=mean(score, na.rm=T)) %>% 
  ungroup() 

score_variance <- data %>%
  group_by(dimension, attribute) %>% 
  summarize(score_var=(var(score, na.rm=T))) %>% 
  ungroup()

importance_average <- data %>%
  group_by(dimension, attribute) %>% 
  summarize(importance_avg=mean(importance_numeric, na.rm=T)) %>% 
  ungroup()  

importance_variance<- data %>% 
  group_by(dimension, attribute) %>% 
  summarize(importance_var=var(importance_numeric, na.rm=T)) %>% 
  ungroup()

# Combine into one cross-case dataset
typology_data <- full_join(score_average, score_variance)
typology_data <- full_join(typology_data, importance_average)
typology_data <- full_join(typology_data, importance_variance)

# Add consistency columns where consistency = 1/ variance
typology_data <- typology_data %>% 
  rename(Dimension= dimension) %>% 
  mutate(consistency_score = (1/score_var)) %>% 
  mutate(consistency_importance = (1/importance_var)) %>%  arrange(consistency_importance) 

typology_data$consistency_importance_scaled <- (typology_data$consistency_importance - min(typology_data$consistency_importance)) / (max(typology_data$consistency_importance) - min(typology_data$consistency_importance))

# Create theme
my_theme2 <- theme(axis.text=element_text(size=10),
                   axis.title=element_text(size=11),
                   legend.text=element_text(size=11),
                   legend.title=element_text(size=11),
                   strip.text=element_text(size=11),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.position="bottom",
                   legend.background = element_rect(fill=alpha('blue', 0)))

mean(typology_data$importance_avg) #2.323099
mean(typology_data$score_avg) #2.843567

################ Add overall average score for attributes and overall average importance for attributes to figure. 
quad <- ggplot(data = typology_data, aes(x = importance_avg, y = score_avg, size = consistency_importance_scaled)) + 
  geom_point(color='black', shape=21, aes(fill=Dimension), alpha=.75) + 
  scale_fill_manual(values = c("Ecological" = "#72B077", "Governance" = "#C25866", "Socio-economic" = "#D6B65D"), name = "Dimension") + 
  scale_size_continuous(range = c(2, 10), name = "Rating consistency", breaks = c(0, 0.5, 1)) + 
  scale_x_continuous(limits = c(1.5, 3), breaks = c(2, 3)) + 
  scale_y_continuous(limits = c(2, 4), breaks = c(2, 3, 4)) +
  theme_bw() + my_theme2 +  
  xlab("Importance") + 
  ylab("Score") + 
  geom_hline(yintercept = mean(typology_data$score_avg), linetype = 'dashed', col = '#333333') + 
  geom_vline(xintercept = mean(typology_data$importance_avg), linetype = 'dashed', col = '#333333') +
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        legend.box = "vertical",  # Ensuring legends are stacked vertically
        legend.box.just = "center") + 
  theme(axis.text = element_text(color="black")) +
  guides(fill = guide_legend(override.aes = list(size=5)), 
         size = guide_legend(override.aes = list(shape = 21, fill = "white")))

# Add back in geom_text(aes(label = attribute), size = 2, vjust = 1, hjust = .5) to see attribute labels. Version here saved without labels or panels. Panles and labels were added in manually afterwards 
ggsave(quad, filename=file.path(plotdir, "Figure_3_no_labels.png"), 
       width=5.51181, height=5.51181, units="in", dpi=600) 
