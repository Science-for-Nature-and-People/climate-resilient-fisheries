
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(raster)
library(ggplot2)
library(tidyverse)

# Directories
datadir <- "score_card"
  
# Read attributes
data_orig <- readxl::read_excel(file.path(datadir, "attributes.xlsx")) %>% 
  # Rename columns
  setNames(c("catg1", "catg2", "att_id", "att_name", "score1", "score2", "score3", "score4", "att_def")) %>% 
  # Add random score
  mutate(score=sample(x=1:4, size=n(), replace=T)) %>% 
  # Arrange columns
  select(catg1:att_name, att_def, score, everything()) %>% 
  # Gather
  gather(key="score_id", value="score_text", 7:ncol(.)) %>% 
  # Format columns
  mutate(att_name=stringr::str_to_sentence(att_name),
         catg1=factor(catg1, levels=c("Ecological", "Socio-economic", "Governance and management")),
         score_id=recode_factor(score_id, "score1"="None", "score2"="Low", "score3"="Moderate", "score4"="High"))



# Plot scorecard
################################################################################



# Plot score card
g <- ggplot(data_orig, aes(x=score_id, y=att_name, fill=score)) +
  # Plot raster
  geom_raster() +
  facet_grid(rows=vars(catg1), scales="free", space="free") +
  # Legend
  scale_fill_discrete(name="", values=RColorBrewer::brewer.pal(4, "Spectral")) +
  # Labels
  labs(x="Resilience score", y="", title="Resilience attribute scorecard") +
  # Theme
  theme_bw()
g

# Export score card
ggsave(g, filename=file.path(datadir, "score_card.pdf"), 
       width=8.5, height=11, units="in", dpi=600)
