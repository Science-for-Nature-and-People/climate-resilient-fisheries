
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
datadir <- "case_study_analysis/cf_scripts/data"
plotdir <- "case_study_analysis/cf_scripts/figures"

# Read data
data_orig <- readxl::read_excel(file.path(datadir, "case_study_meta_data.xlsx")) 


# Build data
################################################################################

# Format data
data <- data_orig %>% 
  # Format scale
  mutate(scale=factor(scale, levels=c("small", "medium", "large"))) %>% 
  # Format case study name
  mutate(name_label=recode(name,
                           "Galacia stalked barnacles"="Galacia\nbarnacles",
                           "US West Coast Pacific sardine"="US West Coast\nPacific sardine",
                           "NE Atlantic pelagic"="NE Atlantic\npelagics",
                           "Kiribati giant clam"="Kiribati\ngiant clam",
                           "California Dungeness crab"="California\nDungeness crab",
                           "Fiji nearshore"="Fiji\nnearshore",
                           "Madagascar reef fish"="Madagascar\nreef fish",
                           "Bering Sea groundfish"="Bering Sea\ngroundfish",
                           "Japan common squid"="Japan\ncommon squid",
                           "Juan Fernandez Islands"="Juan Fernandez\nIslands",
                           "Madang reef fish"="Madang\nreef fish",
                           "Iceland groundfish"="Iceland\ngroundfish",
                           "Maine lobster"="Maine\nlobster",
                           "Tasmania rock lobster"="Tasmania\nrock lobster",
                           "Senegalese small pelagics"="Senegalese\nsmall pelagics",
                           "Japanese spiny lobster"="Japanese\nspiny lobster",
                           "US Atlantic pelagic longline"="US Atlantic\npelagic longline",
                           "Moorea coral reef"="Moorea\ncoral reef")) %>% 
  # Add hjust
  mutate(hjust=recode(name,
                      "Galacia stalked barnacles"="1",
                      "US West Coast Pacific sardine"="1",
                      "NE Atlantic pelagic"="0",
                      "Kiribati giant clam"="1",
                      "California Dungeness crab"="0",
                      "Fiji nearshore"="1",
                      "Madagascar reef fish"="0",
                      "Bering Sea groundfish"="0",
                      "Japan common squid"="0",
                      "Juan Fernandez Islands"="1",
                      "Madang reef fish"="1",
                      "Iceland groundfish"="1",
                      "Maine lobster"="0",
                      "Tasmania rock lobster"="0",
                      "Japanese spiny lobster"="1",
                      "Senegalese small pelagics"="0",
                      "US Atlantic pelagic longline"="0",
                      "Moorea coral reef"="1") %>% as.numeric())


# Plot data
################################################################################

# Projection
wgs84 <- sf::st_crs("+proj=longlat +datum=WGS84")
moll <- sf::st_crs("+proj=moll +datum=WGS84")
rob <- sf::st_crs("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

# Make sites a SF object
data_sf <- sf::st_as_sf(data, coords = c("long_dd", "lat_dd"), crs = wgs84) %>% 
  sf::st_transform(wgs84)

# World
world <- rnaturalearth::ne_countries(scale="small", returnclass = "sf") %>% 
  sf::st_transform(wgs84)

# Theme
my_theme <-  theme(axis.text=element_blank(),
                   axis.title=element_blank(),
                   legend.text=element_text(size=8),
                   legend.title=element_text(size=9),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.position = "bottom",
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot() +
  geom_sf(data=world, fill="grey80", color="white", lwd=0.2) +
  # Fisheries
  geom_sf(data=data_sf, mapping=aes(size=scale, color=type)) +
  geom_sf_text(data=data_sf, mapping=aes(label=name_label, hjust=hjust), size=2.5, lineheight=0.8) +
  # geom_point(data=data, mapping=aes(x=long_dd, y=lat_dd, size=scale, color=type)) +
  # geom_text(data=data, mapping=aes(x=long_dd, y=lat_dd, label=name_label, hjust=hjust), size=2.5, lineheight=0.8) +
  # Labels
  labs(x="", y="") +
  # Crop
  coord_sf(ylim=c(-55,80)) +
  # Legend
  scale_color_discrete(name="Fishery type") +
  scale_size_ordinal(name="Fishery scale") +
  # Theme
  theme_bw() + my_theme
g

# Export plot
ggsave(g, filename=file.path(plotdir, "Fig1_case_study_map.png"), 
       width=6.5, height=3.25, units="in", dpi=600)
 



