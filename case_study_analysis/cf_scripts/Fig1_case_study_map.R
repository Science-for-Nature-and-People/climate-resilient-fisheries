
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
  mutate(scale=stringr::str_to_sentence(scale),
         scale=factor(scale, levels=c("Small", "Large")),
         type=stringr::str_to_sentence(type)) %>% 
  # Format case study name
  mutate(name_label=recode(name,
                           "Galicia stalked barnacle"="Galicia\nstalked barnacle",
                           "US West Coast Pacific sardine"="US West Coast\nPacific sardine",
                           "NE Atlantic small pelagics"="NE Atlantic\nsmall pelagics",
                           "Kiribati giant clam"="Kiribati\ngiant clam",
                           "California Dungeness crab"="California\nDungeness crab",
                           "Fiji nearshore"="Fiji\nnearshore",
                           "Madagascar nearshore"="Madagascar\nnearshore",
                           "US Bering Sea groundfish"="US Bering Sea\ngroundfish",
                           "Hokkaido set-net"="Hokkaido\nset-net",
                           "Juan Fernandez Islands demersal"="Juan Fernandez\nIslands demersal",
                           "Madang reef fish"="Madang\nreef fish",
                           "Iceland groundfish"="Iceland\ngroundfish",
                           "Maine American lobster"="Maine\nAmerican lobster",
                           "Tasmania rock lobster"="Tasmania\nrock lobster",
                           "Senegal small pelagics"="Senegal\nsmall pelagics",
                           "Mie spiny lobster"="Mie\nspiny lobster",
                           "US Atlantic and Gulf migratory pelagics"="US Atlantic & Gulf\nmigratory pelagics",
                           "Moorea reef fish"="Moorea\nreef fish")) %>% 
  # Add hjust
  mutate(hjust=recode(name,
                      "Galicia stalked barnacle"="0",
                      "US West Coast Pacific sardine"="1",
                      "NE Atlantic small pelagics"="0",
                      "Kiribati giant clam"="1",
                      "California Dungeness crab"="0",
                      "Fiji nearshore"="1",
                      "Madagascar nearshore"="0",
                      "US Bering Sea groundfish"="0",
                      "Hokkaido set-net"="0",
                      "Juan Fernandez Islands demersal"="1",
                      "Madang reef fish"="1",
                      "Iceland groundfish"="1",
                      "Maine American lobster"="0",
                      "Tasmania rock lobster"="0",
                      "Mie spiny lobster"="1",
                      "Senegal small pelagics"="0",
                      "US Atlantic and Gulf migratory pelagics"="0",
                      "Moorea reef fish"="1") %>% as.numeric(),
         vjust=ifelse(name=="Kiribati giant clam", 0, 0.5)) %>% 
  # Adjust hjust
  mutate(hjust=ifelse(hjust==0, -0.15, 1.15))


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
                   legend.text=element_text(size=9),
                   legend.title=element_text(size=10),
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
  geom_sf(data=data_sf, mapping=aes(size=scale, fill=type), pch=21, stroke=0.3, color="grey20") +
  geom_sf_text(data=data_sf, mapping=aes(label=name_label, hjust=hjust, vjust=vjust), size=2.5, lineheight=0.8) +
  # geom_point(data=data, mapping=aes(x=long_dd, y=lat_dd, size=scale, color=type)) +
  # geom_text(data=data, mapping=aes(x=long_dd, y=lat_dd, label=name_label, hjust=hjust), size=2.5, lineheight=0.8) +
  # Labels
  labs(x="", y="") +
  # Crop
  coord_sf(ylim=c(-55,80)) +
  # Legend
  # scale_color_discrete(name="Fishery type") +
  scale_fill_manual(name="Fishery type", values=RColorBrewer::brewer.pal(9, "Blues")[c(2,7)]) +
  # scale_size_ordinal(name="Fishery scale") + 
  scale_size_manual(name="Fishery scale", values=c(2, 4)) +
  guides(size = guide_legend(order = 1), fill = guide_legend(order = 2)) +
  # Theme
  theme_bw() + my_theme
g

# Export plot
ggsave(g, filename=file.path(plotdir, "Fig1_case_study_map.png"), 
       width=6.5, height=3.25, units="in", dpi=600)
 



