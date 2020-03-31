
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)
library(countrycode)
library(rnaturalearth)

# Directories
plotdir <- "support_tool_work/figures"

# Read COMTRADE data
comtrade_orig <- readRDS("/Users/cfree/Dropbox/Chris/UCSB/data/comtrade/data/BACI_HS12_2017_data.Rds")
load("/Users/cfree/Dropbox/Chris/UCSB/data/comtrade/data/BACI_HS12_country_and_hscode_keys.Rdata")

# Build country centroids
################################################################################

# Get world
world <- rnaturalearth::ne_countries(scale="small", returnclass = "sf")

# Format world
world_simple <- world %>% 
  # Reduce columns
  select(admin, adm0_a3) %>% 
  rename(country_orig=admin, iso3_orig=adm0_a3) %>% 
  # Correct ISO3s/countrys
  mutate(country_use=countrycode(iso3_orig, "iso3c", "country.name"),
         iso3_use=countrycode(country_use, "country.name", "iso3c")) %>% 
  # Replace missing values
  mutate(country_use=ifelse(is.na(country_use), country_orig, country_use),
         iso3_use=ifelse(is.na(iso3_use), iso3_orig, iso3_use))

# Get centroids
world_centroids <- world_simple %>% 
  # Calculate centroid
  sf::st_centroid(of_largest_polygon=T) %>% 
  # Extract centroids
  mutate(lat_dd = unlist(map(geometry,1)),
         long_dd = unlist(map(geometry,2)))

# Format country codes
country_key <- country_codes %>% 
  select(code, iso3_use, country_use) %>% 
  left_join(world_centroids %>% select(iso3_use, lat_dd, long_dd) %>% sf::st_drop_geometry(), by="iso3_use")


# Build data
################################################################################

# Read HS codes
# Fish: 30111 - 30579
# Crustaceans: 30611 - 30629
# Molluscs: 30711 - 30799
# Aquatic invertebrates: 30811 - 30890
# Oils of fish: 150410
# Fats and oils and their fractions: 150420
# Fats and oils and their fractions: 150430
# Fish preparations: 160411-160432
# Crustacean preparations: 160510 - 160540
# Mollusc preparations: 160551 - 160559	
# Aquatic invertebrates: 160561 - 160569
seafood_codes <- c(30111:30579,
                   30611:30629,
                   30711:30799,
                   30811:30890,
                   160411-160432,
                   160510:160540,
                   160551:160559,
                   160561:160569)

# Summarize
comtrade <- comtrade_orig %>% 
  # Reduce to seafood commodities
  filter(hs6_code %in% seafood_codes) %>% 
  # Summarize flow of seafood commodities
  group_by(exporter_code, importer_code) %>% 
  summarize(quantity_mt=sum(quantity_mt)) %>% 
  # Add exporter country info
  left_join(country_key, by=c("exporter_code"="code")) %>% 
  rename(exporter_country=country_use, exporter_iso3=iso3_use, 
         exporter_lat_dd=lat_dd, exporter_long_dd=long_dd) %>% 
  # Add importer country info
  left_join(country_key, by=c("importer_code"="code")) %>% 
  rename(importer_country=country_use, importer_iso3=iso3_use, 
         importer_lat_dd=lat_dd, importer_long_dd=long_dd) %>% 
  ungroup()


# Plot data
################################################################################

# Nice example of circular trade plot:
# https://gjabel.wordpress.com/2014/03/28/circular-migration-flow-plots-in-r/

# Country
cntry <- "Colombia"

# Subset/format exports
sdata1 <- comtrade %>% 
  filter(exporter_country==cntry) %>% 
  select(importer_code, importer_iso3, importer_country, quantity_mt) %>% 
  mutate(flow_dir="Exports") %>% 
  rename(code=importer_code, iso3=importer_iso3, country=importer_country) %>% 
  select(flow_dir, code, iso3, country, quantity_mt)

# Subset/format imports
sdata2 <- comtrade %>% 
  filter(importer_country==cntry) %>% 
  select(exporter_code, exporter_iso3, exporter_country, quantity_mt) %>% 
  mutate(flow_dir="Imports") %>% 
  rename(code=exporter_code, iso3=exporter_iso3, country=exporter_country) %>% 
  select(flow_dir, code, iso3, country, quantity_mt)

# Format for plotting
world_data1 <- world_simple %>% 
  left_join(sdata1, by=c("iso3_use"="iso3")) %>% 
  mutate(flow_dir="Exports")
# Format for plotting
world_data2 <- world_simple %>% 
  left_join(sdata2, by=c("iso3_use"="iso3")) %>% 
  mutate(flow_dir="Imports")
world_data <- rbind(world_data1, world_data2)

# Plot data
g <- ggplot(world_data, aes(fill=quantity_mt/1000)) +
  facet_wrap(~flow_dir, ncol=1) +
  geom_sf(color="white", lwd=0.1) +
  geom_sf(data=world_centroids %>% filter(country_use==cntry), mapping=aes(fill=NULL), color="black") +
  labs(title=paste("Comtrade bilateral trade flows:", cntry)) +
  scale_fill_gradientn(name="Marine seafood\ntrade volume (1000s mt)", 
                       colors=rev(RColorBrewer::brewer.pal(n=9, name="RdBu")), na.value="grey80") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  theme_bw() +
  theme(legend.position = "bottom")
g

# Export data
ggsave(g, filename=file.path(plotdir, "Fig3_bilateral_trade_flows.png"), 
       width=6.5, height=7, units="in", dpi=600)




