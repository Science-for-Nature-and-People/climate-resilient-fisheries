

# # Get world
# world <- rnaturalearth::ne_countries(scale="small", returnclass = "sf")
# 
# # Format world
# world_simple <- world %>% 
#   # Reduce columns
#   select(admin, adm0_a3) %>% 
#   rename(country_orig=admin, iso3_orig=adm0_a3) %>% 
#   # Correct ISO3s/countrys
#   mutate(country_use=countrycode(iso3_orig, "iso3c", "country.name"),
#          iso3_use=countrycode(country_use, "country.name", "iso3c")) %>% 
#   # Replace missing values
#   mutate(country_use=ifelse(is.na(country_use), country_orig, country_use),
#          iso3_use=ifelse(is.na(iso3_use), iso3_orig, iso3_use))
# 
# # Get centroids
# world_centroids <- world_simple %>% 
#   # Calculate centroid
#   sf::st_centroid(of_largest_polygon=T) %>% 
#   # Extract centroids
#   mutate(lat_dd = unlist(map(geometry,1)),
#          long_dd = unlist(map(geometry,2)))

# Format country codes
# country_key <- country_codes %>% 
#   select(code, iso3_use, country_use) %>% 
#   left_join(world_centroids %>% select(iso3_use, lat_dd, long_dd) %>% sf::st_drop_geometry(), by="iso3_use")


# For testing: data <- comtrade; cntry <- "United States"
plot_natl_comtrade_trade_data <- function(data, world_simple, world_centroids, cntry){
  
  # Subset/format exports
  sdata1 <- data %>% 
    filter(exporter_country==cntry) %>% 
    select(importer_code, importer_iso3, importer_country, quantity_mt) %>% 
    mutate(flow_dir="Exports") %>% 
    rename(code=importer_code, iso3=importer_iso3, country=importer_country) %>% 
    select(flow_dir, code, iso3, country, quantity_mt)
  
  # Subset/format imports
  sdata2 <- data %>% 
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
    labs(title=paste("Comtrade bilateral trade:", cntry)) +
    scale_fill_gradientn(name="Marine seafood\ntrade volume (1000s mt)", 
                         colors=rev(RColorBrewer::brewer.pal(n=9, name="RdBu")), na.value="grey80") +
    guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
    theme_bw() +
    theme(legend.position = "bottom")
  g

}