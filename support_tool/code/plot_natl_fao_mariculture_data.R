
# For testing: data <- fao_maq; cntry <- "United States"
plot_natl_fao_mariculture_data <- function(data, cntry, my_theme){
  
  # Subset data
  sdata <- data %>% 
    # Subset
    filter(country_use==cntry) %>% 
    # Convert wide to long
    gather(key="prod_type", value="prod_mt", 5:6) %>% 
    # Spread and gather to add missing years
    spread(key="year", value="prod_mt") %>% 
    gather(key="year", value="prod_mt", 5:ncol(.)) %>% 
    mutate(prod_mt=ifelse(!is.na(prod_mt), prod_mt, 0)) %>% 
    # Rename production type
    mutate(prod_type=recode_factor(prod_type, "prod_mt"="Total production", meat_mt="Edible meat"),
           year=as.numeric(year)) %>% 
    ungroup()
  
  # Plot data
  g <- ggplot(sdata, aes(x=year, y=prod_mt/1000, fill=isscaap)) +
    facet_wrap(~prod_type, ncol=2) +
    geom_area() + 
    labs(x="", y="Production (1000s mt)", title=paste("FAO reported production:", cntry)) +
    scale_x_continuous(limits=c(1950, 2020), breaks=seq(1950,2020,10)) +
    theme_bw() + my_theme +
    theme(legend.title = element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0))
  g

}