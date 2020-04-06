
# For testing: data <- fao; cntry <- "United States"
plot_natl_fao_catch_data <- function(data, cntry, my_theme){
  
  # Subset data
  sdata <- data %>% 
    filter(country_use==cntry) %>% 
    gather(key="prod_type", value="prod_mt", 5:6) %>% 
    mutate(prod_type=recode_factor(prod_type, "catch_mt"="Landings", meat_mt="Edible meat"))
  
  # Plot data
  g <- ggplot(sdata, aes(x=year, y=prod_mt/1000, fill=isscaap)) +
    facet_wrap(~prod_type, ncol=2) +
    geom_area() + 
    labs(x="", y="Production (1000s mt)", title=paste("FAO reported catch:", cntry)) +
    scale_x_continuous(limits=c(1950, 2020), breaks=seq(1950,2020,10)) +
    theme_bw() + my_theme +
    theme(legend.title = element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0))
  g

}