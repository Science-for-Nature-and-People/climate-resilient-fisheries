

# For testing: cntry <- "Japan"
plot_natl_genus_nutrition_data <- function(genus_pdiet, genus_pnutrient, cntry){
  
  # Seafood consumption time series
  g1 <- ggplot(genus_pdiet %>% filter(country_use==cntry),
               aes(x=year, y=seafood_g_person_day)) +
    geom_line() +
    labs(x="Year", y="Daily per capita marine seafood supply\n(grams / person / day)", title=paste("GENuS seafood nutrition:", cntry)) +
    theme_bw()
  #g1  
  
  # Importance of seafood to nutrient supply
  g2 <- ggplot(genus_pnutrient %>% filter(country_use==cntry), 
               aes(x=reorder(nutrient, prop_seafood), y=prop_seafood)) +
    geom_bar(stat="identity") +
    labs(x="", y="Percent of nutrient supply\nfrom marine seafood") +
    scale_y_continuous(labels = scales::percent) +
    # scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    coord_flip() +
    theme_bw()
  #g2 
  
  # Merge plots
  g <- gridExtra::grid.arrange(g1, g2, ncol=2)
 
}