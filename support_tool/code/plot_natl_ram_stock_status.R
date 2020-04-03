
# For testing: data <- ram; cntry <- "United States"
plot_natl_ram_stock_status <- function(data, cntry){
  
  # Format data
  data <- data %>% 
    mutate(label=ifelse(country_use==cntry, cntry, "Other country"))
  
  # Plot data
  g <- ggplot(data, aes(x=bbmsy_cap, y=uumsy_cap, color=label)) +
    geom_point() + 
    # Labels
    labs(x=expression("B/B"["MSY"]), y=expression("U/U"["MSY"]),
         title=paste("RAM stock status:", cntry)) +
    scale_x_continuous(breaks=0:4, labels=c(0:3, ">4")) +
    scale_y_continuous(breaks=0:4, labels=c(0:3, ">4")) +
    scale_color_manual(name="", values=c("black", "grey80")) +
    # Add lines
    geom_hline(yintercept = 1) +
    geom_vline(xintercept = 1) +
    theme_bw() +
    theme(legend.position = "bottom",
          legend.title=element_blank())
  g
  
}
  






