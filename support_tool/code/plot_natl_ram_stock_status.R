
# For testing: data <- ram; cntry <- "Albania"
plot_natl_ram_stock_status <- function(data, cntry, my_theme){
  
  # Format data
  data <- data %>% 
    mutate(label=ifelse(country_use==cntry, cntry, "Other countries"),
           label=factor(label, levels=c(cntry, "Other countries"))) 
  
  # Plot data
  g <- ggplot(data, aes(x=bbmsy_cap, y=uumsy_cap, color=label, size=msy_mt/1000)) +
    geom_point() + 
    # Labels
    labs(x=expression("B/B"["MSY"]), y=expression("U/U"["MSY"]),
         title=paste("RAM stock status:", cntry)) +
    scale_x_continuous(breaks=0:4, labels=c(0:3, ">4")) +
    scale_y_continuous(breaks=0:4, labels=c(0:3, ">4")) +
    scale_color_manual(name="Country", values=c("black", "grey80"), drop=F) +
    scale_size_continuous(name="MSY (1000s mt)", range=c(2,8)) +
    # Add lines
    geom_hline(yintercept = 1) +
    geom_vline(xintercept = 1) +
    theme_bw() + my_theme + 
    theme(legend.position = "right")
  g
  
}
  






