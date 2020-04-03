
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
plotdir <- "support_tool_work/figures"
appdatadir <- "support_tool/data"

# Read FAO data
fao_orig <- readRDS("/Users/cfree/Dropbox/Chris/UCSB/data/fao/aquaculture/processed/1950_2017_fao_aquaculture_data.Rds")


# Build data
################################################################################

# Build data
table(fao_orig$environment)
table(fao_orig$major_group)
fao <- fao_orig %>% 
  # Marine/brackish only
  filter(environment!="Freshwater") %>% 
  # Remove non-fish/crustaceans/bivalves/echinoderms
  filter(major_group %in% c("Pisces", "Crustacea", "Mollusca", "Invertebrata aquatica")) %>% 
  # Add edible meat conversion
  rename(prod_mt=quantity_mt) %>% 
  mutate(pedible=recode(major_group, 
                        "Pisces"=0.87, 
                        "Crustacea"=0.36, 
                        "Mollusca"=0.17, 
                        "Invertebrata aquatica"=0.21),
         meat_mt=prod_mt * pedible) %>% 
  # Sum catch and edible meat by country-group-year
  group_by(country_use, iso3_use, isscaap, year) %>% 
  summarize(prod_mt=sum(prod_mt, na.rm=T),
            meat_mt=sum(meat_mt, na.rm=T))

# Export data
saveRDS(fao, file.path(appdatadir, "FAO_1950_2017_mariculture_data_by_country_isscaap.Rds"))

  
# Plot data
################################################################################

# Theme
my_theme <- theme(axis.text=element_text(size=8),
                  axis.title=element_text(size=10),
                  plot.title=element_text(size=10),
                  legend.text=element_text(size=6),
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"))

# Subset data
cntry <- "Colombia"
sdata <- fao %>% 
  filter(country_use==cntry) %>% 
  gather(key="prod_type", value="prod_mt", 5:6) %>% 
  mutate(prod_type=recode_factor(prod_type, "prod_mt"="Total production", meat_mt="Edible meat"))

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

# Export plot
ggsave(g, filename=file.path(plotdir, "Fig2_marine_aquaculture_production.png"), 
       width=6.5, height=3, units="in", dpi=600)



