
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)
library(countrycode)

# Directories
plotdir <- "support_tool_work/figures"

# Read RAM Legacy Database v4.491
load("/Users/cfree/Dropbox/Chris/UCSB/data/ramldb/RAM v4.491 Files (1-14-20)/RAM v4.491/DB Files With Assessment Data/R Data/DBdata[asmt][v4.491].Rdata")


# Build data
################################################################################

data <- timeseries_values_views %>% 
  # Reduce to stocks/years with B/BMSY and U/UMSY
  select(stockid, year, BdivBmsypref, UdivUmsypref) %>% 
  filter(!is.na(BdivBmsypref) & !is.na(UdivUmsypref)) %>% 
  # Select most recent year with both
  group_by(stockid) %>% 
  arrange(desc(year)) %>% 
  slice(1) %>% 
  ungroup() %>% 
  # Rename columns
  rename(bbmsy=BdivBmsypref, uumsy=UdivUmsypref) %>% 
  # Add stock meta-data
  left_join(stock, by="stockid") %>% 
  # Arrange columns
  select(-c(inmyersdb, myersstockid, state, tsn)) %>% 
  select(stockid, stocklong, region, areaid, primary_country, primary_FAOarea, 
         commonname, scientificname, year, bbmsy, uumsy, everything()) %>% 
  # Rename columns
  rename(country_orig=primary_country, fao_area=primary_FAOarea, comm_name=commonname, 
         species=scientificname) %>% 
  # Add ISO3 and correct country name
  mutate(iso3_use=countrycode(country_orig, "country.name", "iso3c"),
         country_use=countrycode(iso3_use, "iso3c", "country.name")) %>% 
  # Arrange
  select(stockid:country_orig, country_use, iso3_use, everything()) %>% 
  # Cap values
  mutate(bbmsy_cap=pmin(bbmsy, 4),
         uumsy_cap=pmin(uumsy, 4))

# Inspect
colnames(data) 
str(data)

# Plot data
################################################################################

# Format data
cntry <- "Japan" 
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

# Export plot
ggsave(g, filename=file.path(plotdir, "Fig5_ram_stock_status.png"), 
       width=4.5, height=4.5, units="in", dpi=600)




