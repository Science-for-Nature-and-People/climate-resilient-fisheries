# ------------------#
# case-study-plot.R
# Reads the list of current case studies
# Plots location and other dimensions
# W.Friedman 3/20
# ------------------#

library(leaflet)
library(leaflet.esri)
library(dplyr)
library(tidyr)
library(sf)
library(ggplot2)
library(htmltools)
library(here)
library(readxl)
library(janitor)

case_fn = "Case_Study_Selection.xlsx"
case_raw <- read_excel(here("case_studies",case_fn)) %>% 
                         clean_names()

# V1: Simple Map ---- 
# Sites
sites <- case_raw

m <- leaflet(data = sites) %>%
  addProviderTiles("Esri.OceanBasemap") %>% 
  setView(lat=0, lng=0 , zoom=2) %>%
  addMarkers(~lon, ~lat,
             popup = paste(paste("Fishery:", sites$fishery),
                           paste("Type:", str_to_sentence(sites$type_of_fishery_pelagic_demersal_mixed)),
                           paste("Resilience:", str_to_sentence(sites$resilience_low_high)),
                           paste("Management scale:", str_to_sentence(sites$management_scale_local_national_transboundary)),
                           paste("Stressors:", str_to_sentence(sites$response_event)),
                           paste0('Link: <a href="',as.character(sites$link),'">More Information</a>'),
                           sep = "<br/>"))

m # view

saveWidget(m, file=here("case_studies","Case_study_sites.html"))



# V2: More complicated ----
world_shapefile_path = "/Users/friedman/Documents/Projects/Mapping/world_shape_file/"

# Read the world shapefile with sf: 
world=read_sf(world_shapefile_path)
#plot(world) # class(world)

# Join cases with world
cases <- case_raw %>% 
  mutate(NAME = str_to_title(country)) %>% 
  mutate(NAME = recode(NAME,
                       "Usa" = "United States",
                       "Myanmar" = "Burma"))

cases$NAME[!cases$NAME %in% world$NAME]

cases <- full_join(world, cases, by="NAME") # 3 were dropped. return to this. 
cases['study_site'] <- NA
cases$study_site[!is.na(cases$country)] <- 1

# Sites
sites <- case_raw
#sites <- st_as_sf(case_raw,
#                  coords = c("lon","lat"),
#                  crs = 4326)

# Create a color palette for the map:
mypalette = col_numeric(palette="YlGnBu", domain=cases$study_site, na.color="transparent")
mytext=paste("Country: ", str_to_title(cases$country), sep="") %>%
  lapply(htmltools::HTML)

m <- leaflet(data = sites) %>%
  addProviderTiles("Esri.OceanBasemap") %>% 
  setView( lat=0, lng=0 , zoom=2) %>%
  #addProviderTiles("NASAGIBS.ViirsEarthAtNight2012") %>%
  #addTiles(options = tileOptions(color="gray"))  %>% #
  addPolygons(data = cases,
              group = "fishery",
              fillColor = ~mypalette(study_site),stroke=TRUE, fillOpacity = .5, color="white", weight=.3) %>% 
  addMarkers(~lon, ~lat,
             popup = paste(paste("Fishery:", sites$fishery),
                           paste("Type:", str_to_sentence(sites$type_of_fishery_pelagic_demersal_mixed)),
                           paste("Resilience:", str_to_sentence(sites$resilience_low_high)),
                           paste("Ecosystem type:", str_to_sentence(sites$ecosystem_type)),
                           paste("Management scale:", str_to_sentence(sites$management_scale_local_national_transboundary)),
                           paste("Governance system:", str_to_sentence(sites$governance_system)),
                           paste("Dependence on fisheries:", str_to_sentence(sites$dependence_on_fisheries_percent_gdp_number_jobs_food)),
                           paste("Stressors:", str_to_sentence(sites$response_event)),
                           paste("Data availability:",str_to_sentence(sites$degrees_of_scientific_information_data_limited_data_rich)),
                           paste0('Link: <a href="',as.character(sites$link),'">More Information</a>'),
                           sep = "<br/>"))

m # view

saveWidget(m, file=here("case_studies","Case_study_sites_v2.html"))
