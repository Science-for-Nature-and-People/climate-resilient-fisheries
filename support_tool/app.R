
# Clear
rm(list = ls())

# Setup
################################################################################

# Packages
library(sf)
library(lwgeom)
library(shiny)
library(shinyjs)
library(shinythemes)
library(tidyverse)
library(RColorBrewer)
library(rgdal)
library(leaflet)
library(countrycode)
library(rnaturalearth)

# Read data
datadir <- "data" # for actual app
codedir <- "code"
# datadir <- "support_tool/data" # when testing
# codedir <- "support_tool/code" # when testing
textdir <- "app_text"

# Parameters
ocean_vars <- c("Temperature", "pH", "Salinity", "Dissolved oxygen", "Primary productivity")
checklist_options <- c("Not important", "Well-implemented", "Could be enhanced")

# Read code
sapply(list.files(codedir), function(x) source(file.path(codedir, x)))

# Read data
fao <- readRDS(file.path(datadir, "FAO_1950_2017_catch_data_by_country_isscaap.Rds"))
fao_maq <- readRDS(file.path(datadir, "FAO_1950_2017_mariculture_data_by_country_isscaap.Rds"))
comtrade <- readRDS(file.path(datadir, "Comtrade_2017_seafood_trade_data.Rds"))
genus_pdiet <- readRDS(file.path(datadir, "GENUS_1960_2011_pdiet_seafood_by_country.Rds"))
genus_pnutrient <- readRDS(file.path(datadir, "GENUS_2011_pnutrient_seafood_by_country.Rds"))
ram <- readRDS(file.path(datadir, "RAM_v4.491_stock_status.Rds"))

# Load world data
load(file.path(datadir, "rnaturalearth_world_data.Rdata"))

# Countries
fao_countries <- sort(unique(fao$country_use))

# Plotting theme
my_theme <- theme(axis.text=element_text(size=12),
                  axis.title=element_text(size=14),
                  plot.title=element_text(size=16),
                  strip.text=element_text(size=14),
                  legend.text=element_text(size=12),
                  legend.title=element_text(size=14),
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"))


# User interface
################################################################################

# User interface
ui <- navbarPage("Climate-resilient fisheries",
  
  # Data portal
  #############################################################      
  
  # Overview
  tabPanel("Overview",
           
           p("This tool is designed to help marine fisheries stakeholders assess the resilience of their fishery to climate change and to identify strategies for further enhancing resilience. Throughout the tool, we define resilience as the ability of a fisheries system to resist, recover, adapt, and transform constructively. The tool provides four features:"),
          
           tags$ol(   
            tags$li("A data portal for the user to explore the projected impacts of climate change on the biogeochemistry of their waters and the projected impacts of these changes on their fisheries."),
            tags$li("A resilience checklist for the user to self-assess the ecological, social, economic, and governance attributes of their fishery that make it resilient to climate change and the attributes that could be targeted for enhanced resilience. This checklist seeks to guide the user in identifying barriers, bottlenecks, opportunities, and leverage points in enhancing the resilience of their fishery to climate change."),
            tags$li("A menu of potential resilience enhancing tools that address the objectives of the user and the needs of the fishery. The tool seeks to present context-specific recommendations and case studies from similar contexts to provide operational guidance on enhancing climate resilience in fisheries."),
            tags$li("A list of case studies illustrating the use of the resilience checklist to diagnose the resilience of fisheries to climate change and identify opportunities for enhancing resilience to climate change. These case studies also detail the attributes and actions that make the case study fisheries resilient to climate change and provide useful examples for enhancing resilience in analogous fisheries.")
          ),
           
          p("This tool was developed as part of a Science for Nature and People Partnership (SNAPP) Working Group convened on Climate Resilient Fisheries. SNAPP Working Groups are funded by The Nature Conservancy, Wildlife Conservation Society, and the National Center for Ecological Analysis and Synthesis. All data and code for the tool are on GitHub here."),
           
          p("Recommended citation: SNAPP CRF Working Group (2020) Climate resilient fisheries support tool. Available online at: https://emlab-ucsb.shinyapps.io/crf_support_tool/")
  ),
  
  # Data portal
  #############################################################
  
  # Data portal
  navbarMenu("Data portal",
  
  tabPanel("Oceanography",
           
           h2("Oceanography"),
           
           p("Climate change is altering ocean ecosystems with cascading impacts on the fish and invertebrates that depend on these ecosystems to the people that depend on these resources as a source of food and income. On this page, we illustrate how climate change is expected to alter the biogeochemistry of the coastal waters that support the majority of marine fisheries."), 
           
           p("These climate projections are drawn from the Coupled Model Intercomparison Project Phase 5
           (CMIP5) which provides a coordinated framework for earth system models to make climate projections using the same set of climate experiments. Here, we present the results of four increasingly greenhouse gas concentrations adopted by the IPCC known as the Representative Concentration Pathways (RCPs)"), 
           
           p("To begin, select a country from the dropdown menu below."),
           
           # Select a country
           selectInput("country1",
                       "Select a country:",
                       choices=fao_countries, multiple=F, selectize=F),
           
           # Select a variable
           selectInput("ovean_var",
                       "Select a variable:",
                       choices=ocean_vars, multiple=F, selectize=F),
           
           # Select a climate scenario
           selectInput("clim_scen",
                       "Select a climate scenario:",
                       choices=paste("RCP", c("2.6", "4.5", "6.0", "8.5")), multiple=F, selectize=F),
           
           # Mao
           leafletOutput("ocean_clim_proj_map", width=1000, height=600),
           br(),
           br()
           
           ),
  
  # Data portal > Fisheries
  tabPanel("Marine fisheries",
           
           # Overview
           ##########################################################
           
           # Overview
           h2("Marine fisheries"),
           p("This page allows the user to explore the historical importance of national marine fisheries and the projected impacts of climate change on catch from these fisheries in scenarios with and without climate-adaptive fisheries management reforms. The first section uses data drawn from global datasets to explore historical seafood production (FAO 2018; Edwards et al. 2017), international seafood trade (UN 2020), nutrient provisioning (Smith et al. 2016), and the current status of marine fisheries (Ricard et al. 2012; Hilborn et al. 2020). The second section uses projections of fisheries biomass, catch, and profits under climate change from Free et al. (2020) to explore the extent to which climate-adaptive fisheries reforms could mitigate the negative impacts of climate change."),
           p("To begin, select a country from the dropdown menu below."),
           
           # Select a country
           selectInput("country2",
                       "Select a country:",
                       choices=fao_countries, multiple=F, selectize=F),
           
           # Importance of marine fisheries
           ##########################################################
           
           # Importance of marine fisheries
           h2("Importance of marine fisheries"),
           
           # FAO fisheries
           h3("Marine fisheries production"),
           p("In 2016, [country name] landed XXXX mt of marine seafood yielding XXXX mt of edible meat. The five most important species by landed volume were: species name (Scientific name), species name (Scientific name), species name (Scientific name), species name (Scientific name), and species name (Scientific name)."),
           br(),
           plotOutput(outputId = "plot_natl_fao_catch_data", width=1000, height=400),
           p("Figure 1. Historical (B) capture fisheries landings (FAO 2018) and B) the edible meat derived from these landings. Edible meat represents the edible portion of the cultured organism (i.e., weight without the head, bones, shells, guts, etc.) and was calculated using conversion factors from Edwards et al. (2017)."),
           br(),
           
           # FAO mariculture
           h3("Marine aquaculture production"),
           p("In 2016, [country name] farmed XXXX mt of marine seafood in marine and brackishwater aquaculture yielding XXXX mt of edible meat. The five most important species by harvested volume were: species name (Scientific name), species name (Scientific name), species name (Scientific name), species name (Scientific name), and species name (Scientific name)."),
           br(),
           plotOutput(outputId = "plot_natl_fao_mariculture_data", width=1000, height=400),
           p("Historical (A) mariculture production (FAO 2018) and (B) the edible meat derived from this production. Edible meat represents the edible portion of the cultured organism (i.e., weight without the head, bones, shells, guts, etc.) and was calculated using conversion factors from Edwards et al. (2017)."),
           br(),
           
           # Comtrade trade flows
           h3("International marine seafood trade"),
           p("In 2016, [country name] exported XXXX mt of marine seafood products and imported XXXX mt of marine seafood products making it a net [export/importer]. The majority of its exports were to [country name], [country name], and [country name] while the majority of its imports were from [country name], [country name], and [country name]."),
           br(),
           plotOutput(outputId = "plot_natl_comtrade_trade_data", width=800, height=800),
           p("Figure 3. The top panel shows the countries to which the selected country exported marine seafood in 2017 and the bottom panel shows the countries from which the selected country imported marine seafood in 2017. In both panels, the volume of trade (1000s mt) is represented by the fill color. These data are from the UN Comtrade database (United Nations 2020)."),
           br(),
           
           # GENuS nutrition
           h3("Nutritional importance of marine seafood"),
           p("In 2011, the most recent year with data, X% of daily per capita protein consumption in [country name] came from marine seafood sourced from domestic fisheries, domestic mariculture, and/or international trade. This ranks X out of XXX countries globally."),
           br(),
           plotOutput(outputId = "plot_natl_genus_nutrition_data", width=1000, height=400),
           p("Figure 4. This figure shows (A) the daily per capita supply of marine seafood from 1960 to 2011 and (B) the median proportion of daily per capita nutrient supply derived from marine seafood in 2011. These data are derived from the Global Expanded Nutrient Supply (GENuS) database (Smith et al. 2016). In both plots, marine seafood is defined as coming from the following GENuS database food groups: marine fish (other), pelagic fish, demersal fish, molluscs (other), crustaceans, fish body oil, and fish liver oil. This excludes the following aquatic sources of “seafood”: freshwater fish, aquatic animals (other), and aquatic plants."),
           br(),
           
           # Status of marine fisheries
           h3("Status of marine fisheries"),
           p("XX stocks representing approximately XX% of reported catch are managed using stock assessments available in the RAM Legacy Database (Ricard et al. 2012; Hilborn et al. 2020). Of these stocks, XX (XX%) are overfished (B/BMSY < 1.0) and XX (XX%) are experiencing overfishing (U/UMSY > 1.0). XX stocks (XX%) are neither overfished nor experiencing overfishing."),
           br(),
           plotOutput(outputId = "plot_natl_ram_stock_status", width=750, height=600),
           p("Figure 5. A ‘Kobe plot’ showing the status of stocks in the RAM Legacy Stock Assessment Database (Ricard et al. 2012; Hilborn et al. 2020). Each point represents the status of stock in the most recent year with data and stocks from the nation of interest are shown in black (if any are available). The x-axis shows B/BMSY which is the ratio of the most recent biomass (B) relative to the target biomass (BMSY) and indicates whether a stock is overfished. Stocks with B/BMSY values less than 1.0 are at biomass levels below the target biomass. In the United States, stocks with B/BMSY values less than 0.5 are considered overfished. The FAO considers stocks with B/BMSY values less than 0.8 to be overfished. The y-axis shows U/UMSY which is the ratio of the most recent fishing mortality rate (U) relative to the target fishing mortality rate (UMSY) and indicates whether a stock is experiencing overfishing. Stocks with U/UMSY values greater than 1.0 are experiencing overfishing while stocks with U/UMSY values less than 1.0 are not experiencing overfishing. "),
           
           # Projected impacts of climate change and fisheries reform
           ##########################################################
           
           h2("Projected impacts of climate change and fisheries reform"),
           
           
           p("Climate change is expected to result in shifts in both the spatial distribution of fish stocks (i.e., where fish can be caught and by whom) and the productivity of fish stocks (i.e., how much fish can be caught). In general, productivity is expected to decrease in tropical and temperate regions and increase towards the poles as species distributions shift to follow their preferred temperatures."),
            
           p("The impact of these changes on human society could be either limited or exacerbated by human responses to these changes. On one hand, fisheries management reforms that end overfishing, respond to shifts in productivity through adaptive harvest control rules, and promote better international cooperation to prevent stock degradation as species shift across boundaries could mitigate many of the negative impacts of climate change. On the other hand, business-as-usual management that maintains current levels of fishing pressure and increases overfishing as stocks shift into new jurisdictions could exacerbate the negative impacts of climate change."),
            
           p("Free et al. (2020) evaluated how catch and profits from national fisheries would change under climate change with and without the reforms described above. Specifically, they forecasted shifts in species distributions using a bioclimatic envelope model (i.e., assume that fish track their preferred temperatures) and assumed that changes in range size are proportional to changes in productivity. In this way, they were able to estimate how maximum sustainable yield might change in every country’s waters under four increasingly severe climate scenarios (RCPs 2.6, 4.5, 6.0, 8.5)."),
            
           p("The figure below shows how maximum sustainable yield (MSY) is projected to change in [country name] under climate change."),
            
           p("FIGURE HERE"),
            
           p("However, the change in catch from fisheries does not necessarily match in underlying productivity because few fisheries are managed precisely at MSY. By ending overfishing, overfished fisheries could see increases in catch if climate-driven losses are small. Similarly, by expanding undeveloped fisheries, underfished fisheries could see increases in catch if climate-driven losses are small. On the other hand, failing to correct over or underfishing could lead to even lower catches than would be predicted by the change in MSY alone."), 
            
           p("The figure below shows how Free et al. (2020) predict catch would change in [country name] under climate change and two management scenarios: (1) full adaptation that ends overfishing, responds to shifts in productivity, and promote better international cooperation to prevent stock degradation as species shift across boundaries and (2) business-as-usual management that maintains current levels of fishing pressure and increases overfishing as stocks shift into new jurisdictions."),
            
           p("FIGURE HERE"),
            
           p("The results of the “Full Adaptation” scenario represent the estimated maximum of what could be accomplished (in terms of catch) through the implementation of the resilience enhancement tools identified in this web application."),
            
           p("To explore the global- and country-level results of Free et al. (2020) in greater detail, please visit this web application: https://emlab-ucsb.shinyapps.io/fishcast2/"),

           
           # References
           ##########################################################
           
           h2("References"),
           p("Edwards, P., Zhang, W., Belton, B., Little, D.C. (2019) Misunderstandings, myths and mantras in aquaculture: its contribution to world food supplies has been systematically over reported. Marine Policy 106: 103547. https://doi.org/10.1016/j.marpol.2019.103547"),
           p("FAO (2018) The State of World Fisheries and Aquaculture 2018 - Meeting the sustainable development goals. Food and Agricultural Organization (FAO) of the United Nations: Rome, Italy."),
           p("Free, C.M., Mangin, T., García Molinos, J., Ojea, E., Burden, M., Costello, C., Gaines, S.D. (2020) Realistic fisheries management reforms could mitigate the impacts of climate change in most countries. PLoS One 15(3): e0224347. https://doi.org/10.1371/journal.pone.0224347"),
           p("Hilborn, R., Amoroso, R.O., Anderson, C.M., Baum, J.K., Branch, T.A., Costello, C., de Moor, C.L., Faraj, A., Hively, D., Jensen, O.P., Kurota, H. (2020) Effective fisheries management instrumental in improving fish stock status. Proceedings of the National Academy of Sciences 117(4): 2218-2224. https://doi.org/10.1073/pnas.1909726116"),
           p("Ricard, D., Minto, C., Jensen, O.P., Baum, J.K. (2012) Examining the knowledge base and status of commercially exploited marine species with the RAM Legacy Stock Assessment Database. Fish and Fisheries 13(4): 380-398."),
           p("Smith, M.R., Micha, R., Golden, C.D., Mozaffarian, D., Myers, S.S. (2016) Global Expanded Nutrient Supply (GENuS) model: a new method for estimating the global dietary supply of nutrients. PLoS One 11(1): e0146976. https://doi.org/10.1371/journal.pone.0146976"),
           p("United Nations (2020) UN Comtrade database. United Nations (UN). Available online at: https://comtrade.un.org/"),
           br(),
           br(),
  
           ), 
  
  tabPanel("Other indicators", 
           
           h2("Other indicators"),
           p("Text and figure coming to computers near you.")
           
  )),
  
  # Resilience checklist
  #############################################################
  
  # Resilience checklist
  tabPanel("Resilience checklist", 
           
           # Overview
           h2("Overview"),
           p("This page provides a checklist of ecological, social, economic, and governance attributes shown to make marine fisheries resilient to climate change. The checklist allows the user to self-assess the resilience of their fishery to climate change by indicating whether an attribute is: (1) well-implemented in their fishery; (2) could be enhanced in their fishery; or (3) is not an important or relevant attribute of resilience in their fishery. After completing this checklist, the user can press the “Identify potential strategies for enhancing resilience” button to generate a list of actions that could be taken to enhance resilience to climate change. These actions are drawn from our database of resilience tools and from our case studies."),
           
           # Checklist
           h2("Resilience checklist"),
           
           # Ecological resilience
           ########################################
           
           # Ecological resilience
           h3("Ecological resilience attributes"),
           
           # Attribute 1
           h5("Attribute 1"),
           p("Insert brief description of resilience attribute here."),
           radioButtons(inputId="att1", 
                        label=NULL, 
                        choices = checklist_options, 
                        selected = checklist_options[1],
                        inline = T, 
                        width = NULL, 
                        choiceNames = NULL,
                        choiceValues = NULL),
           
           # Attribute 2
           h5("Attribute 2"),
           p("Insert brief description of resilience attribute here."),
           radioButtons(inputId="att2", 
                        label=NULL, 
                        choices = checklist_options, 
                        selected = checklist_options[1],
                        inline = T, 
                        width = NULL, 
                        choiceNames = NULL,
                        choiceValues = NULL),
           
           # Attribute 3
           h5("Attribute 3"),
           p("Insert brief description of resilience attribute here."),
           radioButtons(inputId="att3", 
                        label=NULL, 
                        choices = checklist_options, 
                        selected = checklist_options[1],
                        inline = T, 
                        width = NULL, 
                        choiceNames = NULL,
                        choiceValues = NULL),
           
           # Attribute 4
           h5("Attribute 4"),
           p("Insert brief description of resilience attribute here."),
           radioButtons(inputId="att4", 
                        label=NULL, 
                        choices = checklist_options, 
                        selected = checklist_options[1],
                        inline = T, 
                        width = NULL, 
                        choiceNames = NULL,
                        choiceValues = NULL),
           
           # Attribute 5
           h5("Attribute 5"),
           p("Insert brief description of resilience attribute here."),
           radioButtons(inputId="att5", 
                        label=NULL, 
                        choices = checklist_options, 
                        selected = checklist_options[1],
                        inline = T, 
                        width = NULL, 
                        choiceNames = NULL,
                        choiceValues = NULL),
           
           # Social resilience
           ########################################
           
           # Social resilience
           h3("Social resilience attributes"),
           
           # Attribute 6
           h5("Attribute 6"),
           p("Insert brief description of resilience attribute here."),
           radioButtons(inputId="att6", 
                        label=NULL, 
                        choices = checklist_options, 
                        selected = checklist_options[1],
                        inline = T, 
                        width = NULL, 
                        choiceNames = NULL,
                        choiceValues = NULL),
           
           # Attribute 7
           h5("Attribute 7"),
           p("Insert brief description of resilience attribute here."),
           radioButtons(inputId="att7", 
                        label=NULL, 
                        choices = checklist_options, 
                        selected = checklist_options[1],
                        inline = T, 
                        width = NULL, 
                        choiceNames = NULL,
                        choiceValues = NULL),
           
           # Attribute 8
           h5("Attribute 8"),
           p("Insert brief description of resilience attribute here."),
           radioButtons(inputId="att8", 
                        label=NULL, 
                        choices = checklist_options, 
                        selected = checklist_options[1],
                        inline = T, 
                        width = NULL, 
                        choiceNames = NULL,
                        choiceValues = NULL),
           
           # Attribute 9
           h5("Attribute 9"),
           p("Insert brief description of resilience attribute here."),
           radioButtons(inputId="att9", 
                        label=NULL, 
                        choices = checklist_options, 
                        selected = checklist_options[1],
                        inline = T, 
                        width = NULL, 
                        choiceNames = NULL,
                        choiceValues = NULL),
           
           # Attribute 10
           h5("Attribute 10"),
           p("Insert brief description of resilience attribute here."),
           radioButtons(inputId="att10", 
                        label=NULL, 
                        choices = checklist_options, 
                        selected = checklist_options[1],
                        inline = T, 
                        width = NULL, 
                        choiceNames = NULL,
                        choiceValues = NULL),
           
           # Economic resilience
           ########################################
           
           # Economic resilience
           h3("Economic resilience attributes"),
           
           # Attribute 11
           h5("Attribute 11"),
           p("Insert brief description of resilience attribute here."),
           radioButtons(inputId="att11", 
                        label=NULL, 
                        choices = checklist_options, 
                        selected = checklist_options[1],
                        inline = T, 
                        width = NULL, 
                        choiceNames = NULL,
                        choiceValues = NULL),
           
           # Attribute 12
           h5("Attribute 12"),
           p("Insert brief description of resilience attribute here."),
           radioButtons(inputId="att12", 
                        label=NULL, 
                        choices = checklist_options, 
                        selected = checklist_options[1],
                        inline = T, 
                        width = NULL, 
                        choiceNames = NULL,
                        choiceValues = NULL),
           
           # Attribute 13
           h5("Attribute 13"),
           p("Insert brief description of resilience attribute here."),
           radioButtons(inputId="att13", 
                        label=NULL, 
                        choices = checklist_options, 
                        selected = checklist_options[1],
                        inline = T, 
                        width = NULL, 
                        choiceNames = NULL,
                        choiceValues = NULL),
           
           # Attribute 14
           h5("Attribute 14"),
           p("Insert brief description of resilience attribute here."),
           radioButtons(inputId="att14", 
                        label=NULL, 
                        choices = checklist_options, 
                        selected = checklist_options[1],
                        inline = T, 
                        width = NULL, 
                        choiceNames = NULL,
                        choiceValues = NULL),
           
           # Attribute 15
           h5("Attribute 15"),
           p("Insert brief description of resilience attribute here."),
           radioButtons(inputId="att15", 
                        label=NULL, 
                        choices = checklist_options, 
                        selected = checklist_options[1],
                        inline = T, 
                        width = NULL, 
                        choiceNames = NULL,
                        choiceValues = NULL),
           
           # Governance resilience
           ########################################
           
           # Governance resilience
           h3("Governance resilience attributes"),
           
           # Attribute 16
           h5("Attribute 16"),
           p("Insert brief description of resilience attribute here."),
           radioButtons(inputId="att16", 
                        label=NULL, 
                        choices = checklist_options, 
                        selected = checklist_options[1],
                        inline = T, 
                        width = NULL, 
                        choiceNames = NULL,
                        choiceValues = NULL),
           
           # Attribute 17
           h5("Attribute 17"),
           p("Insert brief description of resilience attribute here."),
           radioButtons(inputId="att17", 
                        label=NULL, 
                        choices = checklist_options, 
                        selected = checklist_options[1],
                        inline = T, 
                        width = NULL, 
                        choiceNames = NULL,
                        choiceValues = NULL),
           
           # Attribute 18
           h5("Attribute 18"),
           p("Insert brief description of resilience attribute here."),
           radioButtons(inputId="att18", 
                        label=NULL, 
                        choices = checklist_options, 
                        selected = checklist_options[1],
                        inline = T, 
                        width = NULL, 
                        choiceNames = NULL,
                        choiceValues = NULL),
           
           # Attribute 19
           h5("Attribute 19"),
           p("Insert brief description of resilience attribute here."),
           radioButtons(inputId="att19", 
                        label=NULL, 
                        choices = checklist_options, 
                        selected = checklist_options[1],
                        inline = T, 
                        width = NULL, 
                        choiceNames = NULL,
                        choiceValues = NULL),
           
           # Attribute 20
           h5("Attribute 20"),
           p("Insert brief description of resilience attribute here."),
           radioButtons(inputId="att20", 
                        label=NULL, 
                        choices = checklist_options, 
                        selected = checklist_options[1],
                        inline = T, 
                        width = NULL, 
                        choiceNames = NULL,
                        choiceValues = NULL),
           
           # Submit checklist
           br(),
           # submitButton("Save resilience checklist", width=400),
           br(),
           # submitButton("Identify potential strategies for enhancing resilience", width=400),
           br(),
           br()
           
  ), 
  
  
  # Resilience enhancing tools
  #############################################################
  
  # Resilience enhancing tools
  tabPanel("Resilience enhancing tools",
           
           # Overview
           h2("Overview"),
           p("The table below lists tools for enhancing the resilience of fisheries to climate change by targeting the attributes indicated as “could be enhanced” on the resilience checklist page. The table provides a brief description of the tool, a link to learn more about the tool from its primary source, and the country in which the tool was implemented. These tools are drawn from our case studies of fisheries exhibiting resilience to climate change and from our systematic review of the literature for tools that enhance the resilience of marine fisheries to climate change."),
           br(),
           
           # Overview
           h2("Resilience enhancing tools"),
           p("Insert dynamic table here providing tool recommendations based on the results of the reslience checklist."),
           br()
           
  ),
  
  # Case studies
  tabPanel("Case studies")

  
)


# Server
################################################################################

# Server
server <- function(input, output){
  
  # Climate projection map
  output$ocean_clim_proj_map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap)
  })
  
  # FAO catch data
  output$plot_natl_fao_catch_data <- renderPlot({
    g <- plot_natl_fao_catch_data(data=fao, 
                                  cntry=input$country2, my_theme=my_theme)
    g
  })
  
  # FAO mariculture data
  output$plot_natl_fao_mariculture_data <- renderPlot({
    g <- plot_natl_fao_mariculture_data(data=fao_maq, 
                                        cntry=input$country2, my_theme=my_theme)
    g
  })
  
  # Comtrade trade data
  output$plot_natl_comtrade_trade_data <- renderPlot({
    g <- plot_natl_comtrade_trade_data(data=comtrade, world_simple, world_centroids, 
                                       cntry=input$country2, my_theme=my_theme)
    g
  })
  
  # Genus nutrition data
  output$plot_natl_genus_nutrition_data <- renderPlot({
    g <- plot_natl_genus_nutrition_data(genus_pdiet, genus_pnutrient, 
                                        cntry=input$country2, my_theme=my_theme)
    g
  })
  
  # RAM stock status
  output$plot_natl_ram_stock_status <- renderPlot({
    g <- plot_natl_ram_stock_status(data=ram, cntry=input$country2,
                                    my_theme=my_theme)
    g
  })
  

}

shinyApp(ui = ui, server = server)
