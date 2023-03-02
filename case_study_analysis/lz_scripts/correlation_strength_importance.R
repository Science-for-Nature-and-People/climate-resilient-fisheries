
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(ggplot2)
library(tidyverse)
library(ggtext)
library(here)
library(glue)
# Directories
datadir <- "case_study_analysis/clean_data"
plotdir <- "case_study_analysis/lz_scripts"

# Read data
data_orig <- readRDS(file.path(datadir, "case_study_attribute_score_data.Rds"))


# Build data for typology
################################################################################

data <- data_orig %>% 
  # Simplify and convert importance to numeric
  select(case_study, dimension, attribute, score, importance) %>% 
  mutate(importance=stringr::str_to_sentence(importance),
         importance=factor(importance, levels=c("Low", "Medium", "High"))) %>% 
  mutate(importance_numeric= as.numeric(as.factor(importance)))




cor.test(data$score, data$importance_numeric, method = "spearman",
         exact = FALSE) 


cor_meta<- data %>% select(dimension, attribute) %>% 
  distinct()

 cor_data<-data %>% 
  group_by(attribute) %>% 
  summarize(cor=cor(importance_numeric, score,  method = "sp"))

 

 cor_data<- full_join(cor_data, cor_meta) 
 
 cor_data<-  cor_data %>% 
   mutate(ab_cor =abs(cor)) %>% arrange(cor) 
 
 
 #check significance for different attributes
 
 test_cor<-data %>% 
 filter(attribute=="Social capital")
 
   cor.test(test_cor$score, test_cor$importance_numeric, method = "spearman",
            exact = FALSE) 

   # p-value = 0.09464
          
  # rho 
  # 0.4059209 
  
 test_cor<-data %>% 
    filter(attribute=="Leadership and initiative")
 
  cor.test(test_cor$score, test_cor$importance_numeric, method = "spearman",
           exact = FALSE) 
  
 #p-value = 0.03157

 # rho 
  #0.507468 
  
  test_cor<-data %>% 
    filter(attribute=="Flexible and agile infrastructure")
  cor.test(test_cor$score, test_cor$importance_numeric, method = "spearman",
           exact = FALSE) 
  
#  p-value = 0.03149
#  rho 
#  0.5076942 

  
  cor_data<-  cor_data %>% 
    mutate(significance =case_when(ab_cor >=.50 ~ "p-value < 0.05",  
                                   ab_cor <.50 ~ "p-value > 0.05"))
  

 
#######################################################3
#create theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.title=element_text(size=7),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=7),
                   strip.text=element_text(size=7),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.position="bottom",
                   legend.background = element_rect(fill=alpha('blue', 0)))



cor_data$attribute <- as.factor(cor_data$attribute) 
cor_data<- cor_data %>% mutate(attribute=fct_reorder(attribute,ab_cor))

            
bold.attribute <- c("Leadership and initiative", "Flexible and agile infrastructure", "Adult mobility", "Population modularity",            
                     "Habitat diversity",   
                     "Technology transfer",              
                     "Plasticity",                       
                     "Larval dispersal",                 
                     "Social diversity",                  
                     "Species diversity",   
                     "Place attachment")    

bold.labels <- ifelse(levels(cor_data$attribute) %in% bold.attribute, yes = "bold", no = "plain")


corr_plot<- ggplot(data = cor_data, aes(x = attribute, y = cor, size = ab_cor)) + 
  geom_point(color='black', shape=21, aes(fill=dimension, alpha=ab_cor)) + 
  scale_fill_manual(values = c( "Ecological" = "#72B077", "Governance" = "#C25866", "Socio-economic" = "#D6B65D")) + 
 scale_y_continuous(limits = c(-.5, 1), breaks = c(-1, -0.5, 0, 0.5,.7, 1))+
  theme_bw() + my_theme +  theme(axis.text.y = element_text(face = bold.labels)) + guides(size = "none", alpha="none") + coord_flip() +facet_wrap(~dimension)+
  ylab(" Spearman rank correlation between attribute strength and importance") + 
  geom_hline(yintercept = 0, linetype = 'dotted', col = 'grey') + geom_hline(yintercept = .5, linetype = 'dashed', col = 'grey50') +geom_hline(yintercept = .7, linetype = 'dashed', col = 'grey19')
# theme(panel.background = element_rect(fill='transparent'), #transparent panel background
        #plot.background = element_rect(fill='transparent', color=NA))




#add back in geom_text(aes(label = attribute), size = 2, vjust = 1, hjust = .5) to see attribute labels. Version here saved without labels and labels added in manually afterwards 
ggsave(corr_plot, filename=file.path(plotdir, "corr_strength_importance.png"), 
       width=5.5, height=4.5, units="in", dpi=600)
