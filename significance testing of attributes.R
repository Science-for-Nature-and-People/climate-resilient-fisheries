# Packages
library(ggplot2)
library(tidyverse)
library(ggtext)
library(here)
library(glue)
library(FSA)
library(stringr)
# Directories
datadir <- "case_study_analysis/clean_data"
plotdir <- "case_study_analysis/lz_scripts"

# Read data
data_orig <- readRDS(file.path(datadir, "case_study_attribute_score_data.Rds"))


# Build data for typology
################################################################################

data_orig <- data.frame(lapply(data_orig, function(x) {
  gsub("Cross-scale integration", "Crossscale integration", x)
}))

library(dplyr)

data <- data_orig %>% 
  # Simplify and convert importance to numeric
  select(case_study, dimension, attribute, score, importance) %>% 
  mutate(importance=stringr::str_to_sentence(importance),
         importance=factor(importance, levels=c("Low", "Medium", "High"))) %>% 
  mutate(importance_numeric= as.numeric(as.factor(importance))) %>% 
  mutate(score= as.numeric(score))




kruskal.test(score ~ attribute,
             data = data)


DT_score = dunnTest(score ~ attribute,
              data=data,
              method="bh")      


PT = DT_score$res
PT
score_pairs<- PT%>% 
  filter(P.adj < .05) %>% arrange(P.adj)


library(tidyr)
library(reshape2)
  
  
df_significant_score_difference<-  score_pairs %>% separate_wider_delim(Comparison, "-", names = c("attribute_1", "attribute_2"))

df_significant_score_difference<- as.data.frame(df_significant_score_difference)


df_diff_score<- df_significant_score_difference %>% select(attribute_1, attribute_2, P.adj)

#df_diff_score_m <-acast(df_significant_score_difference, attribute_1~attribute_2, value.var="P.adj")




df_diff_score <- data.frame(lapply(df_diff_score, function(x) {
  gsub("Crossscale integration", "Cross-scale integration", x)
}))

df_diff_score$P.adj<- as.numeric( df_diff_score$P.adj)

df_diff_score %>% 
ggplot(aes(x=attribute_2, y=attribute_1, fill=P.adj)) + 
  geom_raster() +
  scale_fill_viridis_c()+theme(axis.text.x = element_text(angle = 90))+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.background = element_blank())
 

#df_diff_score<- as.data.frame(df_diff_score)




##############################3-----------
kruskal.test(importance_numeric~ attribute,
             data = data)

DT_importance = dunnTest(importance_numeric ~ attribute,
                    data=data,
                    method="bh")      


PT_imp = DT_importance$res
PT_imp
pairs_imp<- PT_imp%>% 
  filter(P.adj < .05) %>% arrange(P.adj)


df_significant_imp_difference<-  pairs_imp %>% separate_wider_delim(Comparison, "-", names = c("attribute_1", "attribute_2"))

df_significant_imp_difference<- as.data.frame(df_significant_imp_difference)


df_diff_imp<- df_significant_imp_difference %>% select(attribute_1, attribute_2, P.adj)

#df_diff_imp_m <-acast(df_significant_imp_difference, attribute_1~attribute_2, value.var="P.adj")




df_diff_imp <- data.frame(lapply(df_diff_imp, function(x) {
  gsub("Crossscale integration", "Cross-scale integration", x)
}))

df_diff_imp$P.adj<- as.numeric(df_diff_imp$P.adj)

df_diff_imp %>% 
  ggplot(aes(x=attribute_2, y=attribute_1, fill=P.adj)) + 
  geom_raster() +
  scale_fill_viridis_c()+theme(axis.text.x = element_text(angle = 90))+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.background = element_blank())



#To test for significant differences of score among attributes,as well as importance, we used non-parametric Kruskal–Wallis tests  
#followed by post-hoc Dunn multiple comparisons tests. 

