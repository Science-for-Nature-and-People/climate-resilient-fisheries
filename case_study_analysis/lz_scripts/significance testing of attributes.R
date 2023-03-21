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


# Build data for kruskal test of sig. diff of score among attributes and importance among attributes
################################################################################

###temporarily remove "-" from Cross-scale for easier analysis later on

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




################## Kruskal test of difference in score among attributes

kruskal.test(score ~ attribute,
             data = data)

################### post-doc Dunn Test with bh correction method 
DT_score = dunnTest(score ~ attribute,
              data=data,
              method="bh")      


############### df with only significant adjusted p-values
PT = DT_score$res
score_pairs<- PT%>% 
  filter(P.adj < .05) %>% arrange(P.adj)


library(tidyr)
library(reshape2)
  

############################ break up comparison column 
df_significant_score_difference<-  score_pairs %>% separate_wider_delim(Comparison, "-", names = c("attribute_1", "attribute_2"))

df_significant_score_difference<- as.data.frame(df_significant_score_difference)

######keep only adjusted p-values and attributes being compared
df_diff_score<- df_significant_score_difference %>% select(attribute_1, attribute_2, P.adj)


##########add back in "-" in cross-scale integration

df_diff_score <- data.frame(lapply(df_diff_score, function(x) {
  gsub("Crossscale integration", "Cross-scale integration", x)
}))


df_diff_score$P.adj<- as.numeric( df_diff_score$P.adj) #as numeric for P.adjusted


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

##########create heat map of p-values for significant pair-wise comparisons of score by attribute where p adjusted < 0.05
pairwise_comparison_of_scores<- df_diff_score %>% 
ggplot(aes(x=attribute_2, y=attribute_1, fill=P.adj)) + 
  geom_raster() +
  scale_fill_viridis_c(option = "G")+theme(axis.text.x = element_text(angle = 90))+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.background = element_blank()) +guides(fill=guide_legend(title="adjusted p-value")) +my_theme+ theme(legend.position = "none")





################## Kruskal test of difference in importance among attributes
kruskal.test(importance_numeric~ attribute,
             data = data)


################### post-doc Dunn Test with bh correction method 
DT_importance = dunnTest(importance_numeric ~ attribute,
                    data=data,
                    method="bh")      


############### df with only significant adjusted p-values
PT_imp = DT_importance$res
PT_imp
pairs_imp<- PT_imp%>% 
  filter(P.adj < .05) %>% arrange(P.adj)


############################ break up comparison column 
df_significant_imp_difference<-  pairs_imp %>% separate_wider_delim(Comparison, "-", names = c("attribute_1", "attribute_2"))

df_significant_imp_difference<- as.data.frame(df_significant_imp_difference)


df_diff_imp<- df_significant_imp_difference %>% select(attribute_1, attribute_2, P.adj)


##########add back in "-" in cross-scale integration


df_diff_imp <- data.frame(lapply(df_diff_imp, function(x) {
  gsub("Crossscale integration", "Cross-scale integration", x)
}))

df_diff_imp$P.adj<- as.numeric(df_diff_imp$P.adj) #p-value as numeric 


##########create heat map of p-values for significant pair-wise comparisons of importance by attribute where p adjusted < 0.05

pairwise_comparison_of_imp<- df_diff_imp %>% 
  ggplot(aes(x=attribute_2, y=attribute_1, fill=P.adj)) + 
  geom_raster() +
  scale_fill_viridis_c(option = "G")+theme(axis.text.x = element_text(angle = 90))+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.background = element_blank()) +guides(fill=guide_legend(title="adjusted p-value")) +my_theme



library(cowplot)


supp_fig_comparisons<- plot_grid( pairwise_comparison_of_imp, pairwise_comparison_of_scores, labels = c('A', 'B'))



ggsave(supp_fig_comparisons, filename=file.path(plotdir, "Final_supp_fig_comparisons.png"), 
       width=7, height=4, units="in", dpi=600)




#To test for significant differences of score among attributes,as well as importance, we used non-parametric Kruskal–Wallis tests  
#followed by post-hoc Dunn multiple comparisons tests. 

