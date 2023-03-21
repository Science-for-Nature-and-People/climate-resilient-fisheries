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



#To test for significant differences of score among attributes,as well as importance, and quality we used non-parametric Kruskal–Wallis tests  
#followed by post-hoc Dunn multiple comparisons tests. 


# Build data for kruskal test of sig. diff of score among attributes and importance among attributes
################################################################################

###temporarily remove "-" from Cross-scale for easier analysis later on

data_orig <- data.frame(lapply(data_orig, function(x) {
  gsub("Cross-scale integration", "Crossscale integration", x)
}))

library(dplyr)

data <- data_orig %>% 
  # Simplify and convert importance to numeric
  select(case_study, dimension, attribute, score, importance, quality) %>% 
  mutate(importance=stringr::str_to_sentence(importance),
         importance=factor(importance, levels=c("Low", "Medium", "High"))) %>% 
  mutate(importance_numeric= as.numeric(as.factor(importance))) %>% 
  mutate(score= as.numeric(score)) %>% 
mutate(quality=recode(quality, 
                      "A - adequate and reliable data/information"="Excellent",
                      "B - limited data/information and expert judgement"="Good",
                      "C - fairly confident that the answer provided reflects the true state of the system"="Fair",
                      "D - not confident that the answer provided reflects the true state of the system"="Low",
                      "E - No data"="No data"),
       quality=factor(quality, levels=c("No data", "Low", "Fair", "Good", "Excellent"))) %>% 
  # Add numeric quality
  mutate(quality_num=as.numeric(quality))



################# SCORE####################################################


################## Kruskal test of difference in **score** among attributes

kruskal.test(score ~ attribute,
             data = data)

library(FSA)
################### post-doc Dunn Test with bh correction method 
DT_score = dunnTest(score ~ attribute,
              data=data,
              method="bh")      

library(dplyr)

library(tidyr)
library(reshape2)
############### df with only significant adjusted p-values
score_pairs = DT_score$res

############################ break up comparison column 
df_diff_score<-  score_pairs %>% separate_wider_delim(Comparison, "-", names = c("attribute_1", "attribute_2"))

df_diff_score<- as.data.frame(df_diff_score) %>% select(attribute_1, attribute_2, P.adj)

##########add back in "-" in cross-scale integration

df_diff_score <- data.frame(lapply(df_diff_score, function(x) {
  gsub("Crossscale integration", "Cross-scale integration", x)
}))

df_diff_score$P.adj<- as.numeric( df_diff_score$P.adj) #as numeric for P.adjusted

df_diff_score$attribute_1<- factor(df_diff_score$attribute_1)
levels(df_diff_score$attribute_1)

df_diff_score$attribute_2<- as.factor(df_diff_score$attribute_2)
levels(df_diff_score$attribute_2)

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
pairwise_comparison_of_scores<- 
  df_diff_score %>% 
  filter(P.adj < .05) %>% #n=102
  complete(attribute_1, attribute_2) %>% 
ggplot(aes(x=attribute_2, y=attribute_1)) + 
  geom_tile(aes(fill=(P.adj)),colour = 'black')+
  scale_fill_viridis_c(option = "G")+theme(axis.text.x = element_text(angle = 90))+
  scale_y_discrete(limits=rev)+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.background = element_blank())  +guides(fill=guide_legend(title="Adjusted p-value")) +my_theme


####save figure
ggsave(pairwise_comparison_of_scores, filename=file.path(plotdir, "FigS3_pairwise_comparison_of_scores.png"), 
       width=6, height=5, units="in", dpi=600)


################# IMPORTANCE####################################################


################## Kruskal test of difference in **importance** among attributes
kruskal.test(importance_numeric~ attribute,
             data = data)


################### post-doc Dunn Test with bh correction method 
DT_importance = dunnTest(importance_numeric ~ attribute,
                    data=data,
                    method="bh")      


############### df with only significant adjusted p-values
imp_pairs = DT_importance$res


############################ break up comparison column 
df_diff_imp<-  imp_pairs %>% separate_wider_delim(Comparison, "-", names = c("attribute_1", "attribute_2"))

df_diff_imp<- as.data.frame(df_diff_imp) %>% select(attribute_1, attribute_2, P.adj)

##########add back in "-" in cross-scale integration

df_diff_imp <- data.frame(lapply(df_diff_imp, function(x) {
  gsub("Crossscale integration", "Cross-scale integration", x)
}))

df_diff_imp$P.adj<- as.numeric( df_diff_imp$P.adj) #as numeric for P.adjusted

df_diff_imp$attribute_1<- factor(df_diff_imp$attribute_1)
levels(df_diff_imp$attribute_1)

df_diff_imp$attribute_2<- as.factor(df_diff_imp$attribute_2)
levels(df_diff_imp$attribute_2)


##########create heat map of p-values for significant pair-wise comparisons of imp by attribute where p adjusted < 0.05
pairwise_comparison_of_imp<- 
  df_diff_imp %>% 
  filter(P.adj < .05) %>% #n=68 
  complete(attribute_1, attribute_2) %>% 
  ggplot(aes(x=attribute_2, y=attribute_1)) + 
  geom_tile(aes(fill=(P.adj)),colour = 'black')+
  scale_fill_viridis_c(option = "G")+theme(axis.text.x = element_text(angle = 90))+
  scale_y_discrete(limits=rev)+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.background = element_blank())  +guides(fill=guide_legend(title="Adjusted p-value")) +my_theme


####save figure
ggsave(pairwise_comparison_of_imp, filename=file.path(plotdir, "FigS2_pairwise_comparison_of_imp.png"), 
       width=6, height=5, units="in", dpi=600)


################# DATA QUALITY###################################################


################## Kruskal test of difference in **quality** among attributes

kruskal.test(quality_num ~ attribute,
             data = data)


#Kruskal-Wallis rank sum test
#data:  quality by attribute
#Kruskal-Wallis chi-squared = 73.923, df = 37, p-value = 0.0002948




################### post-doc Dunn Test with bh correction method 
DT_quality = dunnTest(quality_num ~ attribute,
                         data=data,
                         method="bh")      


############### df with only significant adjusted p-values
quality_pairs = DT_quality$res


############################ break up comparison column 
df_diff_quality<-  quality_pairs %>% separate_wider_delim(Comparison, "-", names = c("attribute_1", "attribute_2"))

df_diff_quality<- as.data.frame(df_diff_quality) %>% select(attribute_1, attribute_2, P.adj)

##########add back in "-" in cross-scale integration

df_diff_quality <- data.frame(lapply(df_diff_quality, function(x) {
  gsub("Crossscale integration", "Cross-scale integration", x)
}))

df_diff_quality$P.adj<- as.numeric( df_diff_quality$P.adj) #as numeric for P.adjusted

df_diff_quality$attribute_1<- factor(df_diff_quality$attribute_1)
levels(df_diff_quality$attribute_1)

df_diff_quality$attribute_2<- as.factor(df_diff_quality$attribute_2)
levels(df_diff_quality$attribute_2)


##########create heat map of p-values for significant pair-wise comparisons of quality by attribute where p adjusted < 0.05
pairwise_comparison_of_quality<- 
  df_diff_quality %>% 
  filter(P.adj < .05) %>% #n=16
  complete(attribute_1, attribute_2) %>% 
  ggplot(aes(x=attribute_2, y=attribute_1)) + 
  geom_tile(aes(fill=(P.adj)),colour = 'black')+
  scale_fill_viridis_c(option = "G")+theme(axis.text.x = element_text(angle = 90))+
  scale_y_discrete(limits=rev)+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.background = element_blank())  +guides(fill=guide_legend(title="Adjusted p-value")) +my_theme


####save figure
ggsave(pairwise_comparison_of_quality, filename=file.path(plotdir, "FigS5_pairwise_comparison_of_quality.png"), 
       width=6, height=5, units="in", dpi=600)










