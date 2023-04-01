# 02-figure-s2-corrplot.R
# Code to create figure S2 - attribute correlations
# W. Friedman // 03-2023

# Setup
################################################################################

# Clear workspace
rm(list = ls())

# Load libraries
source(here("case_study_analysis","make_figures","00-load-libraries.R"))
library(corrplot)
library(lme4)

# Directories
plotdir <- here("case_study_analysis","make_figures", "plots")

# Build data
source(here("case_study_analysis","make_figures","01-build-data.R"))


# Q. Can we treat score and importance as independent variables?  -----
# A. Overall, there is a null/weak correlation between importance and score. 
# Among all attribs & case studies, rho = 0.302

data %>% 
  select(score, importance_numeric) %>% 
  cor(use = "pairwise.complete.obs", method = "spearman")

# However, particular attributes (n = 11/38) showed individually strong correlations
# (this should be the same as those in bi-plot)
data %>% 
  filter(attribute == "Species diversity") %>% 
  select(score, importance_numeric) %>% 
  cor(use = "pairwise.complete.obs", method = "spearman")

data %>% 
  ggplot(aes(x = score, y = importance_numeric))+
  geom_jitter(width = 0.1, height = 0.1)+
  facet_wrap(~case_study)

data %>% 
  ggplot(aes(x = score, y = importance_numeric))+
  geom_jitter(width = 0.1, height = 0.1)+
  facet_wrap(~attribute)

mdat <- data %>% 
  mutate(case_study = factor(case_study))

# repeated attributes
attribs <- data$attrib_dim %>% unique()
attrib_corrs <- list()
for (a in attribs){
  this_corr <- data %>% 
    filter(attrib_dim == a) %>% 
    select(score, importance_numeric) %>% 
    cor(use = "pairwise.complete.obs", method = "spearman")
  attrib_corrs[a] <- this_corr[2,1]
}

attrib_corrs %>% as_tibble %>% t() %>% view()

# repeated attribs; linear model check
library(nlme)
mod1 <- lm(score ~ importance_numeric, 
           data = mdat)

mod2 <- lme(score ~ importance_numeric,
            random=~1|attribute,data=mdat)
anova(mod1)
anova(mod2)
summary(mod2)


# Correlation among attributes based on SCORE ----
score_corrs <- data %>% 
  select(case_study,attrib_dim, score) %>%
  pivot_wider(names_from = "attrib_dim", values_from = "score") %>% 
  select(-case_study) %>% 
  cor(use = "pairwise.complete.obs", method = "spearman")


png(here(plotdir,"Figure_S2_attrib_score_correlations.png"), width = 9.5, height = 8, units = "in", res = 300)

score_corrs %>% 
  corrplot(method = "color", addCoef.col = 'gray20', diag = F, tl.cex = .6, number.cex = 0.4, 
           tl.col = "gray20")

dev.off()
