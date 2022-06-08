# Clean Data Files

#Load libraries
library(ggplot2)
library(tidyverse)
library(janitor)
library(googlesheets4)
library(here)
library(kableExtra)


# Read in the data from Google Sheets
##########################################################################################################################

aguion_barnacle <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1BFTzr_ITZPHZ0-tdItB13VorTMrSfI9he-nWA4SZBSo/edit", 
                              sheet = "S2_ContextualDescription",
                              range = "A10:P84",
                              col_names = TRUE,
                              na = "",
                              guess_max = 10000,
                              trim_ws = TRUE,
                              col_types = "c"
) %>%
  as_tibble() %>%
  clean_names() %>% 
  add_column(case_study = "Aguion",
             reference = "aguion1",
             .after = "question_id")



#######################################################################################################################
burden_sardine <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1TkB-P94xgRssmcbAuzwoh0BlDdvUfYHO7vWjK6r2pxs/edit#gid=1274102518", 
                             sheet = "S2_ContextualDescription",
                             range = "A10:P84",
                             col_names = TRUE,
                             na = "",
                             guess_max = 10000,
                             trim_ws = TRUE,
                             col_types = "c"
) %>%
  as_tibble() %>%
  clean_names()%>% 
  add_column(case_study = "Burden",
             reference = "burden1",
             .after = "question_id")



#######################################################################################################################
dickey_collas_eur <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1hfXTM5hwe0iLWQQmrc31-xruOV3SUixuxqRSuqC8ydA/edit#gid=1707192308", 
                                sheet = "S2_ContextualDescription",
                                range = "A10:P84",
                                col_names = TRUE,
                                na = "",
                                guess_max = 10000,
                                trim_ws = TRUE,
                                col_types = "c"
) %>%
  as_tibble() %>%
  clean_names() %>% 
  add_column(case_study = "Dickey_Collas",
             reference = "dickey_collas1",
             .after = "question_id")



#######################################################################################################################
eurich_clam <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1ZEIev1lcoD6Lxu3otzDCfbJJSiPJoZeyYbTvBXb_S9U/edit#gid=1453404713", 
                          sheet = "S2_ContextualDescription",
                          range = "A10:P84",
                          col_names = TRUE,
                          na = "",
                          guess_max = 10000,
                          trim_ws = TRUE,
                          col_types = "c"
) %>%
  as_tibble() %>%
  clean_names() %>% 
  add_column(case_study = "Eurich",
             reference = "eurich1",
             .after = "question_id")



#####################################################################################################################################
free_crab <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/12zMb5el0QAB-CdZdmdKjWw5Krbwx7farqlUvbN08HEg/edit#gid=390317686", 
                        sheet = "S2_ContextualDescription",
                        range = "A10:P84",
                        col_names = TRUE,
                        na = "",
                        guess_max = 10000,
                        trim_ws = TRUE,
                        col_types = "c"
) %>%
  as_tibble() %>%
  clean_names() %>% 
  add_column(case_study = "Free",
             reference = "free1",
             .after = "question_id")



#######################################################################################################################
golden_madagascar <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1w99nN8C_Vd82j4Y23g2ZG_kuEh_Jc2HkW6lnLkhzeCU/edit#gid=1980661832", 
                                sheet = "S2_ContextualDescription",
                                range = "A10:P84",
                                col_names = TRUE,
                                na = "",
                                guess_max = 10000,
                                trim_ws = TRUE,
                                col_types = "c"
) %>%
  as_tibble() %>%
  clean_names() %>% 
  add_column(case_study = "Golden",
             reference = "golden1",
             .after = "question_id")



#######################################################################################################################
friedman_fiji <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/13fgkP1xHZB6gZqXUBHuylaC3ImCDIDTFFjqQ8RAoV-s/edit#gid=476628697", 
                            sheet = "S2_ContextualDescription",
                            range = "A10:P84",
                            col_names = TRUE,
                            na = "",
                            guess_max = 10000,
                            trim_ws = TRUE,
                            col_types = "c"
) %>%
  as_tibble() %>%
  clean_names() %>% 
  add_column(case_study = "Friedman",
             reference = "friedman1",
             .after = "question_id")



#######################################################################################################################
hollowed_bering_sea <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1YOYZ9T7RR_8OkqQIWeZobhS-3yaEbKmuAVz16Nrrz9g/edit#gid=1980661832", 
                                  sheet = "S2_ContextualDescription",
                                  range = "A10:P84",
                                  col_names = TRUE,
                                  na = "",
                                  guess_max = 10000,
                                  trim_ws = TRUE,
                                  col_types = "c"
) %>%
  as_tibble() %>%
  clean_names() %>% 
  add_column(case_study = "Hollowed",
             reference = "hollowed1",
             .after = "question_id")



########################################################################################################################
kisara_squid <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1fz-aouYW__YjretfedT13l2ttET5THh1IEYcoycnSB4/edit#gid=1980661832", 
                           sheet = "S2_ContextualDescription",
                           range = "A10:P84",
                           col_names = TRUE,
                           na = "",
                           guess_max = 10000,
                           trim_ws = TRUE,
                           col_types = "c"
) %>%
  as_tibble() %>%
  clean_names() %>% 
  add_column(case_study = "Kisara",
             reference = "kisara1",
             .after = "question_id") 



###########################################################################################################################
kleisner_jfi <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1YM1wc6F0SgGIoBcTvSjVyLJquE6-6Chk1dPFoLiXuQY/edit#gid=1865610127", 
                           sheet = "S2_ContextualDescription",
                           range = "A10:P84",
                           col_names = TRUE,
                           na = "",
                           guess_max = 10000,
                           trim_ws = TRUE,
                           col_types = "c"
) %>%
  as_tibble() %>%
  clean_names() %>% 
  add_column(case_study = "Kleisner",
             reference = "kleisner1",
             .after = "question_id")



################################################################################################################################
lau_png <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/18po7Pgq5Od6Q8yCnFdSfj_Gaxii34e-WsYwBFA3RTvo/edit#gid=1980661832", 
                      sheet = "S2_ContextualDescription",
                      range = "A10:P84",
                      col_names = TRUE,
                      na = "",
                      guess_max = 10000,
                      trim_ws = TRUE,
                      col_types = "c"
) %>%
  as_tibble() %>%
  clean_names() %>% 
  add_column(case_study = "Lau",
             reference = "lau1",
             .after = "question_id")



###############################################################################################################################
mason_iceland <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1ZPs5ow4wpZgj5eP26jvoMRgD9q6VT9DEdkJfqj7UrRE/edit#gid=2142384281", 
                            sheet = "S2_ContextualDescription",
                            range = "A10:P84",
                            col_names = TRUE,
                            na = "",
                            guess_max = 10000,
                            trim_ws = TRUE,
                            col_types = "c"
) %>%
  as_tibble() %>%
  clean_names() %>% 
  add_column(case_study = "Mason",
             reference = "mason1",
             .after = "question_id")




##########################################################################################################################
mills_lobster <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1HEPT54TsR1aTNNzDtZI59zU0eb-eJfH61ujErxAFtiI/edit#gid=553950990", 
                            sheet = "S2_ContextualDescription",
                            range = "A10:P84",
                            col_names = TRUE,
                            na = "",
                            guess_max = 10000,
                            trim_ws = TRUE,
                            col_types = "c"
) %>%
  as_tibble() %>%
  clean_names() %>% 
  add_column(case_study = "Mills",
             reference = "mills1",
             .after = "question_id")



########################################################################################################################
pecl_lobster <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/11Z-SLlAS0f9Ug1Z8gLMnHkaN7hoZT1W0KMVLG8HtbXs/edit#gid=179679195", 
                           sheet = "S2_ContextualDescription",
                           range = "A10:P84",
                           col_names = TRUE,
                           na = "",
                           guess_max = 10000,
                           trim_ws = TRUE,
                           col_types = "c"
) %>%
  as_tibble() %>%
  clean_names() %>% 
  add_column(case_study = "Pecl",
             reference = "pecl1",
             .after = "question_id")



###########################################################################################################################
schmidt_senegal <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1bUY6yII5y0FZfDRJ5lxpA7iD2p3o__2A1xsWivxwKB0/edit#gid=2000123802", 
                              sheet = "S2_ContextualDescription",
                              range = "A10:P84",
                              col_names = TRUE,
                              na = "",
                              guess_max = 10000,
                              trim_ws = TRUE,
                              col_types = "c"
) %>%
  as_tibble() %>%
  clean_names() %>% 
  add_column(case_study = "Schmidt",
             reference = "schmidt1",
             .after = "question_id") 



###############################################################################################################################
tokunaga_lobster <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1jgFZut693B14rdKbxprKtQcoh-mFlMzAktekjhvwf2Q/edit#gid=228934601", 
                               sheet = "S2_ContextualDescription",
                               range = "A10:P84",
                               col_names = TRUE,
                               na = "",
                               guess_max = 10000,
                               trim_ws = TRUE,
                               col_types = "c"
) %>%
  as_tibble() %>%
  clean_names() %>% 
  add_column(case_study = "Tokunaga",
             reference = "tokunaga1",
             .after = "question_id") 



############################################################################################################################
westfall_hms <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1DMx0Ux9Li1LhIe4ee5bbAYtJaw1vCezziRGyeERBthY/edit#gid=1818956842", 
                           sheet = "S2_ContextualDescription",
                           range = "A10:P84",
                           col_names = TRUE,
                           na = "",
                           guess_max = 10000,
                           trim_ws = TRUE,
                           col_types = "c"
) %>%
  as_tibble() %>%
  clean_names() %>% 
  add_column(case_study = "Westfall",
             reference = "westfall1",
             .after = "question_id")



###################################################################################################################################
zhao_moorea <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1-Muz76--fNv1qfdXZcBENP-agemTTySZAay9uAEO0HI/edit#gid=1980661832", 
                          sheet = "S2_ContextualDescription",
                          range = "A10:P84",
                          col_names = TRUE,
                          na = "",
                          guess_max = 10000,
                          trim_ws = TRUE,
                          col_types = "c"
) %>%
  as_tibble() %>%
  clean_names() %>% 
  add_column(case_study = "Zhao",
             reference = "zhao1",
             .after = "question_id")

#################################################################################################################################

# Merge Data Sets

learning_merged <- rbind(aguion_barnacle, burden_sardine, dickey_collas_eur, eurich_clam, free_crab, 
                           friedman_fiji, golden_madagascar, hollowed_bering_sea, kisara_squid, kleisner_jfi, 
                           lau_png, mason_iceland, mills_lobster, pecl_lobster, schmidt_senegal, 
                           tokunaga_lobster, westfall_hms, zhao_moorea)

##################################################################################################################################

# Prep for selected data entries

clean_learning_data <- learning_merged %>% 
  filter(question_id %in% c("2.2.1.1.", "2.2.3.2.", "2.3.5.3.", "2.3.5.4.", "2.3.6.1.", "2.3.7.1.", "2.4.3.5.",
                            "2.4.7.2.", "2.4.7.3.", "2.4.7.4.", "2.4.7.5.", "2.4.7.6.", "2.4.7.7.", "2.4.8.1.", 
                            "2.4.9.1.", "2.4.9.2.", "2.4.9.3.", "2.4.9.3.", "2.5.1.1.", "2.5.2.1.", "2.5.3.1.", 
                            "2.5.4.1.", "2.5.5.1.", "2.5.5.1.", "2.5.5.1.", "2.5.6.1.", "2.5.6.1.", "2.5.7.1.",
                            "2.6.1.1.", "2.6.3.1.")) %>% 
  select("topic", "question_id", "case_study", "question", "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l") 















