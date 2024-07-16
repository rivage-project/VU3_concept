# Sensitivity for vertebrates
# => mean trait value per island

rm(list=ls())
library("tidyverse")


# load bird and mammal checklists per island
birds <- readRDS("data/derived-data/12_bird_ckl_islands_clean.RDS")
mam <- readRDS("data/derived-data/10_mammals_inter_isl_clean.RDS")

# load traits
tr_birds <- readRDS()
tr_mam <- readRDS("data/derived-data/12_Mammal_traits.csv")

# load islands
isl_select <- read.csv("data/derived-data/01_selected_islands.csv")


# bind checklists with traits
# mam
ckl_tr_m <- left_join(
  mam %>% rename(scientificName = sci_name) %>%
    select(scientificName, ULM_ID), 
  tr_mam)

# aggregate values per islands for each group
sens_mam <- ckl_tr_m %>% select(-scientificName) %>%
  group_by(ULM_ID) %>% 
  summarise(across(everything(), mean))


# add number of species per isl
sr_m <- mam %>% group_by(ULM_ID) %>% summarise(SR_mam = n()) 
sens_mam <- left_join(sens_mam, sr_m)

