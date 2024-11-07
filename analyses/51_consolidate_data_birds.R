library(tidyverse)

# load exposure to each threat
ias <- readRDS("data/derived-data/22_IAS_exposure_39_isl.rds")
cc <- readRDS("data/derived-data/21_CC_SED_exposure_55_isl.rds")
lu <- readRDS("data/derived-data/20_LU_exposure_55_isl.rds")

threats <- left_join(
  lu %>% select(ID, mean_HM_static_2017, mean_HM_change, rdens_osm) %>%
    mutate(ID = as.integer(ID)),
  left_join(
    cc %>% select(ID, sed_tot_med), 
    ias %>% select(ULM_ID,
                   nb_alien_bird, 
                   prop_alien_bird, 
                   alien_bird_cover) %>%
      rename(ID=ULM_ID)
  )
) %>%
  rename(ULM_ID=ID)

birds <- readRDS("data/derived-data/12_bird_ckl_islands_clean.RDS")

# load traits
tr_birds <- readRDS("data/derived-data/12_Bird_traits.RDS")

data_birds_all <-
  dplyr::left_join(birds,
                   tr_birds,
                   by = "sci_name_IUCN") %>% 
  dplyr::left_join(threats,
                   by = "ULM_ID")

saveRDS(data_birds_all, "data_birds_all.rds")