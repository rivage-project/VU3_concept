library(tidyverse)

# load exposure to each threat
ias <- readRDS("data/derived-data/22_IAS_exposure_39_isl.rds")
cc <- readRDS("data/derived-data/21_CC_SED_exposure_55_isl.rds")
lu <- readRDS("data/derived-data/20_LU_exposure_55_isl.rds")
colnames(lu)[colnames(lu)=='ID'] <- 'ULM_ID'
colnames(ias)[colnames(ias)=='ID'] <- 'ULM_ID'
colnames(cc)[colnames(cc)=='ID'] <- 'ULM_ID'

ac_elev <- readRDS("data/derived-data/30_extrinsic_AC_elevation.rds")
ac_PA <- readRDS("data/derived-data/30_extrinsic_AC_prop_PA.rds")
ac_intr <- readRDS("data/derived-data/31_AC_intrinsic_BMP.rds")

threats <- left_join(
  lu %>% select(ULM_ID, mean_HM_static_2017, mean_HM_change, rdens_osm) %>%
  mutate(ULM_ID = as.integer(ULM_ID)),
  left_join(
    cc %>% select(ULM_ID, sed_tot_med), 
    ias %>% select(ULM_ID,
                   nb_alien_bird, 
                   prop_alien_bird, 
                   alien_bird_cover)
  )
)

birds <- readRDS("data/derived-data/12_bird_ckl_islands_clean.RDS")

# load traits
tr_birds <- readRDS("data/derived-data/12_Bird_traits.RDS")

data_birds_all <-
  dplyr::left_join(birds,
                   tr_birds,
                   by = "sci_name_IUCN") %>% 
  dplyr::left_join(threats,
                   by = "ULM_ID") %>% 
  dplyr::left_join(ac_elev,
                   by = "ULM_ID") %>% 
  dplyr::left_join(ac_PA[-5],
                   by = "ULM_ID")

saveRDS(data_birds_all, "data_birds_all.rds")
