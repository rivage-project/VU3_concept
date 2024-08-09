# Intrinsic AC (for each taxon)
# plants = max_height, birds = HWI, mammals = body size

rm(list=ls())

library(tidyverse)

# load bird, mammal, and plant checklists per island
birds <- readRDS("data/derived-data/12_bird_ckl_islands_clean.RDS")
mam <- readRDS("data/derived-data/10_mammals_inter_isl_clean.RDS")
plant <- readRDS("data/derived-data/11_cklists_plants_islands.rds")

# load traits
tr_birds <- readRDS("data/derived-data/12_Bird_traits.RDS")
tr_mam <- readRDS("data/derived-data/12_Mammal_traits.RDS")
# tr_plants <- readRDS()
gift_isl_tra <- readRDS("data/derived-data/11_GIFT_traits_isl.rds") %>% 
  ungroup() # test avec uncomplete trait
summary(gift_isl_tra$trait_value_1.6.2) # 2000 NA

# load islands
isl_select <- read.csv("data/derived-data/01_selected_islands.csv")
minor_islands <- c(
  "Isla Graciosa","Isla de Alegranza","Lobos","Isla de Montana Clara", # Canary
  "Lisianski Island","Sand Island","Lehua","Laysan Island", "Ford Island", # Hawaii
  "Ile d' Ambre", # Mascarene
  "Isla Bartolome", "Isla Seymour", "Isla Darwin"# Galapagos
)


# aggregate mean trait per island

ac_plant <- left_join(
  gift_isl_tra %>% 
    rename(max_height = trait_value_1.6.2) %>%
    select(work_species, max_height),
  left_join(
    plant,
    isl_select %>% select(Island_name, ID) %>% rename(ULM_ID = ID)
))

ac_plant_isl <- ac_plant %>%
  group_by(Archip, Island_name, ULM_ID) %>%
  summarize(na = sum(is.na(max_height)),
            n_tot = n(), 
            max_height = mean(max_height, na.rm = T)) %>%
  mutate(na_rate = na/n_tot) %>% ungroup()

ggplot(ac_plant_isl)+
  geom_boxplot(aes(x = Archip, y = na_rate))+
  geom_point(aes(x = Archip, y = na_rate, color = Archip, size = n_tot), position ="jitter")

ggplot(ac_plant_isl)+
  geom_boxplot(aes(x = Archip, y = max_height))+
  geom_point(aes(x = Archip, y = max_height, color = Archip, size = na_rate), position ="jitter")

# pbm with mean(max height) => higher in islands with higher NA rate...
# because plants that are reported in islands with few values are the tallest


# mammals
# bind checklists with traits
ckl_tr_m <- left_join(
  mam %>% rename(scientificName = sci_name) %>%
    select(scientificName, ULM_ID, SHAPE_Area) %>%
    rename(Range.Size = SHAPE_Area), 
  tr_mam)

# aggregate values per islands for each group
ac_mam <- ckl_tr_m %>% select(ULM_ID, adult_mass_g) %>%
  group_by(ULM_ID) %>% 
  summarise(mass = mean(adult_mass_g))

# birds
# bind checklist with traits 
ckl_tr_b <- left_join(birds , tr_birds %>% select(-species) %>% distinct())

# aggregate values per islands for each group
ac_b <- ckl_tr_b %>% 
  select(ULM_ID, `Hand-Wing.Index`) %>%
  group_by(ULM_ID) %>% 
  summarise(mean_hwi = mean(`Hand-Wing.Index`))

# join plants, mam, birds

ac <- left_join(
  ac_mam, 
  left_join(ac_plant_isl %>% select(ULM_ID, max_height), ac_b))


saveRDS(ac, "data/derived-data/31_AC_intrinsic_BMP.rds")

