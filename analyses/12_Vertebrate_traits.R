# vertebrate traits

library("tidyverse")

##### Load species lists from all archipelagoes #####

# Birds
birds <- readRDS("")
# all lists together
all_b <- unique(c(azo_b, glp_b, hw_b, cn_btot, masc_b))

# Mammals

mam <- readRDS("data/derived-data/10_mammals_inter_isl_clean.RDS")
mam_all <- unique(mam %>% pull(sci_name))


##### Habitat breadth from IUCN summary ####


# load IUCN summary
path="Z:/THESE/5_Data/IUCN_summary/MAJ_2023_06/"
amr <- read.csv(paste0(path, "amph_mam_rept/habitats.csv"))
bnp <- read.csv(paste0(path, "birds_nonpasserif/habitats.csv"))
bp <- read.csv(paste0(path, "birds_passerif/habitats.csv")) %>% 
  mutate(code = as.character(code))

hab <- bind_rows(amr, bnp, bp)

hab_breadth <- hab %>% 
  mutate(code_sub = substr(code, 1,3)) %>%
  mutate(code_simple = floor(as.numeric(code_sub)))%>%
  filter(code_simple!=18) %>%
  distinct(scientificName, code_simple) %>%
  group_by(scientificName) %>% count()


hb_b <- hab_breadth %>% filter(scientificName %in% all_b)
length(all_b)-nrow(hb_b) # 39 species without IUCN habitat breadth
# need to look for synonyms

hb_m <- hab_breadth %>% filter(scientificName %in% mam_all)
# all species have HB information
# (logical since the names come from IUCN ranges)


# Find synonyms using rredlist and IUCN API key
syno <- data.frame()
sp_to_find <- all_b[!all_b %in% hb_b$scientificName]

for (i in 1:length(sp_to_find)){
  obj <- rredlist::rl_synonyms(sp_to_find[i],
                     key = "0e9cc2da03be72f04b5ddb679f769bc29834a110579ccdbeb54b79c22d3dd53d")
  syno <- bind_rows(syno, obj$result)
  saveRDS(syno, "data/derived-data/12_synonyms_birds_IUCN.rds")
}

syno <- readRDS("data/derived-data/12_synonyms_birds_IUCN.rds")



#### COMBINE and AVONET for other traits #####

pathtrait="Z:/THESE/5_Data/Traits/"
avonet <- openxlsx::read.xlsx(paste0(
  pathtrait, "Birds_AVONET_Tobias/Supplementary dataset 1.xlsx"), sheet = 2)
# combine: reported traits only
comb_rep <- read.csv(paste0(
  pathtrait, "Mammals_COMBINE_archives/trait_data_reported.csv"))
# combine: including imputed traits
comb_imp <- read.csv(paste0(
  pathtrait, "Mammals_COMBINE_archives/trait_data_imputed.csv"))



#### SELECT TRAITS ####

# MAMMALS 

# check the number of imputed values for each trait
traits_m_rep <- comb_rep %>% # reported dataset
  filter(iucn2020_binomial %in% mam_all)
traits_m_imp <- comb_imp %>% # imputed dataset
  filter(iucn2020_binomial %in% mam_all)
colSums(is.na(traits_m_rep))
colSums(is.na(traits_m_imp))
# body mass was reported for all species
# diet breadth has imputed values for 6 sp
# generation length has imputed values for 17 sp but litter size only for 7 sp
colnames(traits_m)
plot(comb_imp$litter_size_n, comb_imp$generation_length_d)
cor.test(comb_imp$litter_size_n, comb_imp$generation_length_d)

# Select the traits 
traits_m <- comb_imp %>% # imputed dataset
  filter(iucn2020_binomial %in% mam_all) %>%
  # body mass, litter size, diet 
  select(iucn2020_binomial, det_diet_breadth_n, habitat_breadth_n,
         adult_mass_g, generation_length_d, litter_size_n) %>%
  rename(scientificName = iucn2020_binomial)

traits_m_all <- left_join(traits_m, hb_m)
# check if IUCN hab breadth are consistent together
plot(traits_m_all$habitat_breadth_n, traits_m_all$n)
# i'll take the updated IUCN summaries, so not hab breadth from COMBINE

traits_m_all <- traits_m_all %>% 
  select(-habitat_breadth_n) %>% rename(hab_breadth = n)




# HWI for birds

