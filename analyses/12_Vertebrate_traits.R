# vertebrate traits
rm(list=ls())

library("tidyverse")

##### Load species lists from all archipelagoes #####

# Birds
birds <- readRDS("data/derived-data/10_bird_ckl_islands_notclean.RDS")
# all lists together
all_b <- unique(birds$species)

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


# BIRDS 

hb_b <- hab_breadth %>% filter(scientificName %in% all_b)
length(all_b)-nrow(hb_b) # 39 species without IUCN habitat breadth
# need to look for synonyms

# Find synonyms using rredlist and IUCN API key
# syno <- data.frame()
# sp_to_find <- all_b[!all_b %in% hb_b$scientificName]
# 
# for (i in 1:length(sp_to_find)){
#   obj <- rredlist::rl_synonyms(sp_to_find[i],
#                      key = "0e9cc2da03be72f04b5ddb679f769bc29834a110579ccdbeb54b79c22d3dd53d")
#   syno <- bind_rows(syno, obj$result)
#   saveRDS(syno, "data/derived-data/12_synonyms_birds_IUCN.rds")
# }
syno <- readRDS("data/derived-data/12_synonyms_birds_IUCN.rds")
#View(birds %>% filter(species %in% sp_to_find))

hb_in_syno_acc <- left_join(
  hab_breadth %>% 
    filter(scientificName %in% c(syno$accepted_name)),
  syno %>% rename(scientificName = accepted_name)) %>%
  distinct(scientificName, n, synonym)
# 30 species with official IUCN synonyms, to add in the hb database

hb_in_syno_syn <- hab_breadth %>% filter(scientificName %in% c(syno$synonym))

hb_b_all <- bind_rows(
  hb_b %>% mutate(species = scientificName),
  hb_in_syno_acc %>% rename(species = synonym)) %>%
  rename(sci_name_IUCN = scientificName)


#  9 species still missing
setdiff(all_b, hb_b_all$species)
# Extinct species: Dryolimnas augusti, Otus grucheti, Nesoenas cicur,
# Otus sauzieri, Nesoenas rodericanus, Otus murivorus
# => to remove from the database

# Anarhynchus leschenaultii = Charadrius leschenaultii
# Parus teneriffae = Cyanistes teneriffae
# Calandrella rufescens = Alaudala rufescens

to_add <- hab_breadth %>% 
  filter(scientificName %in% c(
    "Charadrius leschenaultii", "Cyanistes teneriffae","Alaudala rufescens")) %>%
  rename(sci_name_IUCN = scientificName) %>%
  mutate(species = sci_name_IUCN) %>%
  mutate(species = if_else(sci_name_IUCN=="Charadrius leschenaultii", "Anarhynchus leschenaultii", species),
         species = if_else(sci_name_IUCN=="Cyanistes teneriffae", "Parus teneriffae", species),
         species = if_else(sci_name_IUCN=="Alaudala rufescens", "Calandrella rufescens", species))

hb_b_all <- bind_rows(hb_b_all, to_add) %>% distinct()
length(unique(hb_b_all$sci_name_IUCN))
length(unique(hb_b_all$species))
# 3 species are duplicated in the IUCN column
# thus need to change their name in the ckl
# => only 244 spe


# MAMMALS

hb_m <- hab_breadth %>% filter(scientificName %in% mam_all)
# all species have HB information
# (logical since the names come from IUCN ranges)


#### Load other trait databases #####


pathtrait="Z:/THESE/5_Data/Traits/"

# BIRDS

# Avonet for HWI, body mass, range size
avonet <- openxlsx::read.xlsx(paste0(
  pathtrait, "Birds_AVONET_Tobias/Supplementary dataset 1.xlsx"), sheet = 2)
avo_taxo <- read.csv2(paste0(
  pathtrait, "Birds_AVONET_Tobias/AVONET_binomial_corresp.csv"))%>%
  select(Species1_BirdLife:Species3_BirdTree) %>%
  distinct()
# amniote (for clutch size)
amniote <- read.csv(paste0(pathtrait, "Amniote_Database_Aug_2015.csv"))
# generation legnth estimates for all birds
# From Bird et al 2020 database
gl <- openxlsx::read.xlsx(paste0(pathtrait, "Bird_et_al_2020_Bird_generation_length_estimates.xlsx"))
# EltonTrait to have Diet_breadth
elton <- read.csv2(paste0(
  pathtrait, "EltonTraits_Birds_WIlman_2014.csv"))

# MAMMALS

# combine: reported traits only
comb_rep <- read.csv(paste0(
  pathtrait, "Mammals_COMBINE_archives/trait_data_reported.csv"))
# combine: including imputed traits
comb_imp <- read.csv(paste0(
  pathtrait, "Mammals_COMBINE_archives/trait_data_imputed.csv"))

#### SELECT TRAITS AND MATCH TAXONOMIES ####

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

# save trait data
saveRDS(traits_m_all, "data/derived-data/12_Mammal_traits.RDS")



# BIRDS

avo_b <- avonet %>%
  filter(Species1 %in% hb_b_all$sci_name_IUCN)

not_avo <- hb_b_all %>% filter(!sci_name_IUCN %in% avo_b$Species1) %>% pull(sci_name_IUCN)

# Find synonyms using rredlist and IUCN API key
# syno <- data.frame()
# for (i in 1:length(not_avo)){
#   obj <- rredlist::rl_synonyms(not_avo[i],
#                                key = "0e9cc2da03be72f04b5ddb679f769bc29834a110579ccdbeb54b79c22d3dd53d")
#   syno <- bind_rows(syno, obj$result)
#   saveRDS(syno, "data/derived-data/12_synonyms_birds_IUCN_AVONET.rds")
# }
syno <- readRDS("data/derived-data/12_synonyms_birds_IUCN_AVONET.rds")

sum(avo_taxo$Species2_eBird %in% syno$synonym)
sum(avo_taxo$Species3_BirdTree %in% syno$synonym)
to_add <- avonet %>%
  filter(Species1 %in% syno$syno) %>%
  mutate(sci_name_IUCN = case_when(
    Species1== "Sylvia conspicillata"~"Curruca conspicillata",
    Species1== "Sylvia melanocephala"~"Curruca melanocephala"
  ))
# only 2 other species 
# "Curruca conspicillata" "Curruca melanocephala"

avo_tot <- bind_rows(
  avo_b %>% mutate(sci_name_IUCN = Species1), to_add)

colnames(avonet)
colnames(avo_taxo)
test <- merge(hb_b_all, avo_taxo, by.x ="sci_name_IUCN", by.y = "Species2_eBird") %>%
  select(-species) %>% distinct()
setdiff(unique(test$sci_name_IUCN), unique(avo_b$Species1))


# fin manually all other species?
sort(not_avo)
# Again extinct species??
# EX = "Alectroenas nitidissimus"

# check with IUCN category
bnp <- read.csv(paste0(path, "birds_nonpasserif/simple_summary.csv"))
bp <- read.csv(paste0(path, "birds_passerif/simple_summary.csv"))
cate <- bind_rows(bnp, bp) %>% select(scientificName, redlistCategory)

cate %>% filter(scientificName %in% not_avo)
#all extinct except "Curruca conspicillata" "Curruca melanocephala"
# to remove from the db

hb_avo_birds <- left_join(avo_tot, hb_b_all) %>%
  select(sci_name_IUCN, species, Species1, n, `Hand-Wing.Index`, Mass, Range.Size)

length(unique(hb_avo_birds$sci_name_IUCN))
# 219 extant birds in total on all islands


# clutch size 
amniote_birds <- amniote %>%
  mutate(binomial=paste(genus, species, sep=" ")) %>%
  filter(class=="Aves") %>%
  select(class, binomial, litter_or_clutch_size_n) %>%
  mutate_all(as.factor) %>%
  mutate(litter_or_clutch_size_n = as.character(litter_or_clutch_size_n)) %>%
  mutate_if(is.character,function(x) gsub("-999", NA, x)) %>%
  mutate_if(is.character,as.numeric)
sum(!is.na(amniote_birds$litter_or_clutch_size_n))

clutch_b <- amniote_birds %>%
  filter(binomial %in% unique(hb_avo_birds$sci_name_IUCN))

# generation length 
gl_b <- gl %>%
  filter(Scientific.name %in% unique(hb_avo_birds$sci_name_IUCN)) %>%
  mutate(sci_name_IUCN = Scientific.name)

# check missing species
setdiff(hb_avo_birds$sci_name_IUCN, gl_b$Scientific.name)
# 3 missing sp with synonyms to add
gl_to_add <- gl %>% 
  filter(Scientific.name %in% c(
    "Psittacula eques", "Sylvia conspicillata", "Sylvia melanocephala")) %>%
  mutate(sci_name_IUCN = case_when(
    Scientific.name == "Psittacula eques" ~ "Alexandrinus eques",
    Scientific.name == "Sylvia conspicillata" ~ "Curruca conspicillata",
    Scientific.name == "Sylvia melanocephala" ~ "Curruca melanocephala"
  ))

gl_all <- bind_rows(gl_b, gl_to_add)

hb_avo_gl <- left_join(hb_avo_birds, gl_all %>% select(sci_name_IUCN, GenLength))


# diet breadth
elton_db <- elton %>%
  select(Scientific, Diet.Inv:Diet.PlantO)

elton_db[elton_db==0] <- NA
elton_db$nb_diet <- rowSums(!is.na(elton_db))-1


db <- elton_db %>%
  select(Scientific, nb_diet) %>%
  filter(Scientific %in% unique(hb_avo_birds$sci_name_IUCN)) %>%
  mutate(sci_name_IUCN = Scientific)

# 39 species are missing, need to find synonyms
sp_to_find_elton <- setdiff(unique(hb_avo_birds$sci_name_IUCN), db$Scientific)

# Find synonyms using rredlist and IUCN API key
# syno_elton <- data.frame()
# 
# for (i in 1:length(sp_to_find_elton)){
#   obj <- rredlist::rl_synonyms(sp_to_find_elton[i],
#                      key = "0e9cc2da03be72f04b5ddb679f769bc29834a110579ccdbeb54b79c22d3dd53d")
#   syno_elton <- bind_rows(syno_elton, obj$result)
#   saveRDS(syno_elton, "data/derived-data/12_synonyms_birds_Elton.rds")
# }
syno_elton <- readRDS("data/derived-data/12_synonyms_birds_Elton.rds")
length(unique(syno_elton$accepted_name))

db_syno <- elton_db %>%
  select(Scientific, nb_diet) %>%
  filter(Scientific %in% unique(syno_elton$synonym))
db_syno <- left_join(
  db_syno, 
  syno_elton %>% distinct(accepted_name, synonym) %>%
    rename(Scientific = synonym)) %>%
  rename(sci_name_IUCN = accepted_name)

db_all <- bind_rows(db, db_syno)

setdiff(unique(hb_avo_birds$sci_name_IUCN), unique(db_all$sci_name_IUCN))
# 5 species without correspondance, check manually
# Nesoenas picturatus = Nesoenas picturata
# Alaudala rufescens = Calandrella rufescens
# Cyanistes teneriffae = Parus teneriffae
# Zosterops mauritianus = same diet breadth as Zosterops borbonicus
# Puffinus bailloni = Puffinus lherminieri
# elton_db %>% filter(Scientific=="Parus teneriffae")
corresp <- c("Nesoenas picturatus" = "Nesoenas picturata",
             "Alaudala rufescens" = "Calandrella rufescens",
             "Cyanistes teneriffae" = "Parus teneriffae",
             "Zosterops mauritianus" = "Zosterops borbonicus",
             "Puffinus bailloni" = "Puffinus lherminieri")

db_to_add <- elton_db %>%
  filter(Scientific %in% corresp) %>% mutate(sci_name_IUCN = Scientific) %>%
  select(Scientific, sci_name_IUCN, nb_diet)
for(i in 1:length(corresp)){
  db_to_add$sci_name_IUCN[i] = names(corresp[corresp==db_to_add$Scientific[i]])
}

db_all <- bind_rows(db_all, db_to_add)


traits_b_all <- left_join(hb_avo_gl, db_all %>% select(-Scientific)) %>%
  rename(nb_hab = n) %>%
  select(-Species1)

saveRDS(traits_b_all, "data/derived-data/12_Bird_traits.RDS")


##### create clean ckl file for islands ####

birds_clean <- left_join(
  birds %>% filter(species %in% hb_avo_birds$species),
  hb_avo_birds %>% select(species, sci_name_IUCN) %>% distinct())
length(unique(birds_clean$species))
length(unique(birds_clean$sci_name_IUCN))

birds_clean_ckl <- birds_clean %>% select(-species) %>% distinct()
length(unique(birds_clean_ckl$sci_name_IUCN))

saveRDS(birds_clean_ckl, "data/derived-data/12_bird_ckl_islands_clean.RDS")
