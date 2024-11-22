# Gaps for all insular plants

rm(list = ls())
library("tidyverse")


#### Load insular native plants
gift_isl <- readRDS("data/raw-data/02_plant_gift_isl.csv")

sp <- read.csv2("data/derived-data/11_Native_plant_list_all_insular.csv")

tr <- read.csv2("data/raw-data/11_GIFT_all_traits.csv")


sum(is.na(gift_isl$geo_entity))
isl_entities <- unique(gift_isl$geo_entity)
length(isl_entities)
# 1573 islands in GIFT

sort(isl_entities)[1]
sort(isl_entities)[1573]

isl_entities[grepl("Zealand",isl_entities)]


length(unique(sp$work_species))
# 94961 species native from islands


tr_iucn <- read.csv("data/raw-data/IUCN_redlist_Plants/plant_specific.csv")
cate <- read.csv("data/raw-data/IUCN_redlist_Plants/simple_summary.csv")  


# Information contained in IUCN Red List
nrow(cate)
# 70584 species assessed by IUCN
table(cate$redlistCategory) 
# 5708 DD for extinction category


sp_iucn <- cate %>%
  dplyr::filter(scientificName %in% sp$work_species)
# 25943 species with names in IUCN

#### Ana's DB ####

ana <- read.csv("data/raw-data/spp_list_gbif_Ana.csv")


aster <- ana %>% filter(gbif_family=="Asteraceae")
# 22,356 species

aster_iucn_isl <- cate %>%
  filter(familyName=="ASTERACEAE") %>%
  dplyr::filter(scientificName %in% sp$work_species)
# 478 insular asteraceae have IUCN information

isl_ana <- ana %>% filter(gbif_species %in% sp$work_species)
# 71,868 plant species in Ana's db

nrow( ana %>%
  filter(gbif_species %in% sp$work_species) %>%
  filter(gbif_family=="Asteraceae"))
# 3917 species

# how many asteraceae in GIFT database
nrow(sp %>% filter(family=="Asteraceae"))
# 5691 

length(unique(gift_isl %>% 
                filter(family=="Asteraceae") %>% 
                filter(native==1) %>%
                pull(geo_entity)))

sort(unique(sp$family))
