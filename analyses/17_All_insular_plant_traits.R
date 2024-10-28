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




