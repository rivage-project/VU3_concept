# Get alien species checklists
# for alien plants, mammals, and birds
# make the distinction between alien only and invasive aliens
rm(list = ls())
library(tidyverse)

###### Archipelago checklists from GRIIS #####

# Get alien list for each archipelago

# "Canary Islands" => list per island, useful for evaluating accuracy?
# "Mascarene Islands", "Rodrigues" = > Mauritius + La Réunion
# "Hawaii","Galapagos Islands", "Azores"

##### Canarias ------

# checklist from GRIIS in GBIF
# https://www.gbif.org/dataset/search?q=Canarias&publishing_org=cdef28b1-db4e-4c58-aa71-3c5238c2d0b5
# downloaded on 18/04/24

# open lists, merge data from each island together
# filter for plants, birds, and mammals 

fold <- list.files("data/raw-data/alien_species/canarias/")

can_isl <- c("gran","hierro","palma","gomera","tenerife","lanzarote","fuerteventura")
alien_can <- data.frame()

for(i in can_isl){
  #i = "gran"
  distrib <- readr::read_tsv(paste0("data/raw-data/alien_species/canarias/",
                             fold[grep(i, fold)], "/distribution.txt"))
  profile <- readr::read_tsv(paste0("data/raw-data/alien_species/canarias/",
                               fold[grep(i, fold)], "/speciesprofile.txt"))
  taxon <- readr::read_tsv(paste0("data/raw-data/alien_species/canarias/",
                               fold[grep(i, fold)], "/taxon.txt"))
  
  all <- left_join(distrib, left_join(profile, taxon))

  alien_can <- bind_rows(alien_can, all)
}

# keep only plants, birds, mammals

pbm_alien_can <- alien_can %>%
  filter(class %in% c("Aves","Mammalia") | kingdom == "Plantae")

length(unique(pbm_alien_can$class))
table(pbm_alien_can$occurrenceStatus) # only presences
table(pbm_alien_can$establishmentMeans) # remove uncertain/ cryptogenic?

nb_ias <- pbm_alien_can %>%
  filter(establishmentMeans =="Alien") %>%
  group_by(locationID,isInvasive,class) %>% #class
  summarize(n = n()) %>%
  pivot_wider(names_from = isInvasive, values_from = n, values_fill = 0) %>%
  rename(nonInvasive = Null) %>%
  mutate(Tot_alien = Invasive + nonInvasive)

nrow(pbm_alien_can %>%
       filter(establishmentMeans =="Alien"))

test <- pbm_alien_can %>%
  filter(establishmentMeans =="Alien") %>%
  distinct(scientificName, isInvasive, class)
table(test$class)
length(unique(pbm_alien_can%>%
                filter(establishmentMeans =="Alien") %>% pull(scientificName)))
# much less exotic species than in GIFT for plants

###### Mascarenes -------
fold <- list.files("data/raw-data/alien_species/mascarene/")

masc_isl <- c("mauritius","reunion")
alien_masc <- data.frame()

for(i in masc_isl){
  distrib <- readr::read_tsv(paste0("data/raw-data/alien_species/mascarene/",
                                    fold[grep(i, fold)], "/distribution.txt"))
  profile <- readr::read_tsv(paste0("data/raw-data/alien_species/mascarene/",
                                    fold[grep(i, fold)], "/speciesprofile.txt"))
  taxon <- readr::read_tsv(paste0("data/raw-data/alien_species/mascarene/",
                                  fold[grep(i, fold)], "/taxon.txt"))
  
  all <- left_join(distrib, left_join(profile, taxon))
  all$locationID = i
  
  alien_masc <- bind_rows(alien_masc, all)
}


###### Azores -------
distrib <- readr::read_tsv(paste0("data/raw-data/alien_species/azores/",
                                  "dwca-griis-portugal-azores-v1.8", "/distribution.txt"))
profile <- readr::read_tsv(paste0("data/raw-data/alien_species/azores/",
                                  "dwca-griis-portugal-azores-v1.8", "/speciesprofile.txt"))
taxon <- readr::read_tsv(paste0("data/raw-data/alien_species/azores/",
                                "dwca-griis-portugal-azores-v1.8", "/taxon.txt"))
az <- left_join(distrib, left_join(profile, taxon))
az_b <- az %>% filter(class=="Aves" & establishmentMeans =="Alien")
# no alien birds according to griis
az_m <- az %>% filter(class=="Mammalia" & establishmentMeans =="Alien")
# 6 alien mammals according to griis


# compare with François' database of birds and mammals
azo_birds <- openxlsx::read.xlsx("data/raw-data/Azores_All_Mammals_Birds_names_ok.xlsx",
                                 sheet = 1)
# 7 alien birds according to François database
azo_mam <- openxlsx::read.xlsx("data/raw-data/Azores_All_Mammals_Birds_names_ok.xlsx",
                               sheet = 2)
azo_mam %>% filter(Status=="ALIEN") %>% pull(Species)
az_m %>% pull(scientificName)
# only 3 species are in common...

az_bm <- az %>% 
  filter(class %in% c("Aves", "Mammalia") & establishmentMeans =="Alien") %>%
  distinct(class, scientificName) %>% 
  mutate(Archip = "Azores")
# add mammals and birds from François database
az_bm_fr <- bind_rows(
  azo_birds %>% filter(Status=="ALIEN") %>%
    select(species) %>% mutate(class="Aves"),
  azo_mam %>% filter(Status=="ALIEN") %>%
    select(Species) %>% mutate(class="Mammalia") %>% 
    rename(species = Species)) %>%
  mutate(Archip = "Azores",
         species = gsub("_", " ", species)) %>%
  rename(scientificName = species)

az_bm <- bind_rows(az_bm, az_bm_fr)

###### Galapagos  -------

# from griis 
distrib <- readr::read_tsv(paste0("data/raw-data/alien_species/galapagos/",
                                  "dwca-griis_galapagos_islands-v1.5", "/distribution.txt"))
profile <- readr::read_tsv(paste0("data/raw-data/alien_species/galapagos/",
                                  "dwca-griis_galapagos_islands-v1.5", "/speciesprofile.txt"))
taxon <- readr::read_tsv(paste0("data/raw-data/alien_species/galapagos/",
                                "dwca-griis_galapagos_islands-v1.5", "/taxon.txt"))
glp <- left_join(distrib, left_join(profile, taxon))

glp_bm <- glp %>% 
  filter(class %in% c("Aves", "Mammalia") & establishmentMeans =="Alien") %>%
  distinct(class, scientificName) %>% 
  mutate(Archip = "Galapagos Islands")

unique(shp_44$ARCHIP)

###### Hawaii -------

# GBIF.org (18 April 2024) GBIF Occurrence Download https://doi.org/10.15468/dl.afsuaj

hw = data.table::fread("data/raw-data/alien_species/hawaii/0192966-240321170329656.csv")
hw_p <- hw %>%
  filter(kingdom=="Plantae")
# 1558 species

hw_bm <- hw %>%
  filter(class %in% c("Aves","Mammalia"))
table(hw_bm$class) 
# 57 birds, 17 mammals => looks underestimated
griis_b <- hw_bm %>% filter(class=="Aves") %>% pull(verbatimScientificName)

# check with data from Matthews et al
# hawaii islands with alien birds

hw_matt_all <- read.csv("data/raw-data/Matthews_et_al_2023/txm676-DARs-8b381ff/Data/Island_datasets/True_island_datasets/Alternative_versions/Alien_versions/Baiser et al (2017) Hawaii Birds_current_withAlien.csv")
hw_matt_nat <- read.csv2("data/raw-data/Baiser et al (2017) Hawaii Birds_current_noAliens.csv")

hw_alien_b <- gsub("_"," ", setdiff(hw_matt_all$species, hw_matt_nat$species))

sum(griis_b %in% hw_alien_b) # 11 species not in common from each list

griis_b[! griis_b %in% hw_alien_b]
hw_alien_b[! hw_alien_b %in% griis_b]
# some are synonyms

# Find synonyms using rredlist and APi key to have all synonyms from IUCN 
syno_griis <- data.frame()
for (i in griis_b[! griis_b %in% hw_alien_b]){
  obj <- rredlist::rl_synonyms(i,
                     key = "0e9cc2da03be72f04b5ddb679f769bc29834a110579ccdbeb54b79c22d3dd53d")
  syno_griis <- bind_rows(syno_griis, obj$result)
}

syno_mat <- data.frame()
for (i in hw_alien_b[! hw_alien_b %in% griis_b]){
  obj <- rredlist::rl_synonyms(i,
                               key = "0e9cc2da03be72f04b5ddb679f769bc29834a110579ccdbeb54b79c22d3dd53d")
  syno_mat <- bind_rows(syno_mat, obj$result)
}

syno <- bind_rows(syno_mat, syno_griis) %>% distinct()

not_common <- griis_b[! griis_b %in% hw_alien_b]
for(i in not_common){
  if (i %in% syno$accepted_name | i %in% syno$synonym){
    print(i)
  }
}

##### Alien checklist per archipelago ########




###### Methods with GBIF #######

# define the list of species to look for:
# all the species listed in the GRIIS checlists 
# + species in other lists (Matthews, Rigal, etc)

# only for alien birds and mammals
# (if we accept the plant checklists from GIFT)



# make a list of all alien birds and mammals
# in all the archipelagoes


# get the GBIF identifier
# get the polygons of all islands
# select only major islands with plant data

# island polygons
path_data <- "Z:/THESE/5_Data/Distribution_spatiale/"
gadm_islands <- sf::st_read(paste0(
  path_data, "Shpfiles_iles_continent/Islands_Weigelt_reparees.shp"))

isl <- readRDS("data/derived-data/11_isl_with_gift_data.rds") %>% 
  filter(!is.na(isl_name_gift))
isl_select <- read.csv("data/derived-data/01_selected_islands.csv")

shp_44 <- gadm_islands %>% 
  filter(ULM_ID %in% (isl_select %>% 
                        filter(Island_name %in% isl$Island_name) %>% 
                        pull(ID)))


