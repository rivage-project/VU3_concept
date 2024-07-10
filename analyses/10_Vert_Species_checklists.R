# retrieve checklists for all the major islands
# for birds and mammals
rm(list=ls())
library("tidyverse")

isl_select <- read.csv("data/derived-data/01_selected_islands.csv")
unique(isl_select$Island_name)

#### BIRDS ####

# Azores
azo_birds <- openxlsx::read.xlsx("data/raw-data/Azores_All_Mammals_Birds_names_ok.xlsx", sheet = 1)
colnames(azo_birds)
#change colnames for matching the island names in isl_select
colnames(azo_birds) <- c("species","Status","Comments",
                         "Ilha do Corvo","Ilha das Flores","Ilha do Faial",
                         "Ilha do Pico","Ilha Graciosa","Ilha de Sao Jorge", 
                         "Ilha Terceira","Ilha de Sao Miguel","Ilha de Santa Maria")
# Galapagos 
glp_b <- read.csv("data/raw-data/Matthews_et_al_2023/txm676-DARs-8b381ff/Data/Island_datasets/True_island_datasets/Baiser et al (2017) Galapagos Birds_current.csv")
# change colnames for matching the island names in isl_select
# Baltra.Seymour refers to two islands => duplicate the column with each name
# Floranea = Isla Santa Maria, Santiago1 = Isla San Salvador
colnames(glp_b) <- c("species","Isla Espanola","Isla Santa Maria","Isla San Cristobal",
                     "Isla Santa Fe","Isla Santa Cruz","Isla Baltra",
                     "Isla Pinzon","Isla Isabela","Isla Fernandina",
                     "Isla San Salvador", "Isla Genovesa")
glp_b <- glp_b[1:54,] %>%
  mutate(species = gsub("_"," ", species)) %>%
  mutate(`Isla Seymour` = `Isla Baltra`)
                         
# Hawaii
hw_b <- read.csv("data/raw-data/Matthews_et_al_2023/txm676-DARs-8b381ff/Data/Island_datasets/True_island_datasets/Baiser et al (2017) Hawaii Birds_current_noAliens.csv")
hw_b <- hw_b[1:38,] %>%
  mutate(species = gsub("_"," ", species)) %>%
  rename(`Island of Hawaii`= Hawaii)

# canarias
cn_b <- read.csv("data/raw-data/Matthews_et_al_2023/txm676-DARs-8b381ff/Data/Island_datasets/True_island_datasets/Borgesetal_canary birds_noAliens.csv")
cn_b2 <- openxlsx::read.xlsx("data/raw-data/Canarias_Mammals_Birds.xlsx", sheet = "birds")

# reunion
lr_b <- openxlsx::read.xlsx("data/raw-data/AVIBASE/AVIBASE_Reunion_accessed240709.xlsx")
# mauritius
m_b <- openxlsx::read.xlsx("data/raw-data/AVIBASE/AVIBASE_Mauritius_main_isl_accessed240709.xlsx")
# rodrigues
rg_b <- openxlsx::read.xlsx("data/raw-data/AVIBASE/AVIBASE_Mauritius_Rodrigues_accessed240709.xlsx")

masc <- bind_rows(lr_b, m_b, rg_b)



# shape all databases in long format
# bind all archipelagoes together for the clean final checklist

azo_b <- azo_birds %>%
  select(-Comments) %>%
  pivot_longer(cols = Corvo:Santa.Maria, names_to = "Island", values_to = "Occ") %>%
  filter(Occ==1) %>%
  mutate(Species = gsub("_"," ", species)) %>%
  filter(Status=="NATIVE")



hw_b <- hw_b[1:38,] %>%
  mutate(Species = gsub("_"," ", species)) %>%
  pull(Species)

cn_b <- cn_b[1:74,] %>%
  mutate(Species = gsub("_"," ", species)) %>%
  pull(Species)

cn_b2 <- cn_b2 %>% pull(Species)

cn_btot <- unique(c(cn_b, cn_b2))

masc_b <- unique(masc %>%
                   filter(!is.na(binomial)) %>%
                   filter(!status %in% c("Rare/Accidentel", "Espèce introduite", "Extirpé")) %>%
                   pull(binomial))

#### MAMMALS ####

# most of mammal checklists in internet were from IUCN
# and checklists are not by island but by archipelago
# so intersect island polygons with IUCN mammal ranges
# and then perform a manual check for each archip

isl_select <- read.csv("data/derived-data/01_selected_islands.csv")
path_data <- "Z:/THESE/5_Data/Distribution_spatiale/"
gadm_islands <- sf::st_read(paste0(
  path_data, "Shpfiles_iles_continent/Islands_Weigelt_reparees.shp"))

shp_55 <- subset(gadm_islands, ULM_ID %in% isl_select$ID)

# takes time (~20 min) => run only if necessary
# mam <- sf::st_read("data/raw-data/IUCN_RANGE/MAMMALS.shp")
# colnames(mam)
# summary(mam)
# mam <- mam %>% 
#   filter(presence %in% c(1:3) & origin %in% c(1:2) & seasonal %in% c(1:3)) %>%
#   filter(terrestria=="true") %>%
#   sf::st_make_valid()
# 
# mam_valid <- mam[sf::st_is_valid(mam),]
# inter <- sf::st_intersects(shp_55, mam_valid)
# 
# names(inter) <- shp_55$ULM_ID
# 
# mam_ckl <- data.frame()
# for (i in 1:length(inter)){
#   m <- mam_valid[inter[[i]],] %>% sf::st_drop_geometry()
#   if(nrow(m)>0){
#     m$ULM_ID <- shp_55$ULM_ID[i]
#     mam_ckl <- bind_rows(mam_ckl,m)
#   }
# }
# saveRDS(mam_ckl, "data/derived-data/10_mammals_inter_isl.RDS")

mam_ckl <- readRDS("data/derived-data/10_mammals_inter_isl.RDS")
unique(mam_ckl %>% pull(sci_name))

# remove Lontra canadensis from Hawaii?

# add native mammals from canary and azores checklists

# Azores (data from  F. Rigal)
azo_mam <- openxlsx::read.xlsx("data/raw-data/Azores_All_Mammals_Birds_names_ok.xlsx",
                               sheet = 2)
# only one native mammal Nyctalus azoreum
# Rattus norvegicus is not native from Azores => remove from native checklist

# Canary islands (data for official national checklist 2012 - JMFP)
can_m <- openxlsx::read.xlsx("data/raw-data/Canarias_Mammals_Birds.xlsx", sheet = "mammals")

mam_ckl %>% filter(sci_name %in% can_m$Species) %>% distinct(sci_name) %>% pull(sci_name)
# ok all species ar in both databases

# mascarenes
# check with wikipedia page for mammals of mauritius + LR => all consistent

# galapagos
sort(mam_ckl %>% filter(ULM_ID %in% c(9883:9993, 85138)) %>% 
  distinct(sci_name) %>% pull(sci_name))
# most species listed in the wikipedia page are extinct...
# so data are consistent with our db

# final checklist
mam_ckl_clean <- mam_ckl %>%
  filter(!sci_name %in% c("Lontra canadensis","Rattus norvegicus"))
saveRDS(mam_ckl_clean, "data/derived-data/10_mammals_inter_isl_clean.RDS")



####### Draft part ######


# Azores: data from François Rigal

azo_birds <- openxlsx::read.xlsx("data/raw-data/Azores_All_Mammals_Birds_names_ok.xlsx",
                                 sheet = 1)
azo_mam <- openxlsx::read.xlsx("data/raw-data/Azores_All_Mammals_Birds_names_ok.xlsx",
                               sheet = 2)
colnames(azo_birds)

azo_b <- azo_birds %>%
  select(-Comments) %>%
  pivot_longer(cols = Corvo:Santa.Maria, names_to = "Island", values_to = "Occ") %>%
  filter(Occ==1) %>%
  mutate(Group = "Birds",
         Species = gsub("_"," ", species)) %>%
  select(-c(Occ, species))

azo_m <- azo_mam  %>%
  pivot_longer(cols = Corvo:Santa.Maria, names_to = "Island", values_to = "Occ") %>%
  filter(Occ==1) %>%
  mutate(Group = "Mammals") %>%
  select(-Occ)

azo_vert <- bind_rows(azo_b, azo_m)

str(azo_mam)
azo_vert %>% group_by(Island, Group) %>% count()


# check the difference between Avibase and the expert checklist for Azores

# create a list with checklists downloaded for each island
azo_isl <- as.list(unique(azo_vert$Island))
names(azo_isl) = unique(azo_vert$Island)

for(i in 1:length(azo_isl)){
  azo_isl[[i]] <- openxlsx::read.xlsx(
    "data/raw-data/AVIBASE/AVIBASE_Azores_accessed240228.xlsx",
    sheet = azo_isl[[i]], colNames = F)
}

azo_isl <- lapply(azo_isl, function(x){
  colnames(x) <- c("English","binomial","French","status")
  x <- x %>% filter(!is.na(binomial))
  return(x)
})


View(azo_isl$Flores)
table(azo_isl$Flores$status)
lapply(azo_isl, function(x){sum(is.na(x$status))})

openxlsx::write.xlsx(azo_isl, "data/derived-data/10_AVIBASE_birds_Azores.xlsx")


#birds from La Réunion
lr_b <- openxlsx::read.xlsx("data/raw-data/AVIBASE/AVIBASE_Reunion_accessed240709.xlsx") %>%
  filter(!is.na(binomial)) %>%
  filter(!status %in% c("Rare/Accidentel", "Espèce introduite"))

filter(status != "Espèce introduite")

# birds from Galapagos 
glp_b <- read.csv("data/raw-data/Matthews_et_al_2023/txm676-DARs-8b381ff/Data/Island_datasets/True_island_datasets/Baiser et al (2017) Galapagos Birds_current.csv")
# 11 islands 

# birds from Hawaii
hw_b <- read.csv("data/raw-data/Matthews_et_al_2023/txm676-DARs-8b381ff/Data/Island_datasets/True_island_datasets/Baiser et al (2017) Hawaii Birds_current_noAliens.csv")
# 6 islands

# canarias
cn_b <- read.csv("data/raw-data/Matthews_et_al_2023/txm676-DARs-8b381ff/Data/Island_datasets/True_island_datasets/Borgesetal_canary birds_noAliens.csv")
cn_b2 <- openxlsx::read.xlsx("data/raw-data/Canarias_Mammals_Birds.xlsx", sheet = "birds")




