# retrieve checklists for all the major islands
# for birds and mammals
rm(list=ls())
library("tidyverse")

isl_select <- read.csv("data/derived-data/01_selected_islands.csv")
unique(isl_select$Island_name)

#### BIRDS ####

# shape all databases in long format
# with a column for Island_name and a column for species


# Azores
azo_b <- openxlsx::read.xlsx("data/raw-data/Azores_All_Mammals_Birds_names_ok.xlsx", sheet = 1)
colnames(azo_b)
#change colnames for matching the island names in isl_select
colnames(azo_b) <- c("species","Status","Comments",
                         "Ilha do Corvo","Ilha das Flores","Ilha do Faial",
                         "Ilha do Pico","Ilha Graciosa","Ilha de Sao Jorge", 
                         "Ilha Terceira","Ilha de Sao Miguel","Ilha de Santa Maria")
azo_bl <- azo_b %>%
  filter(Status=="NATIVE") %>%
  select(-c(Comments, Status)) %>%
  pivot_longer(!species, names_to = "Island_name", values_to = "Occ") %>%
  filter(Occ==1) %>%
  mutate(species = gsub("_"," ", species)) %>%
  select(-Occ)
length(unique(azo_bl$species)) # 27 native bird species in Azores

# Galapagos 
glp_b <- read.csv("data/raw-data/Matthews_et_al_2023/txm676-DARs-8b381ff/Data/Island_datasets/True_island_datasets/Baiser et al (2017) Galapagos Birds_current.csv")
# change colnames for matching the island names in isl_select
# Baltra.Seymour refers to two islands => duplicate the column with each name
# Floranea = Isla Santa Maria, Santiago1 = Isla San Salvador
colnames(glp_b) <- c("species","Isla Espanola","Isla Santa Maria","Isla San Cristobal",
                     "Isla Santa Fe","Isla Santa Cruz","Isla Baltra",
                     "Isla Pinzon","Isla Isabela","Isla Fernandina",
                     "Isla San Salvador", "Isla Genovesa")
glp_bl <- glp_b[1:54,] %>%
  mutate(species = gsub("_"," ", species)) %>%
  mutate(`Isla Seymour` = `Isla Baltra`) %>%
  pivot_longer(!species, names_to = "Island_name", values_to = "Occ") %>%
  filter(Occ==1) %>%
  select(-Occ)
length(unique(glp_bl$species))# 54 native birds
                         
# Hawaii
hw_b <- read.csv("data/raw-data/Matthews_et_al_2023/txm676-DARs-8b381ff/Data/Island_datasets/True_island_datasets/Baiser et al (2017) Hawaii Birds_current_noAliens.csv")
hw_bl <- hw_b[1:38,] %>%
  mutate(species = gsub("_"," ", species)) %>%
  rename(`Island of Hawaii`= Hawaii) %>%
  pivot_longer(!species, names_to = "Island_name", values_to = "Occ") %>%
  filter(Occ==1) %>%
  select(-Occ)
length(unique(hw_bl$species))# 38 native birds

# canarias
cn_b <- read.csv("data/raw-data/Matthews_et_al_2023/txm676-DARs-8b381ff/Data/Island_datasets/True_island_datasets/Borgesetal_canary birds_noAliens.csv")
colnames(cn_b) <- c("species","Isla de Lanzarote","Isla de Fuerteventura",
                    "Isla de Gran Canaria","Isla de Tenerife","La Gomera",
                    "Isla de La Palma","El Hierro")
cn_b <- cn_b[1:74,] %>%
  mutate(species = gsub("_"," ", species))

cn_b2 <- openxlsx::read.xlsx("data/raw-data/Canarias_Mammals_Birds.xlsx", sheet = "birds")
colnames(cn_b2) <- c("species","Status",
                     "El Hierro","Isla de La Palma","La Gomera","Isla de Tenerife",
                     "Isla de Gran Canaria","Isla de Fuerteventura","Isla de Lanzarote")
cn_b_tot <- bind_rows(cn_b, cn_b2 %>% select(-Status)) %>% distinct()
setdiff(cn_b2$species, cn_b$species)
setdiff(cn_b$species, cn_b2$species)
# correct species name when necessary
# serinus canarius = serinus canaria
# Miliaria calandra = Emberiza calandra
# Stigmatopelia senegalensis = Streptopelia senegalensis
# Phaethon aethereus => only vagrant, remove from data
# Nycticorax nycticorax = passage only, remove
# Pelagodroma marina, Milvus migrans, Larus fuscus => ok to keep

cn_b_temp <- cn_b_tot %>%
  mutate(species = if_else(species=="Serinus canarius", "Serinus canaria", species),
         species = if_else(species=="Miliaria calandra", "Emberiza calandra", species),
         species = if_else(species=="Stigmatopelia senegalensis", "Streptopelia senegalensis", species)) %>%
  filter(!species %in% c("Phaethon aethereus", "Nycticorax nycticorax")) %>%
  distinct()

length(unique(cn_b_temp$species))

dup <- cn_b_temp$species[duplicated(cn_b_temp$species)]

cn_dup <- cn_b_temp %>% filter(species %in% dup)

# take a conservative matrix: only present where both databases agree
# (following IUCN for some example species)
cn_pres <- cn_dup %>%
  group_by(species) %>% 
  summarise(across(everything(), sum))
cn_pres[cn_pres<2] = 0 # if not in both databases, put absence
cn_pres[cn_pres==2] = 1 # all presences to 1

# bind with non duplicated species to get the final db
cn_b_ok <- bind_rows(cn_b_temp %>% filter(!species %in% dup), cn_pres)

cn_bl <- cn_b_ok %>%
  pivot_longer(!species, names_to = "Island_name", values_to = "Occ") %>%
  filter(Occ==1) %>%
  select(-Occ)
length(unique(cn_bl$species)) # 77 native birds


# Mascarenes
# reunion
lr_b <- openxlsx::read.xlsx("data/raw-data/AVIBASE/AVIBASE_Reunion_accessed240709.xlsx")
# mauritius
m_b <- openxlsx::read.xlsx("data/raw-data/AVIBASE/AVIBASE_Mauritius_main_isl_accessed240709.xlsx")
# rodrigues
rg_b <- openxlsx::read.xlsx("data/raw-data/AVIBASE/AVIBASE_Mauritius_Rodrigues_accessed240709.xlsx")

masc_bl <- bind_rows(
  lr_b %>% mutate(Island_name = "Ilet du Gros Galet"), 
  m_b %>% mutate(Island_name = "Mauritius"), 
  rg_b %>% mutate(Island_name = "Ile Rodrigues")) %>%
  filter(!is.na(binomial)) %>%
  filter(!status %in% c("Rare/Accidentel", "Espèce introduite", "Extirpé")) %>%
  select(Island_name, binomial) %>% rename(species = binomial)
length(unique(masc_bl$species)) # 99 native birds


# bind all archipelagoes together for the clean final checklist

bird_ckl_clean <- left_join(
  bind_rows(masc_bl, azo_bl, cn_bl, hw_bl, glp_bl),
  isl_select %>% select(Island_name, ID)) %>%
  rename(ULM_ID = ID)
length(unique(bird_ckl_clean$species)) # 253 native birds
length(unique(bird_ckl_clean$Island_name)) # on 37 islands

# carreful: need to be cleaned with trait script (12_Vertebrate_traits)
saveRDS(bird_ckl_clean, "data/derived-data/10_bird_ckl_islands_notclean.RDS")

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




