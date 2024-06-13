# retrieve checklists for all the major islands, for birds and mammals
library("tidyverse")

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


# birds from Galapagos 


# birds from Hawaii
hw_matt_all <- read.csv("data/raw-data/Matthews_et_al_2023/txm676-DARs-8b381ff/Data/Island_datasets/True_island_datasets/Alternative_versions/Alien_versions/Baiser et al (2017) Hawaii Birds_current_withAlien.csv")
hw_matt_nat <- read.csv2("data/raw-data/Baiser et al (2017) Hawaii Birds_current_noAliens.csv")

hw_alien_b <- gsub("_"," ", setdiff(hw_matt_all$species, hw_matt_nat$species))

sum(griis_b %in% hw_alien_b) # 11 species not in common from each list

griis_b[! griis_b %in% hw_alien_b]
hw_alien_b[! hw_alien_b %in% griis_b]




