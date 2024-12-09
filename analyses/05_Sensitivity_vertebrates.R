# Sensitivity for vertebrates
# => use script from WS#2 to include uncertainty

rm(list=ls())
library("tidyverse")


# load bird and mammal checklists per island
birds <- readRDS("data/derived-data/12_bird_ckl_islands_clean.RDS")
mam <- readRDS("data/derived-data/10_mammals_inter_isl_clean.RDS")

# load traits
tr_birds <- readRDS("data/derived-data/12_Bird_traits.RDS")
tr_mam <- readRDS("data/derived-data/12_Mammal_traits.RDS")

# load islands
isl_select <- read.csv("data/derived-data/01_selected_islands.csv")
minor_islands <- c(
  "Isla Graciosa","Isla de Alegranza","Lobos","Isla de Montana Clara", # Canary
  "Lisianski Island","Sand Island","Lehua","Laysan Island", "Ford Island", # Hawaii
  "Ile d' Ambre", # Mascarene
  "Isla Bartolome", "Isla Seymour", "Isla Darwin"# Galapagos
)

##### Mammals #####

# bind checklists with traits
ckl_tr_m <- left_join(
  mam %>% rename(scientificName = sci_name) %>%
    select(scientificName, ULM_ID, SHAPE_Area) %>%
    rename(Range.Size = SHAPE_Area), 
  tr_mam)

# aggregate values per islands for each group
sens_mam <- ckl_tr_m %>% select(-scientificName) %>%
  group_by(ULM_ID) %>% 
  summarise(across(everything(), mean))

colnames(sens_mam)

# select major or all islands
sens_mam_major <- sens_mam %>%
  filter(!ULM_ID %in% isl_select$ID[isl_select$Island_name %in% minor_islands])

sm_max_min  <- sm_log <- sm_rank <- x <- sens_mam_major %>% column_to_rownames("ULM_ID") 

#------ initialization
# max min linear
maxcol <- apply(x, 2, max, na.rm=T)
mincol <- apply(x, 2, min, na.rm=T)
# log transformed
maxlog <- apply(x, 2, function(x){max(log(x+1), na.rm = T)})
minlog <- apply(x, 2, function(x){min(log(x+1), na.rm = T)})
# ranks
x_rank <- x %>%
  dplyr::mutate_all(dense_rank)
maxrank <- apply(x_rank, 2, max, na.rm=T)
minrank <- apply(x_rank, 2, min, na.rm=T)

#------ normalization
for (i in 1:length(maxcol)){
  # max min
  sm_max_min[,i] <- (x[,i]-mincol[i])/(maxcol[i]-mincol[i])
  # log-transformed
  sm_log[,i] <- ((log(x[,i]+1)-minlog[i]))/
    (maxlog[i]-minlog[i])
  # ranks
  sm_rank[,i] <- (x_rank[,i]-minrank[i])/(maxrank[i]-minrank[i])
}

summary(sm_max_min)
summary(sm_log)
summary(sm_rank)

sm_norm = list(
  sm_max_min = sm_max_min,
  sm_log = sm_log,
  sm_rank = sm_rank
)

#### Sum components to get final sensitivity

# add number of species per isl
sr_m <- mam %>% group_by(ULM_ID) %>% summarise(SR_mam = n())

sm_agg <- lapply(sm_norm, function(x){
  left_join(
    x %>%
      mutate(diet_specialism = 1 - det_diet_breadth_n, 
             hab_specialism = 1 - hab_breadth,
             restricted_range = 1 - Range.Size) %>%
      mutate(sens = restricted_range + diet_specialism + hab_specialism + generation_length_d) %>%
      select(sens, restricted_range, diet_specialism, hab_specialism, generation_length_d) %>% 
      rownames_to_column("ULM_ID") %>% 
      mutate(ULM_ID = as.integer(ULM_ID)), 
    sr_m) %>%
    rename(ID = ULM_ID)
})


saveRDS(sm_agg, "data/derived-data/13_Sensitivity_mam_major_isl.rds")


###### birds #####

#bind checklist with traits 
ckl_tr_b <- left_join(birds , tr_birds %>% select(-species) %>% distinct())

# aggregate values per islands for each group
sens_b <- ckl_tr_b %>% 
  select(-c(sci_name_IUCN, Island_name)) %>%
  group_by(ULM_ID) %>% 
  summarise(across(everything(), \(x) mean(x, na.rm = TRUE)))

# select major or all islands
sens_b_major <- sens_b %>%
  filter(!ULM_ID %in% isl_select$ID[isl_select$Island_name %in% minor_islands])

sb_max_min  <- sb_log <- sb_rank <- x <- sens_b_major %>% column_to_rownames("ULM_ID") 

#------ initialization
# max min linear
maxcol <- apply(x, 2, max, na.rm=T)
mincol <- apply(x, 2, min, na.rm=T)
# log transformed
maxlog <- apply(x, 2, function(x){max(log(x+1), na.rm = T)})
minlog <- apply(x, 2, function(x){min(log(x+1), na.rm = T)})
# ranks
x_rank <- x %>%
  dplyr::mutate_all(dense_rank)
maxrank <- apply(x_rank, 2, max, na.rm=T)
minrank <- apply(x_rank, 2, min, na.rm=T)

#------ normalization
for (i in 1:length(maxcol)){
  # max min
  sb_max_min[,i] <- (x[,i]-mincol[i])/(maxcol[i]-mincol[i])
  # log-transformed
  sb_log[,i] <- ((log(x[,i]+1)-minlog[i]))/
    (maxlog[i]-minlog[i])
  # ranks
  sb_rank[,i] <- (x_rank[,i]-minrank[i])/(maxrank[i]-minrank[i])
}

summary(sb_max_min)
summary(sb_log)
summary(sb_rank)

sb_norm = list(
  sb_max_min = sb_max_min,
  sb_log = sb_log,
  sb_rank = sb_rank
)


#### Sum components to get final sensitivity

# add number of species per isl
sr_b <- birds %>% group_by(ULM_ID) %>% summarise(SR_bird = n())
colnames(sb_norm$sb_max_min)
sb_agg <- lapply(sb_norm, function(x){
  left_join(
    x %>%
      mutate(diet_specialism = 1 - nb_hab, 
             hab_specialism = 1 - nb_diet,
             restricted_range = 1 - Range.Size) %>%
      mutate(sens = restricted_range + diet_specialism + hab_specialism + GenLength) %>%
      select(sens, restricted_range, diet_specialism, hab_specialism, GenLength) %>% 
      rownames_to_column("ULM_ID") %>% 
      mutate(ULM_ID = as.integer(ULM_ID)), 
    sr_b) %>%
    rename(ID = ULM_ID)
})


saveRDS(sb_agg, "data/derived-data/13_Sensitivity_bird_major_isl.rds")

###### Explore sensitivity variations ######
sm_norm <- readRDS("data/derived-data/13_Sensitivity_mam_major_isl.rds")

sens_mam <- left_join(
  sm_norm[["sm_max_min"]],
  isl_select %>% select(ID, Island_name, Archip, Area, Dist, Elev, SLMP, Lat)
)

hist(sens_mam$sens)

ggplot(sens_mam)+
  geom_boxplot(aes(x=Archip, y = sens))+
  geom_point(aes(x=Archip, y = sens, size = SR_mam, color = Archip), 
             position = 'jitter', alpha = .5)+
  theme_classic()


# birds
sb_norm <- readRDS("data/derived-data/13_Sensitivity_bird_major_isl.rds")

sens_b <- left_join(
  sb_norm[["sb_log"]],
  isl_select %>% select(ID, Island_name, Archip, Area, Dist, Elev, SLMP, Lat)
)

hist(sens_b$sens)

ggplot(sens_b)+
  geom_boxplot(aes(x=Archip, y = sens))+
  geom_point(aes(x=Archip, y = sens, size = SR_bird, color = Archip), 
             position = 'jitter', alpha = .5)+
  theme_classic()


###### Checklist on major islands


mam2 <- left_join(mam %>% select(ULM_ID, sci_name, kingdom:genus) %>% rename(ID = ULM_ID), 
                  isl_select %>% select(ID, Island_name, Archip))
bird2 <- left_join(birds %>% rename(ID = ULM_ID), 
                   isl_select %>% select(ID, Island_name, Archip))


write.csv2(bird2, "data/derived-data/13_Checklist_BIRDS.csv", row.names = F)
write.csv2(mam2, "data/derived-data/13_Checklist_MAMMALS.csv", row.names = F)
