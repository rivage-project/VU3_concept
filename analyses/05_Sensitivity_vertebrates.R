# Sensitivity for vertebrates

rm(list=ls())


# load bird and mammal checklists per island
birds <- readRDS("data/derived-data/02_bird_ckl_45_isl.rds")
mam <- readRDS("data/derived-data/03_mammal_ckl_45_isl.rds")

# load traits
tr_birds <- readRDS("data/derived-data/04_Bird_traits.RDS")
tr_mam <- readRDS("data/derived-data/04_Mammal_traits.RDS")

# load islands
isl <- readRDS("data/derived-data/01_shp_45_major_isl.rds")



# Combine birds and mammals



# normalize metrics that have not the same units between birds and mammals
# generation length
# min_glb = min(tr_birds$GenLength)
# max_glb = max(tr_birds$GenLength)
# 
# min_glm = min(tr_mam$generation_length_d)
# max_glm = max(tr_mam$generation_length_d)

# or convert all metric to same unit:
#   - nb diet, nb hab and AoH have the same unit
#   - gen length is in days for mammals and years for birds => convert days to years 

sens_sp <- dplyr::bind_rows(
  tr_birds |> 
    dplyr::mutate(gen_length_y = GenLength) |> #(GenLength - min_glb)/(max_glb-min_glb)
    dplyr::select(scientificName, nb_hab, nb_diet, AoH_range_km2, gen_length_y),
  tr_mam |>
    dplyr::mutate(gen_length_y = generation_length_d/365) |> #(generation_length_d - min_glm)/(max_glm-min_glm)
    dplyr::rename(nb_diet = det_diet_breadth_n,
                  nb_hab = hab_breadth) |>
    dplyr::select(scientificName, nb_hab, nb_diet, AoH_range_km2, gen_length_y)) |> 
  dplyr::rename(sci_name = scientificName,
                aoh_km2 = AoH_range_km2)


hist(sens_sp$gen_length_y)
hist(tr_birds$GenLength)
hist(tr_mam$generation_length_d/365)

# add species occurrences on islands

mam_isl <- mam |> dplyr::rename(ID=ULM_ID) |> dplyr::distinct(ID, sci_name)
bird_isl<- dplyr::left_join(birds, isl |> sf::st_drop_geometry()) |> 
  dplyr::distinct(ID, sci_name)
sp_isl <- dplyr::bind_rows(mam_isl, bird_isl)

sens_sp_isl <- dplyr::left_join(sp_isl, sens_sp)


saveRDS(sens_sp_isl, "data/derived-data/05_traits_BM_45_isl.rds")





#################################### Let's see after if its useful

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
