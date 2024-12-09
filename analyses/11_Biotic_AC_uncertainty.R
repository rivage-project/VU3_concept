# Biotic AC, combined for mammals and birds
# birds = HWI, mammals = body size
# community level metric = Functional redundancy


rm(list=ls())


# load bird, mammal, and plant checklists per island
birds <- readRDS("data/derived-data/12_bird_ckl_islands_clean.RDS")
mam <- readRDS("data/derived-data/10_mammals_inter_isl_clean.RDS")

# load traits
tr_birds <- readRDS("data/derived-data/")
tr_mam <- readRDS("data/derived-data/12_Mammal_traits.RDS")

# load islands
isl <- readRDS("data/derived-data/01_shp_45_major_isl.rds")


# aggregate mean trait per island

ac_plant <- dplyr::left_join(
  gift_isl_tra |> 
    dplyr::rename(max_height = trait_value_1.6.2) |>
    dplyr::select(work_species, max_height),
  dplyr::left_join(
    plant,
    isl_select |> dplyr::select(Island_name, ID) |> dplyr::rename(ULM_ID = ID)
))

ac_plant_isl <- ac_plant |>
  dplyr::group_by(Archip, Island_name, ULM_ID) |>
  dplyr::summarize(na = sum(is.na(max_height)),
            n_tot = dplyr::n(), 
            max_height = mean(max_height, na.rm = T)) |>
  dplyr::mutate(na_rate = na/n_tot) |> ungroup()


library(ggplot2)

ggplot(ac_plant_isl)+
  geom_boxplot(aes(x = Archip, y = na_rate))+
  geom_point(aes(x = Archip, y = na_rate, color = Archip, size = n_tot), position ="jitter")

ggplot(ac_plant_isl)+
  geom_boxplot(aes(x = Archip, y = max_height))+
  geom_point(aes(x = Archip, y = max_height, color = Archip, size = na_rate), position ="jitter")

# pbm with mean(max height) => higher in islands with higher NA rate...
# because plants that are reported in islands with few values are the tallest


# mammals
# bind checklists with traits
ckl_tr_m <- dplyr::left_join(
  mam |> dplyr::rename(scientificName = sci_name) |>
    dplyr::select(scientificName, ULM_ID, SHAPE_Area) |>
    dplyr::rename(Range.Size = SHAPE_Area), 
  tr_mam)

# aggregate values per islands for each group
ac_mam <- ckl_tr_m |> dplyr::select(ULM_ID, adult_mass_g) |>
  dplyr::group_by(ULM_ID) |> 
  dplyr::summarise(mass = mean(adult_mass_g))

# birds
# bind checklist with traits 
ckl_tr_b <- dplyr::left_join(birds , tr_birds |> dplyr::select(-species) |> dplyr::distinct())

# aggregate values per islands for each group
ac_b <- ckl_tr_b |> 
  dplyr::select(ULM_ID, `Hand-Wing.Index`) |>
  dplyr::group_by(ULM_ID) |> 
  dplyr::summarise(mean_hwi = mean(`Hand-Wing.Index`))

# join plants, mam, birds

ac <- dplyr::left_join(
  ac_mam, 
  dplyr::left_join(ac_plant_isl |> dplyr::select(ULM_ID, max_height), ac_b))


saveRDS(ac, "data/derived-data/31_AC_intrinsic_BMP.rds")

