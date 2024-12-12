# Biotic AC, combined for mammals and birds
# birds = HWI, mammals = body size
# community level metric = Functional redundancy


rm(list=ls())


# load bird and mammal checklists per island
birds <- readRDS("data/derived-data/02_bird_ckl_45_isl.rds")
mam <- readRDS("data/derived-data/03_mammal_ckl_45_isl.rds")

# load traits
tr_birds <- readRDS("data/derived-data/04_Bird_traits.RDS")
tr_mam <- readRDS("data/derived-data/04_Mammal_traits.RDS")

# load islands
isl <- readRDS("data/derived-data/01_shp_45_major_isl.rds")


# calculate functional redundancy



# aggregate mean trait per island

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

