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

str(tr_birds)
str(tr_mam)

# group species into FE -- birds

quant_mass_b <- quantile(tr_birds$Mass, c(0.25,0.5,0.75))
quant_genl_b <- quantile(tr_birds$GenLength, c(0.25,0.5,0.75))

tr_fr_b <- tr_birds |>
  dplyr::mutate(nb_hab_cat = ifelse(nb_hab>4, 5, nb_hab),
                nb_diet_cat = ifelse(nb_diet>4, 5, nb_diet),
                mass_cat = dplyr::case_when(
                  Mass < quant_mass_b[1] ~ 1,
                  Mass >= quant_mass_b[1] & Mass < quant_mass_b[2] ~ 2,
                  Mass >= quant_mass_b[2] & Mass < quant_mass_b[3] ~ 3,
                  Mass >= quant_mass_b[3] ~ 4),
                genlength_cat = dplyr::case_when(
                  GenLength < quant_genl_b[1] ~ 1,
                  GenLength >= quant_genl_b[1] & GenLength < quant_genl_b[2] ~ 2,
                  GenLength >= quant_genl_b[2] & GenLength < quant_genl_b[3] ~ 3,
                  GenLength >= quant_genl_b[3] ~ 4))|>
  dplyr::select(scientificName, contains("_cat")) |>
  dplyr::mutate(Class = "Aves")

# group species into FE -- mammals
quant_mass_m <- quantile(tr_mam$adult_mass_g, c(0.25,0.5,0.75))
quant_genl_m <- quantile(tr_mam$generation_length_d, c(0.25,0.5,0.75))

tr_fr_m <- tr_mam |>
  dplyr::mutate(nb_hab_cat = ifelse(hab_breadth>4, 5, hab_breadth),
                nb_diet_cat = ifelse(det_diet_breadth_n>4, 5, det_diet_breadth_n),
                mass_cat = dplyr::case_when(
                  adult_mass_g < quant_mass_m[1] ~ 1,
                  adult_mass_g >= quant_mass_m[1] & adult_mass_g < quant_mass_m[2] ~ 2,
                  adult_mass_g >= quant_mass_m[2] & adult_mass_g < quant_mass_m[3] ~ 3,
                  adult_mass_g >= quant_mass_m[3] ~ 4),
                genlength_cat = dplyr::case_when(
                  generation_length_d < quant_genl_m[1] ~ 1,
                  generation_length_d >= quant_genl_m[1] & generation_length_d < quant_genl_m[2] ~ 2,
                  generation_length_d >= quant_genl_m[2] & generation_length_d < quant_genl_m[3] ~ 3,
                  generation_length_d >= quant_genl_m[3] ~ 4))|>
  dplyr::select(scientificName, contains("_cat")) |>
  dplyr::mutate(Class = "Mammalia")



tr_fr <- dplyr::bind_rows(tr_fr_m, tr_fr_b)|>
  textshape::column_to_rownames("scientificName")|>
  dplyr::mutate_if(is.numeric, as.ordered) |>
  dplyr::mutate(Class = as.factor(Class))


# trait categories
tr_cat <- data.frame(
  trait_name = colnames(tr_fr),
  trait_type = c("O","O","O","O","N"),
  fuzzy_name = NA
)

#group species to FE 
sp_FEs <- mFD::sp.to.fe(
  sp_tr      = tr_fr, 
  tr_cat     = tr_cat, 
  fe_nm_type = "fe_rank")


# a matrix linking occurrences (coded as 0/1) of species (columns) in a set of assemblages (rows). 
str(birds)
str(mam)

mam_isl <- mam |> dplyr::rename(ID=ULM_ID) |> dplyr::distinct(ID, sci_name)
bird_isl<- dplyr::left_join(birds, isl |> sf::st_drop_geometry()) |> 
  dplyr::distinct(ID, sci_name)
sp_isl <- dplyr::bind_rows(mam_isl, bird_isl)

sp_isl_occ <- sp_isl |>
  dplyr::mutate(occ = 1) |>
  tidyr::pivot_wider(names_from = sci_name, 
                     values_from = occ,
                     values_fill = 0) |>
  textshape::column_to_rownames("ID")

length(unique(mam_isl$sci_name)) + length(unique(bird_isl$sci_name))
ncol(sp_isl_occ)
nrow(sp_isl_occ)


alpha <- mFD::alpha.fd.fe(
  asb_sp_occ = sp_isl_occ,
  sp_to_fe = sp_FEs
)

ac_biotic_com <- as.data.frame(alpha$asb_fdfe) |>
  tibble::rownames_to_column("ID") |>
  dplyr::mutate(ID = as.numeric(ID))



# normalize mass and hwi to get species dispersal for birds and mammals combined


min_hwi = min(tr_birds$`Hand-Wing.Index`)
max_hwi = max(tr_birds$`Hand-Wing.Index`)
#log transform mass because seals are super outsiders, especially southern elephant seal
tr_mam$log_mass <- log(tr_mam$adult_mass_g)
min_mass = min(tr_mam$log_mass)
max_mass = max(tr_mam$log_mass)

ac_biotic_sp <- dplyr::bind_rows(
  tr_birds |> 
    dplyr::mutate(dispersal = (`Hand-Wing.Index` - min_hwi)/(max_hwi-min_hwi)) |>
    dplyr::select(scientificName, dispersal),
  tr_mam |>
    dplyr::mutate(dispersal = (log_mass - min_mass)/(max_mass-min_mass)) |>
  dplyr::select(scientificName, dispersal)) |> 
  dplyr::rename(sci_name = scientificName)


hist(ac_biotic_sp$dispersal)
hist(tr_birds$`Hand-Wing.Index`)
hist(tr_mam$log_mass)

# add species occurrences on islands
ac_biotic_sp_isl <- dplyr::left_join(sp_isl, ac_biotic_sp)


saveRDS(ac_biotic_sp, "data/derived-data/11_Biotic_AC_BM_species.rds")
saveRDS(ac_biotic_com, "data/derived-data/11_Biotic_AC_BM_community.rds")


################################## see if useful after or not


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

