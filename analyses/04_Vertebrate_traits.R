# vertebrate traits
rm(list=ls())


##### Load species lists from all archipelagoes #####

# Birds
birds <- readRDS("data/derived-data/02_bird_ckl_45_isl.rds")
# all lists together
all_b <- unique(birds$sci_name)

# Mammals

mam <- readRDS("data/derived-data/03_mammal_ckl_45_isl.rds")
mam_all <- unique(mam |> dplyr::pull(sci_name))


##### Habitat breadth from IUCN summary ####


# load IUCN summary
path="Z:/THESE/5_Data/IUCN_summary/MAJ_2023_06/"
amr <- read.csv(paste0(path, "amph_mam_rept/habitats.csv"))
bnp <- read.csv(paste0(path, "birds_nonpasserif/habitats.csv"))
bp <- read.csv(paste0(path, "birds_passerif/habitats.csv")) |> 
  dplyr::mutate(code = as.character(code))

hab <- dplyr::bind_rows(amr, bnp, bp)

hab_breadth <- hab |> 
  dplyr::mutate(code_sub = substr(code, 1,3)) |>
  dplyr::mutate(code_simple = floor(as.numeric(code_sub)))|>
  dplyr::filter(code_simple!=18) |>
  dplyr::distinct(scientificName, code_simple) |>
  dplyr::group_by(scientificName) |> dplyr::count()


# BIRDS 

hb_b <- hab_breadth |> dplyr::filter(scientificName %in% all_b)
length(all_b)-nrow(hb_b) # all species have habitat breadth information

# MAMMALS

hb_m <- hab_breadth |> dplyr::filter(scientificName %in% mam_all)
# all species have HB information
# (logical since the names come from IUCN ranges)


#### Load other trait databases #####

pathtrait="Z:/THESE/5_Data/Traits/"

# BIRDS

# Avonet for HWI, body mass, range size
avonet <- openxlsx::read.xlsx(paste0(
  pathtrait, "Birds_AVONET_Tobias/Supplementary dataset 1.xlsx"), sheet = 2)
# generation legnth estimates for all birds
# From Bird et al 2020 database
gl <- openxlsx::read.xlsx(paste0(pathtrait, "Bird_et_al_2020_Bird_generation_length_estimates.xlsx"))
# EltonTrait to have Diet_breadth
elton <- read.csv2(paste0(
  pathtrait, "EltonTraits_Birds_WIlman_2014.csv"))

# MAMMALS

# combine: reported traits only
comb_rep <- read.csv(paste0(
  pathtrait, "Mammals_COMBINE_archives/trait_data_reported.csv"))
# combine: including imputed traits
comb_imp <- read.csv(paste0(
  pathtrait, "Mammals_COMBINE_archives/trait_data_imputed.csv"))


# Area of habitat
aoh_m <- read.csv("data/raw-data/AoH/Mammals_list_AOH.csv")
aoh_b <- read.csv("data/raw-data/AoH/Birds_list_AOH.csv")


#### SELECT TRAITS AND MATCH TAXONOMIES ####

##### MAMMALS #####

# check the number of imputed values for each trait
traits_m_rep <- comb_rep |> # reported dataset
  dplyr::filter(iucn2020_binomial %in% mam_all)
traits_m_imp <- comb_imp |> # imputed dataset
  dplyr::filter(iucn2020_binomial %in% mam_all)
colSums(is.na(traits_m_rep))
colSums(is.na(traits_m_imp))
# body mass was reported for all species
# diet breadth has imputed values for 6 sp
# generation length has imputed values for 17 sp but litter size only for 7 sp
plot(comb_imp$litter_size_n, comb_imp$generation_length_d)
cor.test(comb_imp$litter_size_n, comb_imp$generation_length_d)

# Select the traits 
traits_m <- comb_imp |> # imputed dataset
  dplyr::filter(iucn2020_binomial %in% mam_all) |>
  # body mass, litter size, diet 
  dplyr::select(iucn2020_binomial, det_diet_breadth_n, habitat_breadth_n,
         adult_mass_g, generation_length_d, litter_size_n) |>
  dplyr::rename(scientificName = iucn2020_binomial)

traits_m_all <- dplyr::left_join(traits_m, hb_m)
# check if IUCN hab breadth are consistent together
plot(traits_m_all$habitat_breadth_n, traits_m_all$n)
# i'll take the updated IUCN summaries, so not hab breadth from COMBINE

traits_m_all <- traits_m_all |> 
  dplyr::select(-habitat_breadth_n) |> dplyr::rename(hab_breadth = n)

#AoH 
aoh_m_list <- aoh_m |> dplyr::filter(BINOMIAL %in% c(mam_all, "Pteropus mascarinus"))
setdiff(mam_all, aoh_m_list$BINOMIAL)
# does not exist for one chiroptera 
# (Pteropus rodricensis, including synonymous species Pteropus mascarinus)
# and does not work for seals...

# Lumbierres et al. 2022 README in data
# advise to take the range for species without AoH model
# We thus attribute a prevalence value of 1 
# to get AOH size, we multiply the IUCN range with AoH model prevalence

# open mam range
mam_range <- sf::st_read("data/raw-data/IUCN_RANGE/MAMMALS.shp")

# Union of polygons by species, calculate range area for each sp
mam_range_calc <- mam_range |> dplyr::filter(sci_name %in% mam_all) |> 
  dplyr::filter(presence %in% c(1:3) & origin %in% c(1:2) & seasonal %in% c(1:3)) |> 
  dplyr::filter(terrestria=="true") |>
  dplyr::group_by(sci_name) |>
  dplyr::summarize(geometry = sf::st_union(geometry)) |>
  dplyr::mutate(IUCN_range_km2 = round(units::set_units(sf::st_area(geometry), km^2), 2)) |>
  sf::st_drop_geometry()

traits_m_all_aoh <- dplyr::left_join(
  traits_m_all,
  dplyr::left_join(
    mam_range_calc |> dplyr::rename(scientificName = sci_name) |> units::drop_units(), 
    aoh_m |> dplyr::select(BINOMIAL, Model_prevalence)|> dplyr::rename(scientificName = BINOMIAL)))

colSums(is.na(traits_m_all_aoh))

traits_m_all_aoh[is.na(traits_m_all_aoh)] <-1
traits_m_all_aoh$AoH_range_km2 <- traits_m_all_aoh$IUCN_range_km2*traits_m_all_aoh$Model_prevalence

plot(traits_m_all_aoh$IUCN_range_km2, traits_m_all_aoh$AoH_range_km2)

# save trait data
saveRDS(traits_m_all_aoh, "data/derived-data/04_Mammal_traits.RDS")



##### BIRDS #####

avo_b <- avonet |>
  dplyr::filter(Species1 %in% hb_b$scientificName)

not_avo <- hb_b |> 
  dplyr::filter(!scientificName %in% avo_b$Species1) |> 
  dplyr::pull(scientificName)
# only 3 species
# check synonyms manually in IUCN

to_add <- avonet |>
  dplyr::mutate(scientificName = dplyr::case_when(
    Species1== "Sylvia conspicillata"~"Curruca conspicillata",
    Species1== "Sylvia melanocephala"~"Curruca melanocephala",
    Species1== "Gygis alba"~"Gygis candida",
  )) |>
  dplyr::filter(scientificName %in% not_avo)

avo_tot <- dplyr::bind_rows(
  avo_b |> dplyr::mutate(scientificName = Species1), to_add)

hb_avo_birds <- dplyr::left_join(avo_tot, hb_b) |>
  dplyr::select(scientificName, Species1, n, `Hand-Wing.Index`, Mass, Range.Size)

length(unique(hb_avo_birds$scientificName))
# 240 extant birds in total on all islands, with traits 


# generation length 
gl_b <- gl |>
  dplyr::filter(Scientific.name %in% unique(hb_avo_birds$scientificName)) |>
  dplyr::mutate(scientificName = Scientific.name)

# check missing species
setdiff(hb_avo_birds$scientificName, gl_b$Scientific.name)
# 5 missing sp with synonyms to add
gl_to_add <- gl |> 
  dplyr::filter(Scientific.name %in% c(
    "Psittacula eques", "Sylvia conspicillata", "Sylvia melanocephala", "Gygis alba", "Atlantisia rogersi")) |>
  dplyr::mutate(scientificName = dplyr::case_when(
    Scientific.name == "Atlantisia rogersi" ~ "Laterallus rogersi",
    Scientific.name == "Gygis alba" ~ "Gygis candida",
    Scientific.name == "Psittacula eques" ~ "Alexandrinus eques",
    Scientific.name == "Sylvia conspicillata" ~ "Curruca conspicillata",
    Scientific.name == "Sylvia melanocephala" ~ "Curruca melanocephala"
  ))

gl_all <- dplyr::bind_rows(gl_b, gl_to_add)

hb_avo_gl <- dplyr::left_join(hb_avo_birds, gl_all |> dplyr::select(scientificName, GenLength))


# diet breadth
elton_db <- elton |>
  dplyr::select(Scientific, Diet.Inv:Diet.PlantO)

elton_db[elton_db==0] <- NA
elton_db$nb_diet <- rowSums(!is.na(elton_db))-1


db <- elton_db |>
  dplyr::select(Scientific, nb_diet) |>
  dplyr::filter(Scientific %in% unique(hb_avo_birds$scientificName)) |>
  dplyr::mutate(scientificName = Scientific)
sp_to_find_elton <- setdiff(unique(hb_avo_birds$scientificName), db$Scientific)
# 58 species are missing, need to find synonyms


# Find synonyms using rredlist and IUCN API key
# syno_elton <- data.frame()
# 
# for (i in 1:length(sp_to_find_elton)){
#   obj <- rredlist::rl_synonyms(sp_to_find_elton[i],
#                      key = "0e9cc2da03be72f04b5ddb679f769bc29834a110579ccdbeb54b79c22d3dd53d")
#   syno_elton <- dplyr::bind_rows(syno_elton, obj$result)
#   saveRDS(syno_elton, "data/derived-data/04_synonyms_birds_Elton.rds")
#   print(i)
# }
syno_elton <- readRDS("data/derived-data/04_synonyms_birds_Elton.rds")
length(unique(syno_elton$accepted_name))

db_syno <- elton_db |>
  dplyr::select(Scientific, nb_diet) |>
  dplyr::filter(Scientific %in% unique(syno_elton$synonym))
db_syno <- dplyr::left_join(
  db_syno, 
  syno_elton |> dplyr::distinct(accepted_name, synonym) |>
    dplyr::rename(Scientific = synonym)) |>
  dplyr::rename(scientificName = accepted_name)

db_all <- dplyr::bind_rows(db, db_syno)

setdiff(unique(hb_avo_birds$scientificName), unique(db_all$scientificName))
# 15 species without correspondance, check manually

corresp <- c("Gallinula galeata" = "Gallinula chloropus",
             "Alaudala rufescens" = "Calandrella rufescens",
             "Fringilla polatzeki" = "Fringilla teydea",
             "Chasiempis sclateri" = "Chasiempis sandwichensis", 
             "Cyanistes teneriffae" = "Parus teneriffae",
             "Certhidea fusca" = "Certhidea olivacea", # only species from the same genus
             "Geospiza acutirostris" = "Geospiza difficilis",
             "Geospiza propinqua" = "Geospiza conirostris",
             "Geospiza septentrionalis" = "Geospiza difficilis",
             "Pyrocephalus nanus" = "Pyrocephalus rubinus", 
             "Zosterops mauritianus" = "Zosterops borbonicus",
             "Puffinus bailloni" = "Puffinus lherminieri",
             "Puffinus elegans" = "Puffinus assimilis",
             "Puffinus subalaris" = "Puffinus lherminieri",
             "Gygis candida" = "Gygis alba")

db_to_add <- dplyr::left_join(data.frame(Scientific = corresp, Syno = names(corresp)), elton_db)|> 
  dplyr::mutate(scientificName = Syno) |>
  dplyr::select(Scientific, scientificName, nb_diet)

db_all <- dplyr::bind_rows(db_all, db_to_add)

length(unique(db_all$scientificName))
length(unique(db_all$Scientific))

traits_b_all <- dplyr::left_join(
  hb_avo_gl, 
  db_all |> dplyr::select(-Scientific)) |> 
  dplyr::rename(nb_hab = n) |>
  dplyr::select(-Species1)



# AoH  

# Get IUCN range size (long task, do only when necessary)

# Load bird file names
# file_names <- list.files("data/derived-data/", pattern = "02_valid", full.names = T)
# bird_range_calc <- data.frame()
# for(f in file_names){ # Loop on each bird file (takes ~30min)
#   # open bird file
#   bshp <- readRDS(f)
#   bird_range_calc_temp <- bshp |>
#     dplyr::filter(sci_name %in% all_b) |>
#     # keep only native resident and breeding birds
#     dplyr::filter(presence %in% c(1:3) & origin %in% c(1:2) & seasonal %in% c(1:2)) |>
#     # group by species to have the size of species range's union
#     dplyr::group_by(sci_name) |>
#     dplyr::summarize(geometry = sf::st_union(geometry)) |>
#     sf::st_make_valid() |>
#     dplyr::mutate(IUCN_range_km2 = round(units::set_units(sf::st_area(geometry), km^2), 2)) |>
#     sf::st_drop_geometry()
#   
#   bird_range_calc <- dplyr::bind_rows(bird_range_calc, bird_range_calc_temp)
#   print(f)
# }
# saveRDS(bird_range_calc, "data/derived-data/04_IUCN_range_calculated.rds")

bird_range_calc <- readRDS("data/derived-data/04_IUCN_range_calculated.rds")

# check if all birds have AoH info
aoh_b_list <- aoh_b |> dplyr::filter(BINOMIAL %in% c(all_b)) |>
  dplyr::select(BINOMIAL, Model_prevalence)|>
  dplyr::mutate(scientificName = BINOMIAL) # for matching with final db
length(setdiff(all_b, aoh_b_list$BINOMIAL))
# 51 species are missing in AoH database
exclu_aoh <- read.csv("data/raw-data/AoH/Birds_list_excluded.csv")
sum(all_b %in% exclu_aoh$BINOMIAL)
# 46 species are in the list of excluded birds

# 5 species did not match any list
missing <- c("Curruca conspicillata" = "Sylvia conspicillata", 
  "Curruca melanocephala" = "Sylvia melanocephala", 
  "Gygis candida" = "Gygis alba",
  "Alexandrinus eques" = "Psittacula eques",
  "Laterallus rogersi" = "Atlantisia rogersi")

missing %in% aoh_b$BINOMIAL # 3 sp in Aoh DB
missing %in% exclu_aoh$BINOMIAL # 2 species in the list of excluded birds
# all the species are found!

# add the 3 species with info
to_match <- missing[missing %in% aoh_b$BINOMIAL]
aoh_to_add <- dplyr::left_join(data.frame(BINOMIAL = to_match, Syno = names(to_match)), aoh_b)|> 
  dplyr::mutate(scientificName = Syno) |>
  dplyr::select(BINOMIAL, scientificName, Model_prevalence)

aoh_all_b <- dplyr::bind_rows(aoh_b_list, aoh_to_add) |> dplyr::distinct()
# do we have all the species, either in aaoh db or in exluded birds?
nrow(aoh_all_b)+ sum(c(all_b, missing) %in% exclu_aoh$BINOMIAL)
# yes 240!!


# final join with trait db

traits_b_all_aoh <- dplyr::left_join(
  traits_b_all,
  dplyr::left_join(
    bird_range_calc |> dplyr::rename(scientificName = sci_name) |> units::drop_units(), 
    aoh_all_b |> dplyr::select(scientificName, Model_prevalence)))

colSums(is.na(traits_b_all_aoh))


traits_b_all_aoh[is.na(traits_b_all_aoh)] <-1
traits_b_all_aoh$AoH_range_km2 <- traits_b_all_aoh$IUCN_range_km2*traits_b_all_aoh$Model_prevalence


plot(traits_b_all_aoh$IUCN_range_km2, traits_b_all_aoh$AoH_range_km2)
colSums(is.na(traits_b_all_aoh))


saveRDS(traits_b_all_aoh, "data/derived-data/04_Bird_traits.RDS")


