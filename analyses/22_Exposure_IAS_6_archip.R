# Get alien species checklists
# for alien mammals and birds
# consider all alien species (not only invasive ones)

# METHODS
# define the list of species to look for:
# all the species listed in the GRIIS checlists 
# + species in other lists (Matthews, Rigal, etc)
# make a list of all alien birds and mammals in all the archipelagos
# get the GBIF identifier
# extract occurrences for alien species falling in the island polygons

rm(list = ls())

# Get island polygons
isl <- readRDS("data/derived-data/01_shp_45_major_isl.rds")
unique(isl$Archip)


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
# filter for birds and mammals 

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
  
  all <- dplyr::left_join(distrib, dplyr::left_join(profile, taxon))

  alien_can <- dplyr::bind_rows(alien_can, all)
}

# keep only birds, mammals


can_bm <- alien_can |>
  dplyr::filter(establishmentMeans =="Alien" & class %in% c("Aves","Mammalia")) |>
  dplyr::distinct(scientificName, class) |>
  dplyr::mutate(Archip ="Canary Islands")


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
  
  all <- dplyr::left_join(distrib, dplyr::left_join(profile, taxon))
  all$locationID = i
  
  alien_masc <- dplyr::bind_rows(alien_masc, all)
}

masc_bm <- alien_masc |>
  # pbm in mascarene: columns establishmentmeans and occurrencestatus are inversed
  # need to filter Alien status in both 
  dplyr::filter(establishmentMeans =="Alien" | occurrenceStatus=="Alien") |>
  dplyr::filter(class %in% c("Aves","Mammalia")) |>
  dplyr::distinct(scientificName, class) |>
  dplyr::mutate(Archip ="Mascarene Islands")
  
###### Azores -------
distrib <- readr::read_tsv(paste0("data/raw-data/alien_species/azores/",
                                  "dwca-griis-portugal-azores-v1.8", "/distribution.txt"))
profile <- readr::read_tsv(paste0("data/raw-data/alien_species/azores/",
                                  "dwca-griis-portugal-azores-v1.8", "/speciesprofile.txt"))
taxon <- readr::read_tsv(paste0("data/raw-data/alien_species/azores/",
                                "dwca-griis-portugal-azores-v1.8", "/taxon.txt"))
az <- dplyr::left_join(distrib, dplyr::left_join(profile, taxon))
az_b <- az |> dplyr::filter(class=="Aves" & establishmentMeans =="Alien")
# no alien birds according to griis
az_m <- az |> dplyr::filter(class=="Mammalia" & establishmentMeans =="Alien")
# 6 alien mammals according to griis


# compare with François' database of birds and mammals
azo_birds <- openxlsx::read.xlsx("data/raw-data/Azores_All_Mammals_Birds_names_ok.xlsx",
                                 sheet = 1)
# 7 alien birds according to François database
azo_mam <- openxlsx::read.xlsx("data/raw-data/Azores_All_Mammals_Birds_names_ok.xlsx",
                               sheet = 2)
azo_mam |> dplyr::filter(Status=="ALIEN") |> dplyr::pull(Species)
az_m |> dplyr::pull(scientificName)
# only 3 species are in common...

az_bm <- az |> 
  dplyr::filter(class %in% c("Aves", "Mammalia") & establishmentMeans =="Alien") |>
  dplyr::distinct(class, scientificName) |> 
  dplyr::mutate(Archip = "Azores")
# add mammals and birds from François database
az_bm_fr <- bind_rows(
  azo_birds |> dplyr::filter(Status=="ALIEN") |>
    dplyr::select(species) |> dplyr::mutate(class="Aves"),
  azo_mam |> filter(Status=="ALIEN") |>
    dplyr::select(Species) |> dplyr::mutate(class="Mammalia") |> 
    dplyr::rename(species = Species)) |>
  dplyr::mutate(Archip = "Azores",
         species = gsub("_", " ", species)) |>
  dplyr::rename(scientificName = species)

az_bm <- dplyr::bind_rows(az_bm, az_bm_fr)

###### Galapagos  -------

# from griis 
distrib <- readr::read_tsv(paste0("data/raw-data/alien_species/galapagos/",
                                  "dwca-griis_galapagos_islands-v1.5", "/distribution.txt"))
profile <- readr::read_tsv(paste0("data/raw-data/alien_species/galapagos/",
                                  "dwca-griis_galapagos_islands-v1.5", "/speciesprofile.txt"))
taxon <- readr::read_tsv(paste0("data/raw-data/alien_species/galapagos/",
                                "dwca-griis_galapagos_islands-v1.5", "/taxon.txt"))
glp <- left_join(distrib, left_join(profile, taxon))

glp_bm <- glp |> 
  dplyr::filter(class %in% c("Aves", "Mammalia") & establishmentMeans =="Alien") |>
  dplyr::distinct(class, scientificName) |> 
  dplyr::mutate(Archip = "Galapagos Islands")



###### Hawaii -------

# GBIF.org (18 April 2024) GBIF Occurrence Download https://doi.org/10.15468/dl.afsuaj

hw = data.table::fread("data/raw-data/alien_species/hawaii/0192966-240321170329656.csv")

hw_bm <- hw |>
  dplyr::filter(class %in% c("Aves","Mammalia"))
table(hw_bm$class) 
# 57 birds, 17 mammals => looks underestimated
griis_b <- hw_bm |> dplyr::filter(class=="Aves") |> dplyr::pull(verbatimScientificName)

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
  syno_griis <- dplyr::bind_rows(syno_griis, obj$result)
}

syno_mat <- data.frame()
for (i in hw_alien_b[! hw_alien_b %in% griis_b]){
  obj <- rredlist::rl_synonyms(i,
                               key = "0e9cc2da03be72f04b5ddb679f769bc29834a110579ccdbeb54b79c22d3dd53d")
  syno_mat <- dplyr::bind_rows(syno_mat, obj$result)
}

syno <- dplyr::bind_rows(syno_mat, syno_griis) |> dplyr::distinct()

not_common <- griis_b[! griis_b %in% hw_alien_b]
for(i in not_common){
  if (i %in% syno$accepted_name | i %in% syno$synonym){
    print(i)
  }
}

# extract only bird and mammal names
# from GRIIS and Matthews to have the most complete list

hw_bm <- bind_rows(
  hw |>
    dplyr::filter(class %in% c("Aves","Mammalia")) |>
    dplyr::distinct(scientificName, class) |>
    dplyr::mutate(Archip ="Hawaii"),
  hw_matt_all |>
    dplyr::mutate(scientificName = gsub("_", " ", species),
           class = "Aves", 
           Archip = "Hawaii") |>
    dplyr::filter(scientificName %in% hw_alien_b[! hw_alien_b %in% griis_b])|>
    dplyr::distinct(scientificName, class, Archip)
)


###### Tristan da Cunha -------

#https://doi.org/10.15468/30wm6u
# from griis 
distrib <- readr::read_tsv("data/raw-data/alien_species/tristan/distribution.txt")
profile <- readr::read_tsv("data/raw-data/alien_species/tristan/speciesprofile.txt")
taxon <- readr::read_tsv("data/raw-data/alien_species/tristan/taxon.txt")
tdc <- dplyr::left_join(distrib, dplyr::left_join(profile, taxon))

tdc_bm <- tdc |> 
  dplyr::filter(class %in% c("Aves", "Mammalia") & establishmentMeans =="Alien") |>
  dplyr::distinct(class, scientificName) |> 
  dplyr::mutate(Archip = "Tristan da Cunha Islands")

tdc |> dplyr::filter(class %in% c("Aves", "Mammalia") & establishmentMeans =="Alien") |> 
  dplyr::distinct(locality, scientificName)

ias_tdc <- tdc |> dplyr::filter(class %in% c("Aves", "Mammalia") & establishmentMeans =="Alien") |> 
  dplyr::distinct(locality, scientificName) |>
  # in TIB & DIISE => eradication of S. scrofa + C. hircus + F. catus on TdC
  # eradication of S. scrofa on Inaccessible
  dplyr::filter(locality =="Tristan da Cunha")|>
  dplyr::filter(!scientificName %in% c("Felis catus Linnaeus, 1758", "Sus scrofa Linnaeus, 1758"))

#### combine all checklists ----

all_bm <- dplyr::bind_rows(az_bm, can_bm, hw_bm, glp_bm, masc_bm, tdc_bm) 
length(unique(all_bm$scientificName))
gbif_names <- rgbif::name_backbone_checklist(all_bm)

sum(gbif_names$rank =="SPECIES")
# some are subspecies

# collect all information at the species level
# work with gbif taxo ID

length(unique(gbif_names$speciesKey))
length(sort(gbif_names$verbatim_name))
length(unique(gbif_names$verbatim_index))
cklist <- dplyr::left_join(all_bm |> 
                             dplyr::select(Archip) |> 
                             dplyr::mutate(verbatim_index = 1:nrow(all_bm)), 
                    gbif_names) |>
  dplyr::distinct(Archip, usageKey, speciesKey, class)

table(cklist |> dplyr::distinct(speciesKey, class) |> dplyr::pull(class))
length(unique(cklist |> dplyr::distinct(speciesKey, class) |> dplyr::pull(speciesKey)))

saveRDS(cklist, "data/derived-data/alien_occ/22_list_alien_birds_mam_6_archip.rds")


# Transform island polygons as WKT for passing to rgbif
wkt <- lapply(as.list(unique(isl$Archip)), function(a){
  archip = isl |> dplyr::filter(Archip==a)
  ar_union <- sf::st_union(archip) |>
    sf::st_as_text()
  return(ar_union)
})
names(wkt) <- unique(isl$Archip)

saveRDS(wkt, "data/derived-data/22_6_archip_wkt_for_GBIF.rds")

# check if ok
wkt_df <- data.frame(archip = names(wkt),
                     geometry = unlist(wkt))
sf_wkt <- sf::st_as_sf(wkt_df, wkt = "geometry")
library(ggplot2)
ggplot(sf_wkt[1,])+geom_sf(aes(fill=archip))


###### Download occurrences #######

# Using gbif.org credentials 
user <- "claramarino" # your gbif.org username 
pwd <- "data2021" # your gbif.org password
email <- "claramarino665@gmail.com" # your email 

# load checklist
cklist <- readRDS("data/derived-data/alien_occ/22_list_alien_birds_mam_6_archip.rds")
# load island polygons
wkt <- readRDS("data/derived-data/22_6_archip_wkt_for_GBIF.rds")


# occ_archip <- lapply(as.list(names(wkt)), function(a){
#   occ_i <- rgbif::occ_download(
#     rgbif::pred_in("taxonKey", cklist |> filter(Archip==a) |> pull(usageKey)),
#     rgbif::pred("hasCoordinate", TRUE),
#     rgbif::pred("hasGeospatialIssue", FALSE),
#     rgbif::pred("geometry", wkt[[a]]),
#     format = "SIMPLE_CSV",
#     user=user,pwd=pwd,email=email
#   )
#   
#   meta <- rgbif::occ_download_meta(occ_i)
#   return(meta)
# })
# Erreur : A download limitation is exceeded:
# User claramarino has too many simultaneous downloads; the limit is 3.
# Please wait for some to complete, or cancel any unwanted downloads. 

# do it for archipelagos 1-2-3 and then 4-5-6
occ_archip123 <- lapply(as.list(names(wkt)[1:3]), function(a){
  occ_i <- rgbif::occ_download(
    rgbif::pred_in("taxonKey", cklist |> filter(Archip==a) |> pull(usageKey)),
    rgbif::pred("hasCoordinate", TRUE),
    rgbif::pred("hasGeospatialIssue", FALSE),
    rgbif::pred("geometry", wkt[[a]]),
    format = "SIMPLE_CSV",
    user=user,pwd=pwd,email=email
  )
  meta <- rgbif::occ_download_meta(occ_i)
  return(meta)
})
# wait for the downloads to proceed to start the next request
occ_archip456 <- lapply(as.list(names(wkt)[4:6]), function(a){
  occ_i <- rgbif::occ_download(
    rgbif::pred_in("taxonKey", cklist |> dplyr::filter(Archip==a) |> dplyr::pull(usageKey)),
    rgbif::pred("hasCoordinate", TRUE),
    rgbif::pred("hasGeospatialIssue", FALSE),
    rgbif::pred("geometry", wkt[[a]]),
    format = "SIMPLE_CSV",
    user=user,pwd=pwd,email=email
  )
  meta <- rgbif::occ_download_meta(occ_i)
  return(meta)
})
# combine the metadata
occ_archip = c(occ_archip123, occ_archip456)
names(occ_archip) <- names(wkt) 

saveRDS(occ_archip, "data/raw-data/alien_species/gbif_occ/22_metadata_GBIF_6_archip.rds")

# get gbif metadata
meta <- readRDS("data/raw-data/alien_species/gbif_occ/22_metadata_GBIF_6_archip.rds")

lapply(meta, function(x){x$doi})

# once all downloads are ready, get the .csv files 
for (i in 1:length(occ_archip)){
  rgbif::occ_download_get(
    occ_archip[[i]]$key, 
    path = "data/raw-data/alien_species/gbif_occ"
  )
  print(i)
}

names(occ_archip)

###### Clean GBIF records #####

# functions from CoordinateCleaner package + RISK project
source("R/clean_coordinates_gbif.R")

# open occ files from all archipelagos, combine in a single df
occ_all <- data.frame()
files_all <- list.files("data/raw-data/alien_species/gbif_occ/")
files = files_all[grepl(".zip", files_all)]
for (i in files){
  occ <- readr::read_tsv(unzip(paste0(
  "data/raw-data/alien_species/gbif_occ/",i))) |>
    dplyr::mutate(recordNumber = as.character(recordNumber),
                  eventDate= as.character(eventDate)) |>
    dplyr::mutate()
  if(nrow(occ)>0){
    occ_all <- dplyr::bind_rows(occ_all, occ)
  }
  
}
str(occ_all)

# get clean coordinates
occ_clean <- clean_coord_base(occ_all)

# remove points with flagged errors
occ_clean_no_flag <- flag_rm_terr(occ_clean)

# species difference?
sp_diff(occ_all, occ_clean_no_flag)
# 1 species has no more alien occurrence on any of the archip
# cavia porcellus

# how many species in total?
length(unique(occ_clean_no_flag$speciesKey))
length(unique(occ_clean_no_flag$taxonKey))
length(unique(cklist$speciesKey))


saveRDS(occ_clean_no_flag, 
        "data/derived-data/alien_occ/22_GBIF_clean_alien_occ_124sp.rds")


##### Compute alien checklists for each island ######

occ <- readRDS("data/derived-data/alien_occ/22_GBIF_clean_alien_occ_124sp.rds")
# transform occ to sf object

coord <- occ |>
  dplyr::mutate_at(dplyr::vars(LONG, LAT), as.numeric) |>   # coordinates must be numeric
  sf::st_as_sf(
    coords = c("LONG", "LAT"),
    agr = "constant",
    crs = "+proj=longlat +datum=WGS84",
    stringsAsFactors = FALSE,
    remove = TRUE)

#ggplot(coord)+geom_sf()

# get the intersection of all occ and isl shapefiles
sum(sf::st_is_valid(isl))
sum(sf::st_is_valid(coord))

sf::st_crs(isl) == sf::st_crs(coord)

inter <- sf::st_intersects(isl, coord)

# dataframe for each island
out = inter
out_df = data.frame()
for(i in 1:length(inter)){
  print(i)
  if(!rlang::is_empty(inter[[i]])){
    out[[i]] = coord[inter[[i]],] |> dplyr::distinct(taxonKey, speciesKey, species, geometry)
    out[[i]]$ID = isl$ID[i]
    out_df <- dplyr::bind_rows(out_df, out[[i]] |> dplyr::distinct(taxonKey, speciesKey, species, ID))
  }
}

# get the checklist of IAS per island, aggregated in one df
ias_archip <- dplyr::left_join(
  out_df, 
  isl |> sf::st_drop_geometry() |> dplyr::select(ID, Archip, Island)) |>
  dplyr::distinct(speciesKey, species, ID, Archip, Island)
  # dplyr::mutate(ISLAND = if_else(ISLAND == "El Hierro","Isla de El Hierro", ISLAND)) |>
  # dplyr::mutate(ISLAND = if_else(ISLAND == "La Gomera","Isla de La Gomera", ISLAND))

# add island from Tristan da Cunha
# because no occurrence with GBIF on this archipelago
# but still some species occurring => double check with TIB and DIISE databases
distrib <- readr::read_tsv("data/raw-data/alien_species/tristan/distribution.txt")
profile <- readr::read_tsv("data/raw-data/alien_species/tristan/speciesprofile.txt")
taxon <- readr::read_tsv("data/raw-data/alien_species/tristan/taxon.txt")
tdc <- dplyr::left_join(distrib, dplyr::left_join(profile, taxon))

ias_tdc <- tdc |> dplyr::filter(class %in% c("Aves", "Mammalia") & establishmentMeans =="Alien") |> 
  dplyr::distinct(locality, scientificName) |>
  # in TIB & DIISE => eradication of S. scrofa + C. hircus + F. catus on TdC
  # eradication of S. scrofa on Inaccessible
  dplyr::filter(locality =="Tristan da Cunha")|>
  dplyr::filter(!scientificName %in% c("Felis catus Linnaeus, 1758", "Sus scrofa Linnaeus, 1758")) |>
  dplyr::mutate(speciesKey = 0,
                species = stringr::word(scientificName, 1,2),
                Archip = "Tristan da Cunha Islands",
                ID = 7733) |>
  dplyr::rename(Island = locality) |> dplyr::select(-scientificName) |>
  dplyr::mutate(species = dplyr::if_else(species=="Canis familiaris","Canis lupus", species))

ias_archip_all <- dplyr::bind_rows(ias_archip, ias_tdc)
table(ias_archip_all$Archip)
length(unique(ias_archip_all$Island))
length(unique(ias_archip_all$speciesKey)) # because some zero but not useful
length(unique(ias_archip_all$species))
test <- ias_archip_all |> dplyr::distinct(species, speciesKey)

saveRDS(ias_archip_all, "data/derived-data/22_ias_6_archip_BM_ckl.rds")


# for each island with occ
# count the number of 1km²-cells that have at least one alien occurrence
# separately for alien birds and mammals
df_alien_range <- data.frame()
for (i in 1:length(out)){
  if(length(out[[i]])>0){
    
    out[[i]] = coord[inter[[i]],] |> dplyr::distinct(taxonKey, speciesKey, species, geometry)
    out[[i]]$ID = isl$ID[i]
    
    # transform points and polygons to Mollweide projection
    # for having an equal-area cell grid
    occ <- sf::st_transform(out[[i]], crs = "+proj=moll")
    isl_temp <- sf::st_transform(isl |> dplyr::filter(ID==unique(occ$ID)), crs = "+proj=moll")
    grid <- sf::st_make_grid(isl_temp, c(1000, 1000), what = "polygons", square = T)
    grid_sf = sf::st_sf(grid) |>
      dplyr::mutate(grid_id = 1:length(lengths(grid)))
    
    df <- data.frame(
      ID = isl_temp$ID,
      island_cells = length(sf::st_intersects(isl_temp, grid_sf)[[1]]),
      occ_cells_tot = length(unique(unlist(sf::st_intersects(occ, grid_sf))))
    )
    
    df_alien_range <- dplyr::bind_rows(df_alien_range, df)
    print(i)
  }
}

saveRDS(df_alien_range, "data/derived-data/22_df_alien_range_BM.rds")

#############

# Create a database with IAS info for each island
# - nb of alien vertebrates & prop of alien vertebrates compared to native sp
# - % of 1km² cells with at least one alien vertebrate occurrence

df_alien_range <- readRDS("data/derived-data/22_df_alien_range_BM.rds")
ias_archip <- readRDS( "data/derived-data/22_ias_6_archip_BM_ckl.rds")

bm <- ias_archip |>
  dplyr::select(-speciesKey) |> dplyr::distinct() |>
  dplyr::group_by(ID, Archip, Island) |>
  dplyr::summarize(nb_alien = dplyr::n())
# get alien range cover for final exposure to IAS
bm_cover <- dplyr::left_join(bm, df_alien_range) |>
  dplyr::mutate(island_cells = ifelse(ID==7733, 98, island_cells),
                occ_cells_tot = ifelse(ID==7733, 1, occ_cells_tot)) |>
  dplyr::mutate(alien_vert_cover = occ_cells_tot/island_cells)

expo_ias <- dplyr::left_join(
  isl |> sf::st_drop_geometry() |> dplyr::select(ID, Island),
  bm_cover |> dplyr::ungroup() |> dplyr::select(ID, nb_alien:alien_vert_cover) 
)

expo_ias[is.na(expo_ias)] <- 0

# save final dataset
saveRDS(expo_ias, "data/derived-data/22_IAS_exposure_45_isl.rds")

#############

# plot simple relationships
expo_ias <- readRDS("data/derived-data/22_IAS_exposure_45_isl.rds")

expo_ias <- dplyr::left_join(expo_ias, isl |> dplyr::select(ID, Archip))

# alien plants vs alien vertebrates
ggplot(expo_ias) +
  geom_point(aes(x=nb_alien, y= alien_vert_cover, color = Archip), 
             alpha = .5, size = 3)

# alien birds vs alien mammals
ggplot(expo_ias) +
  geom_point(aes(x=nb_alien_bird, y= nb_alien_mam, color = ARCHIP), 
             alpha = .5, size = 3)

# richness of alien vertebrates vs alien vertebrate cover
ggplot(expo_ias) +
  geom_point(aes(x=nb_alien_vert, y= alien_vert_cover, color = ARCHIP), 
             alpha = .5, size = 3)



ggplot(expo_ias) +
  geom_boxplot(aes(y = nb_alien_vert, x = ARCHIP))+
  geom_point(aes(y = nb_alien_vert, x = ARCHIP, color = ARCHIP), 
             alpha = .5, size = 3)

