rm(list=ls())

##### MAMMALS ####

# most of mammal checklists in internet were from IUCN
# and checklists are not by island but by archipelago
# so intersect island polygons with IUCN mammal ranges
# and then perform a manual check for each archip

# IUCN version 2024-1, accessed on 09/07/2024

isl <- readRDS("data/derived-data/01_shp_45_major_isl.rds")

# takes time (~20 min) => run only if necessary
mam <- sf::st_read("data/raw-data/IUCN_RANGE/MAMMALS.shp")
# colnames(mam)
# summary(mam)
mam <- mam |>
  dplyr::filter(presence %in% c(1:3) & origin %in% c(1:2) & seasonal %in% c(1:3)) |>
  dplyr::filter(terrestria=="true") |>
  sf::st_make_valid()

mam_valid <- mam[sf::st_is_valid(mam),]
inter <- sf::st_intersects(isl, mam_valid)

names(inter) <- isl$ULM_ID

mam_ckl <- data.frame()
for (i in 1:length(inter)){
  m <- mam_valid[inter[[i]],] |> sf::st_drop_geometry()
  if(nrow(m)>0){
    m$ULM_ID <- isl$ID[i]
    mam_ckl <- dplyr::bind_rows(mam_ckl,m)
  }
}
saveRDS(mam_ckl, "data/derived-data/03_mammals_inter_45_isl.rds")

mam_ckl <- readRDS("data/derived-data/03_mammals_inter_45_isl.rds")
unique(mam_ckl |> dplyr::pull(sci_name))

# remove Lontra canadensis from Hawaii?

# add native mammals from canary and azores checklists

# Azores (data from  F. Rigal)
azo_mam <- openxlsx::read.xlsx("data/raw-data/Azores_All_Mammals_Birds_names_ok.xlsx",
                               sheet = 2)
# only one native mammal Nyctalus azoreum
# Rattus norvegicus is not native from Azores => remove from native checklist

# Canary islands (data for official national checklist 2012 - JMFP)
can_m <- openxlsx::read.xlsx("data/raw-data/Canarias_Mammals_Birds.xlsx", sheet = "mammals")

mam_ckl |> dplyr::filter(sci_name %in% can_m$Species) |> 
  dplyr::distinct(sci_name) |> dplyr::pull(sci_name)
# ok all species ar in both databases

# mascarenes
# check with wikipedia page for mammals of mauritius + LR => all consistent

# galapagos
sort(mam_ckl |> dplyr::filter(ULM_ID %in% c(9883:9993, 85138)) |> 
       dplyr::distinct(sci_name) |> dplyr::pull(sci_name))
# most species listed in the wikipedia page are extinct...
# so data are consistent with our db

# tristan da cunha
# Check with The Fauna of the Tristan Da Cunha Islands (Holdgate, 1965)
# ok!

# final checklist
mam_ckl_clean <- mam_ckl |>
  dplyr::filter(!sci_name %in% c("Lontra canadensis","Rattus norvegicus"))

saveRDS(mam_ckl_clean, "data/derived-data/03_mammal_ckl_45_isl.rds")

length(unique(mam_ckl_clean$sci_name))
