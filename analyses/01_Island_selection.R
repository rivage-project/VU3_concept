# load islands from weigelt
# select islands from 
# Hawaii, Canarias, Mascarenes, Galapagos, Azores, Tristan da Cunha


tbl <- read.csv("Z:/THESE/5_Data/Distribution_spatiale/Weigelt_isl_database/Weigelt_etal_2013_PNAS_islanddata.csv")

# all archipel
sort(unique(tbl$Archip))

tbl[grepl("Rodrigues",tbl$Island),]

# filter the archipelagos
isl6 <- tbl |>
  dplyr::filter(Archip %in% c("Mascarene Islands", "Hawaii",
                       "Galapagos Islands", "Azores", 
                       "Canary Islands", "Rodrigues", 
                       "Tristan da Cunha Islands")) |>
  dplyr::mutate(Island_name = gsub("\x91", "", Island)) |>
  dplyr::mutate(Archip = dplyr::if_else(Archip=="Rodrigues","Mascarene Islands",Archip)) |>
  # filter(!is.na(Island)) |>
  dplyr::select(ID:Elev, Island_name)# |>
  # select(ID:Archip, Island_name)


isl6$Island_name
table(isl6$Archip)

# Name correctly TdC islands
isl6$Island_name[isl6$ID==7731] <- "Inaccessible Island"
isl6$Island_name[isl6$ID==7725] <- "Nightingale Island"

# Remove minor islands

minor_islands <- c(
  "Isla Graciosa","Isla de Alegranza","Lobos","Isla de Montana Clara", # Canary
  "Lisianski Island","Sand Island","Lehua","Laysan Island", "Ford Island", # Hawaii
  "Ile d' Ambre", # Mascarene
  "Isla Bartolome", "Isla Seymour", "Isla Darwin"# Galapagos
)

isl6_major <- isl6 |>
  dplyr::filter(!is.na(Island_name)) |>
  dplyr::filter(!Island_name %in% minor_islands) |>
  dplyr::mutate(Island=Island_name) |>
  dplyr::select(ID:Island)

table(isl6_major$Archip)


correct_names <- openxlsx::read.xlsx(
  "data/derived-data/01_selected_islands_clean JM corr.xlsx")


isl6_major_ok <- dplyr::left_join(
  isl6_major,
  correct_names |> dplyr::select(ID, Correct.island.name)) |>
  dplyr::select(-Island)|>
  dplyr::rename(Island = Correct.island.name)

write.csv(isl6_major, "data/derived-data/01_selected_islands.csv")

openxlsx::write.xlsx(isl6_major, 
                     "data/derived-data/01_selected_islands.xlsx")
  
#### Get selected island shapes #####

shp <- sf::st_read("Z:/THESE/6_Projects/zDone/biogeo_FD_alien/Data/Islands_Weigelt_reparees.shp")
colnames(shp)

shp <- shp |> 
  dplyr::filter(ULM_ID %in% isl6_major$ID) |>
  dplyr::mutate(ARCHIP = dplyr::if_else(ARCHIP=="Rodrigues","Mascarene Islands",ARCHIP)) |>
  dplyr::select(ULM_ID, ARCHIP)


ggplot(shp)+
  geom_sf(aes(fill=ARCHIP, color = ARCHIP))


h <- ggplot(shp |> dplyr::filter(ARCHIP == "Hawaii"))+
  geom_sf() + theme_light() + ggtitle("Hawai‘i")
h

g <- ggplot(shp |> dplyr::filter(ARCHIP == "Galapagos Islands"))+
  geom_sf() + theme_light() + ggtitle("Galapagos Islands")

a <- ggplot(shp |> dplyr::filter(ARCHIP == "Azores"))+
  geom_sf() + theme_light() + ggtitle("Azores")

c <- ggplot(shp |> dplyr::filter(ARCHIP == "Canary Islands"))+
  geom_sf() + theme_light() + ggtitle("Canary Islands")

m <- ggplot(shp |> dplyr::filter(ARCHIP == "Mascarene Islands"))+
  geom_sf() + theme_light() + ggtitle("Mascarene Islands")+
  ggspatial::annotation_scale(pad_x = unit(10, "cm"))

t <- ggplot(shp |> dplyr::filter(ARCHIP == "Tristan da Cunha Islands"))+
  geom_sf() + theme_light() + ggtitle("Tristan da Cunha Islands") #+
  #geom_text(aes(x=LONG, y = LAT, label = ULM_ID), color = "black")
t

ggpubr::ggarrange(h,g,a,c,m,t, ncol=2, nrow=3)

shp <- dplyr::left_join(shp |> dplyr::rename(ID = ULM_ID), isl6_major_ok)

saveRDS(shp, "data/derived-data/01_shp_45_major_isl_clean.rds")
