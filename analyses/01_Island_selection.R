# load islands from weigelt
# select islands from 
# Hawaii, Canarias, Mascarenes, Galapagos, Azores, Tristan da Cunha

library(tidyverse)

tbl <- read.csv("Z:/THESE/5_Data/Distribution_spatiale/Weigelt_isl_database/Weigelt_etal_2013_PNAS_islanddata.csv")

# all archipel
sort(unique(tbl$Archip))

tbl[grepl("Rodrigues",tbl$Island),]

# filter the archipelagos
isl6 <- tbl %>%
  filter(Archip %in% c("Mascarene Islands", "Hawaii",
                       "Galapagos Islands", "Azores", 
                       "Canary Islands", "Rodrigues", 
                       "Tristan da Cunha Islands")) %>%
  mutate(Island_name = gsub("\x91", "", Island)) %>%
  mutate(Archip = if_else(Archip=="Rodrigues","Mascarene Islands",Archip)) %>%
  # filter(!is.na(Island)) %>%
  select(ID:Elev, Island_name)# %>%
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

isl6_major <- isl6 %>%
  dplyr::filter(!is.na(Island_name)) %>%
  dplyr::filter(!Island_name %in% minor_islands) %>%
  dplyr::mutate(Island=Island_name) %>%
  dplyr::select(ID:Island)

table(isl6_major$Archip)

write.csv(isl6_major, "data/derived-data/01_selected_islands.csv")

openxlsx::write.xlsx(isl6_major, 
  "data/derived-data/01_selected_islands.xlsx", )


#### Get selected island shapes #####

shp <- sf::st_read("Z:/THESE/6_Projects/zDone/biogeo_FD_alien/Data/Islands_Weigelt_reparees.shp")
colnames(shp)

shp <- shp %>% filter(ULM_ID %in% isl6_major$ID) %>%
  mutate(ARCHIP = if_else(ARCHIP=="Rodrigues","Mascarene Islands",ARCHIP)) %>%
  select(ULM_ID)


ggplot(shp)+
  geom_sf(aes(fill=ARCHIP, color = ARCHIP))


h <- ggplot(shp %>% filter(ARCHIP == "Hawaii"))+
  geom_sf() + theme_light() + ggtitle("Hawai‘i")
h

g <- ggplot(shp %>% filter(ARCHIP == "Galapagos Islands"))+
  geom_sf() + theme_light() + ggtitle("Galapagos Islands")

a <- ggplot(shp %>% filter(ARCHIP == "Azores"))+
  geom_sf() + theme_light() + ggtitle("Azores")

c <- ggplot(shp %>% filter(ARCHIP == "Canary Islands"))+
  geom_sf() + theme_light() + ggtitle("Canary Islands")

m <- ggplot(shp %>% filter(ARCHIP == "Mascarene Islands"))+
  geom_sf() + theme_light() + ggtitle("Mascarene Islands")+
  ggspatial::annotation_scale(pad_x = unit(10, "cm"))

t <- ggplot(shp %>% filter(ARCHIP == "Tristan da Cunha Islands"))+
  geom_sf() + theme_light() + ggtitle("Tristan da Cunha Islands") #+
  #geom_text(aes(x=LONG, y = LAT, label = ULM_ID), color = "black")
t

ggpubr::ggarrange(h,g,a,c,m,t, ncol=2, nrow=3)

shp <- left_join(shp %>% rename(ID = ULM_ID), isl6_major)

saveRDS(shp, "data/derived-data/01_shp_45_major_isl.rds")
