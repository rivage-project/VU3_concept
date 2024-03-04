# load islands from weigelt
# select islands from Hawaii, Canarias, Mascarene islands, Galapagos, Azores

library(tidyverse)

tbl <- read.csv("Z:/THESE/5_Data/Distribution_spatiale/Weigelt_isl_database/Weigelt_etal_2013_PNAS_islanddata.csv")

# all archipel
sort(unique(tbl$Archip))

tbl[grepl("Rodrigues",tbl$Island),]
# filter the archipelagos
isl5 <- tbl %>%
  filter(Archip %in% c("Mascarene Islands", "Hawaii",
                       "Galapagos Islands", "Azores", 
                       "Canary Islands", "Rodrigues")) %>%
  mutate(Island_name = gsub("\x91", "", Island)) %>%
  mutate(Archip = if_else(Archip=="Rodrigues","Mascarene Islands",Archip)) %>%
  filter(!is.na(Island)) %>%
  select(ID:Elev, Island_name)# %>%
  #select(ID:Archip, Island_name)


isl5$Island_name
table(isl5$Archip)

isl5 

write.csv(isl5, "data/derived-data/01_selected_islands.csv")

openxlsx::write.xlsx(
  isl5 %>% mutate(Island=Island_name) %>% select(-Island_name), 
  "data/derived-data/01_selected_islands.xlsx", )




shp <- sf::st_read("Z:/THESE/6_Projects/biogeo_FD_alien/Data/Islands_Weigelt_reparees.shp")
colnames(shp)
shp <- shp %>% filter(ULM_ID %in% isl5$ID) %>%
  mutate(ARCHIP = if_else(ARCHIP=="Rodrigues","Mascarene Islands",ARCHIP))

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

m
ggpubr::ggarrange(h,g,a,c,m, ncol=2, nrow=3)
