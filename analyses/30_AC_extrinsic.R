# Extrinsic AC 

rm(list=ls())

library(tidyverse)

# Load island list and shapes
path_data <- "Z:/THESE/5_Data/Distribution_spatiale/"
gadm_islands <- sf::st_read(paste0(
  path_data, "Shpfiles_iles_continent/Islands_Weigelt_reparees.shp"))

isl <- readRDS("data/derived-data/11_isl_with_gift_data.rds") %>% 
  filter(!is.na(isl_name_gift))
isl_select <- read.csv("data/derived-data/01_selected_islands.csv")
isl2 <- left_join(isl_select, isl) %>% rename(ULM_ID = ID)

shp_44 <- gadm_islands %>% 
  filter(ULM_ID %in% (isl_select %>% 
                        filter(Island_name %in% isl$Island_name) %>% 
                        pull(ID))) %>%
  mutate(ARCHIP = if_else(ARCHIP=="Rodrigues","Mascarene Islands", ARCHIP))

unique(shp_44$ARCHIP)

###### 1. Island topography #####


# Elevation, TRI, TPI
zfiles <- list.files("data/raw-data/elevation")[grepl(".zip", list.files("data/raw-data/elevation"))]
dat <- data.frame()
for(i in zfiles){
  i="N00W090.zip"
  
  # read elevation file
  elev <- terra::rast(unzip(paste0("data/raw-data/elevation/", i)))
  #calculate tri and tpi
  tpi_tri <- terra::terrain(elev, v=c("TRI", "TPI"))
  # bind all layers in 1 raster
  all <- c(elev, tpi_tri)
  names(all) <- c("elev","TRI","TPI")
 
  zdat <- terra::extract(all, shp_44) %>%
    filter(!(is.na(elev) & is.na(TRI) & is.na(TPI)))

  dat <- bind_rows(dat, zdat)
  print(i)
}

head(dat)
length(unique(dat$ID))
unique(dat$ID)

dat_isl <- dat %>%
  group_by(ID) %>%
  summarize(
    mean_elev = mean(elev, na.rm = T),
    sd_elev = sd(elev, na.rm = T),
    max_elev = max(elev, na.rm = T),
    mean_tpi = mean(TPI, na.rm = T),
    sd_tpi = sd(TPI, na.rm = T),
    mean_tri = mean(TRI, na.rm = T),
    sd_tri = sd(TRI, na.rm = T)
  )

dat_isl$ULM_ID <- shp_44$ULM_ID[dat_isl$ID]

saveRDS(dat_isl, "data/derived-data/30_extrinsic_AC_elevation.rds")


###### 2. Island conservation potential #####

# Load PAs from WDPA database
# Following Leclerc et al. 2020, select only non-marine PA
# only PA that have strict protection measures (IUCN I to IV)

path_pa <- "data/raw-data/WDPA/"

shp0 <- sf::st_read(paste0(path_pa,
                       "WDPA_May2024_Public_shp_0/WDPA_May2024_Public_shp-polygons.shp"))
shp1 <- sf::st_read(paste0(path_pa,
                       "WDPA_May2024_Public_shp_1/WDPA_May2024_Public_shp-polygons.shp"))
shp2 <- sf::st_read(paste0(path_pa,
                       "WDPA_May2024_Public_shp_2/WDPA_May2024_Public_shp-polygons.shp"))

pts0 <- sf::st_read(
  paste0(path_pa, "WDPA_May2024_Public_shp_0/WDPA_May2024_Public_shp-points.shp"))
pts1 <- sf::st_read(
  paste0(path_pa, "WDPA_May2024_Public_shp_1/WDPA_May2024_Public_shp-points.shp"))
pts2 <- sf::st_read(
  paste0(path_pa, "WDPA_May2024_Public_shp_2/WDPA_May2024_Public_shp-points.shp"))

shp <- bind_rows(
  shp0 %>% filter(IUCN_CAT %in% c("Ia","Ib","II","III","IV")), 
  shp1 %>% filter(IUCN_CAT %in% c("Ia","Ib","II","III","IV")), 
  shp2 %>% filter(IUCN_CAT %in% c("Ia","Ib","II","III","IV")))

rm(shp1, shp2, shp0)

pts <- bind_rows(
  pts0 %>% filter(IUCN_CAT %in% c("Ia","Ib","II","III","IV")), 
  pts1 %>% filter(IUCN_CAT %in% c("Ia","Ib","II","III","IV")), 
  pts2 %>% filter(IUCN_CAT %in% c("Ia","Ib","II","III","IV")))
rm(pts1, pts2, pts0)


# remove marine PA
shpt <- shp %>% filter(MARINE!="2")
ptst <- pts %>% filter(MARINE!="2")

sf::st_crs(shpt)==sf::st_crs(shp_44)
sum(sf::st_is_valid(shpt))

shpt <- sf::st_make_valid(shpt)

isl_pa <- sf::st_intersection(shp_44, shpt)


