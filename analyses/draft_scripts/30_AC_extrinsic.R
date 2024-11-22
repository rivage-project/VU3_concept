# Extrinsic AC 

rm(list=ls())

library(tidyverse)

# Load island list and shapes
path_data <- "Z:/THESE/5_Data/Distribution_spatiale/"
gadm_islands <- sf::st_read(paste0(
  path_data, "Shpfiles_iles_continent/Islands_Weigelt_reparees.shp"))

isl_select <- read.csv("data/derived-data/01_selected_islands.csv")

shp_55 <- gadm_islands %>% 
  filter(ULM_ID %in% isl_select$ID) %>%
  mutate(ARCHIP = if_else(ARCHIP=="Rodrigues","Mascarene Islands", ARCHIP))

unique(shp_55$ARCHIP)

###### 1. Island topography #####


# Elevation, TRI, TPI
zfiles <- list.files("data/raw-data/elevation")[grepl(".zip", list.files("data/raw-data/elevation"))]
dat <- data.frame()
for(i in zfiles){
  #i="N00W090.zip"
  
  # read elevation file
  elev <- terra::rast(unzip(paste0("data/raw-data/elevation/", i)))
  #calculate tri and tpi
  tpi_tri <- terra::terrain(elev, v=c("TRI", "TPI"))
  # bind all layers in 1 raster
  all <- c(elev, tpi_tri)
  names(all) <- c("elev","TRI","TPI")
 
  zdat <- terra::extract(all, shp_55) %>%
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

dat_isl$ULM_ID <- shp_55$ULM_ID[dat_isl$ID]

saveRDS(dat_isl, "data/derived-data/30_extrinsic_AC_elevation.rds")

#### Exploration topography
dat_isl <- readRDS("data/derived-data/30_extrinsic_AC_elevation.rds")
tp <- left_join(shp_55, dat_isl)
ggplot(tp)+
  geom_point(aes(x=mean_tpi, y=mean_tri, color = ARCHIP),
             size = 3, alpha =.5)
ggplot(tp)+
  geom_point(aes(x=sd_tpi, y=sd_tri, color = ARCHIP),
             size = 3, alpha =.5)
  
ggplot(tp)+
  geom_point(aes(x=mean_elev, y=max_elev, color = ARCHIP),
             size = 3, alpha =.5)

ggplot(tp)+
  geom_point(aes(x=mean_elev, y=mean_tpi, color = ARCHIP))
ggplot(tp)+
  geom_point(aes(x=mean_elev, y=mean_tri, color = ARCHIP),
             size = 3, alpha =.5)
cor.test(tp$mean_elev, tp$mean_tri)

###### 2. Island conservation potential #####

# Load PAs from WDPA database
# Following Leclerc et al. 2020, select only non-marine PA
# only PA that have strict protection measures (IUCN I to IV)
# and only terrestrial PAs

path_pa <- "data/raw-data/WDPA/"
all_isl_pa <- list()
for(i in 0:2){ # takes ~25min
  shpi <- sf::st_read(paste0(path_pa, "WDPA_May2024_Public_shp_", 
                             i, "/WDPA_May2024_Public_shp-polygons.shp"))
  shp <- shpi %>% 
    filter(IUCN_CAT %in% c("Ia","Ib","II","III","IV")) %>%
    filter(MARINE!="2")
  
  shpv <- sf::st_make_valid(shp)
  # sum(sf::st_is_valid(shpv))
  # select only valid geometries (check the invalid ones after if they cross islands)
  validshp <- shpv[sf::st_is_valid(shpv),]
  
  # call intersect function before (faster than intersection)
  inter <- sf::st_intersects(shp_55, validshp)
  # extract all lines from PA shp which intersects with selected isl
  test <- unique(unlist(inter))
  # call intersection to have the 
  isl_pa <- sf::st_intersection(shp_55, validshp[test,])
  
  all_isl_pa[[i+1]] <- isl_pa
  print(i)
}


# see if any point data intersect with our isl?
pts0 <- sf::st_read(
  paste0(path_pa, "WDPA_May2024_Public_shp_0/WDPA_May2024_Public_shp-points.shp"))
pts1 <- sf::st_read(
  paste0(path_pa, "WDPA_May2024_Public_shp_1/WDPA_May2024_Public_shp-points.shp"))
pts2 <- sf::st_read(
  paste0(path_pa, "WDPA_May2024_Public_shp_2/WDPA_May2024_Public_shp-points.shp"))

pts <- bind_rows(pts0, pts1, pts2) %>% 
  filter(IUCN_CAT %in% c("Ia","Ib","II","III","IV")) %>% 
  filter(MARINE!="2")

inter_pts <- sf::st_intersects(shp_55, validshp[pts,])
length(unlist(inter_pts))
# 0 island has any point from WDPA data 



# bind all shapefiles together 
isl_pa_df <- bind_rows(all_isl_pa)
d = data.frame()
for(i in unique(isl_pa_df$ULM_ID)){
  a = isl_pa_df %>% filter(ULM_ID == i) %>% sf::st_make_valid()
  b = sf::st_union(a) %>% sf::st_area()
  c = data.frame(pa_area = b, 
                 ULM_ID = i)
  d <- bind_rows(d, c)
}

prop_pa <- left_join(d, shp_55 %>% select(ULM_ID, ISLAND, ARCHIP) %>% mutate(area = sf::st_area(geometry))) %>%
  mutate(PA_prop = round(pa_area/area, 4))

saveRDS(prop_pa, "data/derived-data/30_extrinsic_AC_prop_PA.rds")

#### Exploration pa coverage
prop_pa <- readRDS("data/derived-data/30_extrinsic_AC_prop_PA.rds")
pa <- left_join(shp_55, prop_pa)
library(units)
ggplot(pa)+
  geom_boxplot(aes(x=ARCHIP, y = PA_prop), outlier.shape = NA)+
  geom_point(aes(x=ARCHIP, y = PA_prop, color = ARCHIP, size = as.numeric(area)), 
             position ="jitter", size = 3, alpha =.5)
