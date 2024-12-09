# Abiotic AC 

rm(list=ls())


# Load island list and shapes
isl <- readRDS("data/derived-data/01_shp_45_major_isl.rds")

###### 1. Island topography #####

# Elevation, TRI, TPI
# https://srtm.csi.cgiar.org/srtmdata/ for raw data

zfiles <- list.files("data/raw-data/elevation")[grepl(".zip", list.files("data/raw-data/elevation"))]
dat <- data.frame()
for(i in zfiles){
  i="S60W030.zip"
  
  # read elevation file
  elev <- terra::rast(unzip(paste0("data/raw-data/elevation/", i)))
  #calculate tri and tpi
  tpi_tri <- terra::terrain(elev, v=c("TRI", "TPI"))
  # bind all layers in 1 raster
  all <- c(elev, tpi_tri)
  names(all) <- c("elev","TRI","TPI")
 
  zdat <- terra::extract(all, isl) |>
    dplyr::filter(!(is.na(elev) & is.na(TRI) & is.na(TPI)))

  dat <- dplyr::bind_rows(dat, zdat)
  print(i)
}

head(dat)
length(unique(dat$ID))
unique(dat$ID)

dat_isl <- dat |>
  dplyr::group_by(ID) |>
  dplyr::summarize(
    mean_elev = mean(elev, na.rm = T),
    sd_elev = sd(elev, na.rm = T),
    max_elev = max(elev, na.rm = T),
    mean_tpi = mean(TPI, na.rm = T),
    sd_tpi = sd(TPI, na.rm = T),
    mean_tri = mean(TRI, na.rm = T),
    sd_tri = sd(TRI, na.rm = T)
  )

dat_isl$ULM_ID <- isl$ID[dat_isl$ID]

saveRDS(dat_isl |> dplyr::select(-ID) |> dplyr::rename (ID = ULM_ID), 
        "data/derived-data/10_abiotic_AC_elevation.rds")

#### Exploration topography
dat_isl <- readRDS("data/derived-data/10_abiotic_AC_elevation.rds")
tp <- dplyr::left_join(isl, dat_isl)

library(ggplot2)
ggplot(tp)+
  geom_point(aes(x=mean_tpi, y=mean_tri, color = Archip),
             size = 3, alpha =.5)
ggplot(tp)+
  geom_point(aes(x=sd_tpi, y=sd_tri, color = Archip),
             size = 3, alpha =.5)
  
ggplot(tp)+
  geom_point(aes(x=mean_elev, y=max_elev, color = Archip),
             size = 3, alpha =.5)

ggplot(tp)+
  geom_point(aes(x=mean_elev, y=mean_tpi, color = Archip))
ggplot(tp)+
  geom_point(aes(x=mean_elev, y=mean_tri, color = Archip),
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
  shp <- shpi |> 
    dplyr::filter(IUCN_CAT %in% c("Ia","Ib","II","III","IV")) |>
    dplyr::filter(MARINE!="2")
  
  shpv <- sf::st_make_valid(shp)
  # sum(sf::st_is_valid(shpv))
  # select only valid geometries (check the invalid ones after if they cross islands)
  validshp <- shpv[sf::st_is_valid(shpv),]
  
  # call intersect function before (faster than intersection)
  inter <- sf::st_intersects(isl, validshp)
  # extract all lines from PA shp which intersects with selected isl
  test <- unique(unlist(inter))
  # call intersection to have the 
  isl_pa <- sf::st_intersection(isl, validshp[test,])
  
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

pts <- dplyr::bind_rows(pts0, pts1, pts2) |> 
  dplyr::filter(IUCN_CAT %in% c("Ia","Ib","II","III","IV")) |> 
  dplyr::filter(MARINE!="2")

inter_pts <- sf::st_intersects(isl, pts)
length(unlist(inter_pts))
# 1 island has a point => Grand Port fishing reserve, not terrestrial
# no need to consider


# bind all shapefiles together 
isl_pa_df <- dplyr::bind_rows(all_isl_pa)
d = data.frame()
for(i in unique(isl_pa_df$ID)){
  a = isl_pa_df |> dplyr::filter(ID == i) |> sf::st_make_valid()
  b = sf::st_union(a) |> sf::st_area()
  c = data.frame(pa_area = b, 
                 ID = i)
  d <- dplyr::bind_rows(d, c)
}

prop_pa <- dplyr::left_join(
  d, 
  isl |> dplyr::select(ID, Island, Archip) |> 
    dplyr::mutate(area = sf::st_area(geometry))) |>
  dplyr::mutate(PA_prop = round(pa_area/area, 4))

# add Inaccessible island
# Le secteur maritime et terrestre de l'île est une réserve naturelle protégée 
# (Inaccessible Island Nature Reserve) depuis 1997, inscrite sur la liste du patrimoine
# mondial par l'UNESCO en tant qu'îles Gough et Inaccessible, en raison notamment de 
# la présence habituelle de nombreux cétacés. Elle fait partie en outre de l'aire 
# protégée plus large du Tristan da Cunha Cetacean Sanctuary. 

ina <- prop_pa[1,] |>
  dplyr::mutate(ID = 7731, 
         Island = "Inaccessible Island", 
         Archip = "Tristan da Cunha Islands",
         pa_area = units::set_units(12650000, m^2),
         area = units::set_units(12650000, m^2)) |>
  dplyr::mutate(PA_prop = pa_area/area)

prop_pa_all <- dplyr::bind_rows(prop_pa, ina) |> sf::st_as_sf() |> sf::st_drop_geometry()

saveRDS(prop_pa_all, "data/derived-data/10_abiotic_AC_prop_PA.rds")

#### Exploration pa coverage
prop_pa <- readRDS("data/derived-data/10_abiotic_AC_prop_PA.rds")
pa <- dplyr::left_join(isl, prop_pa)
pa$PA_prop[is.na(pa$PA_prop)] <- 0
library(units)
ggplot(pa)+
  geom_boxplot(aes(x=Archip, y = PA_prop), outlier.shape = NA)+
  geom_point(aes(x=Archip, y = PA_prop, color = Archip, size = as.numeric(area)), 
             position ="jitter", size = 3, alpha =.5)


##### Select variables for AC abiotic #####
#   - island area
#   - max elevation
#   - mean TRI
#   - proportion PA

isl <- readRDS("data/derived-data/01_shp_45_major_isl.rds")
prop_pa <- readRDS("data/derived-data/10_abiotic_AC_prop_PA.rds")
elev_tri <- readRDS("data/derived-data/10_abiotic_AC_elevation.rds")

a <- dplyr::left_join(elev_tri, prop_pa)

ac <- dplyr::left_join(isl, dplyr::left_join(elev_tri, prop_pa |> dplyr::select(ID, PA_prop))) |>
  dplyr::mutate(PA_prop = ifelse(is.na(PA_prop), 0, PA_prop)) |> 
  dplyr::mutate(Area = round(units::set_units(sf::st_area(geometry), km^2), 2)) |>
  units::drop_units() |>
  sf::st_as_sf() |> sf::st_drop_geometry() |>
  dplyr::select(ID, Archip, Island, Area, max_elev, mean_tri, PA_prop)
  


saveRDS(ac, "data/derived-data/10_abiotic_AC_45_isl.rds")

corr = cor(ac |> dplyr::select(-c(ID, Archip, Island)))
p.mat <- ggcorrplot::cor_pmat(ac |> dplyr::select(-c(ID, Archip, Island)))

ggcorrplot::ggcorrplot(corr, p.mat = p.mat, hc.order = TRUE,
                       type = "lower", insig = "blank", lab = T)

ggplot(ac, aes(x=Area, y = max_elev)) +
  geom_point(aes(color = Archip))
