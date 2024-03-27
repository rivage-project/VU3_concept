# extract data for Land_use
# create a database combining the three LU metrics
# for all selected islands

rm(list=ls())

library(tidyverse)

path_data <- "Z:/THESE/5_Data/Distribution_spatiale/"


# Load island list and shapes
gadm_islands <- sf::st_read(paste0(
  path_data, "Shpfiles_iles_continent/Islands_Weigelt_reparees.shp"))

isl_select <- read.csv("data/derived-data/01_selected_islands.csv")

shp_55 <- subset(gadm_islands, ULM_ID %in% isl_select$ID)


##### Human static change 2017 #####

# Source: Temporal mapping of global human modification from 1990 to 2017
# https://zenodo.org/record/3963013#.YsQkSITP1D8
# Associated paper: Theobald et al. 2020
# https://essd.copernicus.org/articles/12/1953/2020/


# open one file for defining CRS
hm2017 <- raster::raster(paste0(path_data, "Human_modification/gHMv1_300m_2017_static/",
                        "gHMv1_300m_2017_static-0000046592-0000093184.tif"))

# transform island poly to raster crs
raster::crs(hm2017)
shp_55_proj <-  sf::st_transform(shp_55, crs = raster::crs(hm2017))

# save workspace
rm(gadm_islands, shp_55, hm2017)

# 9 raster files for covering the world
# list all files, and open each file until finding the one were the island is
# extract for each island => to see advances and saving output

hm2017_files <- list.files(paste0(path_data, "Human_modification/gHMv1_300m_2017_static/"))

output_list <- as.list(isl_select$ID)
names(output_list) <- isl_select$ID

for(k in 1:length(output_list)){
  # subset for each island
  isl <- subset(shp_55_proj, ULM_ID == output_list[[k]])
  # set up while loop
  i = 1
  e <- NULL
  while (is.null(e)){
    # while no data extracted, open next raster
    hm2017_f <- raster::raster(paste0(path_data, "Human_modification/gHMv1_300m_2017_static/",
                              hm2017_files[i]))
    e <- unlist(raster::extract(hm2017_f, isl))
    i = i+1
  }
  # store extracted data in output list
  output_list[[k]] <- e
  saveRDS(output_list, "data/derived-data/20_LU_hm2017.rds")
  print(k)
}

# check the data 
hm2017_isl <- readRDS("data/derived-data/20_LU_hm2017.rds")
med_hm <- lapply(hm2017_isl, median)
med <- unlist(med_hm)/65536
hist(med)
# are islands from the list without human change?
sum_hm <- unlist(lapply(hm2017_isl, sum))
isl_zero_only <- names(which(sum_hm==0)) # nope, all good



##### Human modif change 1990-2015 #####

# formula used for change: 
# change <- hm2015-hm1990
# done for the 2*9 original .tif files 


# open one file for defining CRS
change1 <- raster::raster(paste0(path_data, "Human_modification/change_btw_1990_2015/", 
                         "change_btw_1990_2015_1.tif"))

# transform island poly to raster crs
shp_55_proj <-  sf::st_transform(shp_55, crs = raster::crs(change1))
# save workspace
rm(change1, shp_55)

# 9 raster files for covering the world
# list all files, and open each file until finding the one were the island is
# extract for each island => to see advances and saving output

change_files <- list.files(paste0(path_data, "Human_modification/change_btw_1990_2015/"))
change_files_ok <- change_files[!grepl(".aux",change_files)]

output_list <- as.list(isl_select$ID)
names(output_list) <- isl_select$ID

for(k in 1:length(output_list)){
  # subset for each island
  isl <- subset(shp_55_proj, ULM_ID == output_list[[k]])
  # set up while loop
  #i = 1
  e <- c()
  for (i in 1:length(change_files_ok)){
    # while no data extracted, open next raster
    change_f <- raster::raster(paste0(path_data, "Human_modification/change_btw_1990_2015/",
                              change_files_ok[i]))
    e <- c(e, unlist(raster::extract(change_f, isl)))
  }
  # store extracted data in output list
  output_list[[k]] <- e
  saveRDS(output_list, "data/derived-data/20_LU_hm_1990_2015.rds")
  print(k)
}

test2 <- unlist(lapply(output_list, length))
test == test2

change90_15 <- readRDS("data/derived-data/20_LU_hm_1990_2015.rds")

hist(unlist(lapply(change90_15, mean))/65536, n = 50)

hist(unlist(lapply(change90_15, median))/65536, n = 50)


# correlation between hm2017 and modification between 90 & 2015
static <- unlist(lapply(hm2017_isl, median))/65536
change <- unlist(lapply(change90_15, median))/65536

norm_stat <- static/max(static)

plot(static, change)

cor.test(static, change)

plot(density(static))
plot(density(change))
plot(density(norm_stat))



##### Road density #####

# Data from Global Roads Inventory Project - GRIP - version 4
# source: Meijer et al 2018 Environ. Res. Lett. 
# https://doi.org/10.1088/1748-9326/aabd42

rdens <- terra::rast(paste0(path_data, "GRIP_Roads/grip4_total_dens_m_km2.asc"))

# transform island poly to raster crs
shp_55_proj <-  sf::st_transform(shp_55, crs = raster::crs(rdens))

output_list <- as.list(isl_select$ID)
names(output_list) <- isl_select$ID

for(k in 1:length(output_list)){
  # subset for each island
  isl <- subset(shp_55_proj, ULM_ID == output_list[[k]])
  # extract data and store in output list
  output_list[[k]] <- terra::extract(rdens, isl, touches=T) %>%
    pull(grip4_total_dens_m_km2)
  # save output
  saveRDS(output_list, "data/derived-data/20_LU_road_density.rds")
  print(k)
}

dens_isl <- readRDS("data/derived-data/20_LU_road_density.rds")

unlist(lapply(dens_isl, length))

hist(unlist(lapply(dens_isl, mean)), n = 50)
hist(unlist(lapply(dens_isl, median)), n = 50)



glp <- shp_55 %>% filter(ARCHIP == "Galapagos Islands")
ggplot(glp)+geom_sf()
glp_dens <- terra::crop(rdens,glp)
terra::plot(glp_dens)
ggplot() +
  tidyterra::geom_spatraster(data = glp_dens, aes(fill = grip4_total_dens_m_km2))+
  geom_sf(data=glp, fill=NA, color="white")


hw <- shp_55 %>% filter(ARCHIP == "Azores") # Hawaii, Canary Islands, Azores
hw_dens <- terra::crop(rdens,hw)
ggplot() +
  tidyterra::geom_spatraster(data = hw_dens, aes(fill = grip4_total_dens_m_km2))+
  geom_sf(data=hw, fill=NA, color="white")

for(i in 1:length(dens_isl)){
  dens_isl[[i]]= data.frame(
    rdens = dens_isl[[i]],
    ID = names(dens_isl)[i])
}
dens_df <- bind_rows(dens_isl) %>%
  filter(!is.na(rdens)) %>%
  group_by(ID) %>%
  summarize(
    mean_dens = mean(rdens),
    med_dens = median(rdens),
    max_dens = max(rdens)
  )
hist(dens_df$mean_dens, n=10)
hist(dens_df$med_dens, n=10)

# Road density using open street map

#install.packages("osmdata")

for(i in 1:nrow(shp_55)){
  # define bounding box of the island
  bbox <- sf::st_bbox(shp_55[i,])
  # define query for osm
  query <- osmdata::opq(bbox) %>%
    osmdata::add_osm_feature(key = 'highway') %>%
    osmdata::osmdata_sf()
  
  # Extract the road data
  roads <- query$osm_lines
  # class(roads)
  # ggplot(roads)+geom_sf()
  
  # calculate total road length
  if(!is.null(roads)){
    shp_55$road_length[i] = sum(sf::st_length(roads))
    } else {
      shp_55$road_length[i] = 0
    }
  print(i)
}

saveRDS(shp_55 %>% sf::st_drop_geometry() %>% select(ULM_ID, road_length), 
        "data/derived-data/20_LU_road_length_OSM.rds")

osm_length <- readRDS("data/derived-data/20_LU_road_length_OSM.rds")


# check consistency with dens_df from GRIP

osm_dens <- left_join(osm_length %>% rename(ID = ULM_ID), 
                      isl_select %>% select(ID, Area, Archip)) %>%
  mutate(rdens_osm = road_length/Area)


osm_grip <- left_join((osm_dens %>% mutate(ID=as.character(ID))), dens_df)

cor.test(osm_grip$mean_dens, osm_grip$rdens_osm)

# using mean road density from GRIP
ggplot(osm_grip %>% mutate(mean_dens = if_else(is.na(mean_dens), 0, mean_dens))) +
  geom_point(aes(rdens_osm, mean_dens, color=Archip), size = 2)+
  geom_abline(slope = 1, intercept = 0)+
  ylab("Mean road density from GRIP") + xlab("Road density from OSM")+
  theme_minimal()

# using median road density from GRIP
ggplot(osm_grip %>% mutate(med_dens = if_else(is.na(med_dens), 0, med_dens))) +
  geom_point(aes(rdens_osm, med_dens, color=Archip))+
  geom_abline(slope = 1, intercept = 0)



##### Combine all LU metrics in one df #####

# load data
hm2017_isl <- readRDS("data/derived-data/20_LU_hm2017.rds")
change90_15 <- readRDS("data/derived-data/20_LU_hm_1990_2015.rds")
osm_length <- readRDS("data/derived-data/20_LU_road_length_OSM.rds")
isl_select <- read.csv("data/derived-data/01_selected_islands.csv")


# shape data to one value per island
# take median, max, mean, and sd for aggregated metrics

# HM 2017 static

for(i in 1:length(hm2017_isl)){
  hm2017_isl[[i]]= data.frame(
    HM_static_2017 = hm2017_isl[[i]],
    ID = names(hm2017_isl)[i])
}
hm2017_df <- bind_rows(hm2017_isl) %>%
  group_by(ID) %>%
  summarize(
    mean_HM_static_2017 = mean(HM_static_2017/65536),
    med_HM_static_2017 = median(HM_static_2017/65536),
    sd_HM_static_2017 = sd(HM_static_2017/65536),
    max_HM_static_2017 = max(HM_static_2017/65536)
  )

hist(hm2017_df$mean_HM_static_2017)
hist(hm2017_df$med_HM_static_2017)
hist(hm2017_df$max_HM_static_2017, n= 10)

# HM Change 1990-2015
for(i in 1:length(change90_15)){
  change90_15[[i]]= data.frame(
    HM_change = change90_15[[i]],
    ID = names(change90_15)[i])
}
change_df <- bind_rows(change90_15) %>%
  group_by(ID) %>%
  summarize(
    mean_HM_change = mean(HM_change/65536),
    med_HM_change = median(HM_change/65536),
    sd_HM_change = sd(HM_change/65536),
    max_HM_change = max(HM_change/65536)
  )

hist(change_df$mean_HM_change)
hist(change_df$med_HM_change)
hist(change_df$max_HM_change, n= 10)


# Road density

osm_dens <- left_join(osm_length %>% rename(ID = ULM_ID), 
                      isl_select %>% select(ID, Area)) %>%
  mutate(rdens_osm = road_length/Area) %>%
  mutate(ID = as.character(ID))


# bind all databases

LU_exposure <- left_join(left_join(hm2017_df, change_df), osm_dens )
saveRDS(LU_exposure, "data/derived-data/20_LU_exposure_55_isl.rds")

# check correlation between variables

corr = cor(LU_exposure %>% select(-ID))
p.mat <- ggcorrplot::cor_pmat(LU_exposure %>% select(-ID))

ggcorrplot::ggcorrplot(corr, p.mat = p.mat, hc.order = TRUE,
                       type = "lower", insig = "blank")


corr = cor(LU_exposure %>% select(-ID))
p.mat <- ggcorrplot::cor_pmat(LU_exposure %>% select(-ID))

ggcorrplot::ggcorrplot(corr, p.mat = p.mat, hc.order = TRUE,
                       type = "lower", insig = "blank",
                       lab = TRUE)


# sep by archipelago?
LU_exposure <- left_join(isl_select %>% select(ID, Archip) %>% mutate(ID= as.character(ID)),
                         LU_exposure)

colnames(LU_exposure)

ggplot(data = LU_exposure, aes(x=mean_HM_static_2017, y = mean_HM_change, color = Archip))+
  geom_point()+
  geom_pointrange(aes(ymin = mean_HM_change-sd_HM_change, 
                      ymax = mean_HM_change+sd_HM_change), 
                  size = .8, alpha = .6) +
  geom_pointrange(aes(xmin = mean_HM_static_2017-sd_HM_static_2017, 
                      xmax = mean_HM_static_2017+sd_HM_static_2017), 
                  size = .8, alpha = .6) +
  theme_classic()


ggplot(data = LU_exposure)+
  geom_boxplot(aes(x=Archip, y = rdens_osm, fill = Archip), alpha = .6) +
  geom_jitter(aes(x=Archip, y = rdens_osm, color = Archip), alpha = .6, size = 3) +
  theme_classic()


