# extract data for Land use change
# create a database combining the three LU metrics
# for all selected islands

rm(list=ls())

library(tidyverse)

path_data <- "Z:/THESE/5_Data/Distribution_spatiale/"


# Load island list and shapes
isl <- readRDS("data/derived-data/01_shp_45_major_isl.rds")


##### Human modif change 1990-2015 #####

# formula used for change: 
# change <- hm2015-hm1990
# done for the 2*9 original .tif files 

# open one file for defining CRS
change1 <- raster::raster(paste0(path_data, "Human_modification/change_btw_1990_2015/", 
                         "change_btw_1990_2015_1.tif"))

# transform island poly to raster crs
shp_proj <-  sf::st_transform(isl, crs = raster::crs(change1))
# save workspace
rm(change1)

# 9 raster files for covering the world
# list all files, and open each file until finding the one were the island is
# extract for each island => to see advances and saving output

change_files <- list.files(paste0(path_data, "Human_modification/change_btw_1990_2015/"))
change_files_ok <- change_files[!grepl(".aux",change_files)]

output_list <- as.list(isl$ID)
names(output_list) <- isl$ID

for(k in 1:length(output_list)){
  # subset for each island
  isl <- subset(shp_proj, ID == output_list[[k]])
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

change90_15 <- readRDS("data/derived-data/20_LU_hm_1990_2015.rds")

hist(unlist(lapply(change90_15, mean))/65536, n = 50)

hist(unlist(lapply(change90_15, median))/65536, n = 50)

change <- unlist(lapply(change90_15, median))/65536
plot(density(change))



##### Road density #####

isl <- readRDS("data/derived-data/01_shp_45_major_isl.rds")

# Road density using open street map
#install.packages("osmdata")

for(i in 1:nrow(isl)){
  # define bounding box of the island
  bbox <- sf::st_bbox(isl[i,])
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
    isl$road_length[i] = sum(sf::st_length(roads))
    } else {
      isl$road_length[i] = 0
    }
  print(i)
}

saveRDS(isl %>% sf::st_drop_geometry() %>% select(ID, road_length), 
        "data/derived-data/20_LU_road_length_OSM.rds")

osm_length <- readRDS("data/derived-data/20_LU_road_length_OSM.rds")

##### Combine all LU metrics in one df #####

# load data
change90_15 <- readRDS("data/derived-data/20_LU_hm_1990_2015.rds")
osm_length <- readRDS("data/derived-data/20_LU_road_length_OSM.rds")
isl <- readRDS("data/derived-data/01_shp_45_major_isl.rds")
isl_select <- isl %>% 
  dplyr::mutate(Area = sf::st_area(isl)) %>%
  dplyr::mutate(Area_km2 = units::drop_units(Area)/1000000)

# shape data to one value per island
# take median, max, mean, and sd for aggregated metrics


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

osm_dens <- left_join(osm_length, 
                      isl_select %>% sf::st_drop_geometry() %>% 
                        select(ID, Area_km2)) %>%
  mutate(rdens_osm = road_length/Area_km2) %>%
  mutate(ID = as.character(ID))


# bind all databases

LU_exposure <- left_join(change_df, osm_dens) 
saveRDS(LU_exposure, "data/derived-data/20_LU_exposure_45_isl.rds")

# check correlation between variables
LU_exposure <- readRDS("data/derived-data/20_LU_exposure_45_isl.rds") %>% units::drop_units()
str(LU_exposure)




corr = cor(LU_exposure %>% select(-ID))
p.mat <- ggcorrplot::cor_pmat(LU_exposure %>% select(-ID))

ggcorrplot::ggcorrplot(corr, p.mat = p.mat, hc.order = TRUE,
                       type = "lower", insig = "blank",
                       lab = TRUE)


# sep by archipelago?
# reprendre ici

lu <- left_join(isl, LU_exposure %>% mutate(ID = as.numeric(ID)))

ggplot(data = lu)+
  geom_boxplot(aes(x=Archip, y = rdens_osm, fill = Archip), alpha = .6) +
  geom_jitter(aes(x=Archip, y = rdens_osm, color = Archip), alpha = .6, size = 3) +
  theme_classic()

ggplot(data = lu)+
  geom_boxplot(aes(x=Archip, y = mean_HM_change, fill = Archip), alpha = .6) +
  geom_jitter(aes(x=Archip, y = mean_HM_change, color = Archip), alpha = .6, size = 3) +
  theme_classic()


ggplot(data = lu)+
  geom_point(aes(x=rdens_osm, y = mean_HM_change, color = Archip), alpha = .6,  size = 3) +
  theme_classic()

