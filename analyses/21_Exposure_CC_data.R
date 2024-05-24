# extract data for Climate change
# create a database combining the three CC metrics
# for all selected islands

rm(list=ls())

library(tidyverse)

# Load island list and shapes
path_data <- "Z:/THESE/5_Data/Distribution_spatiale/"
gadm_islands <- sf::st_read(paste0(
  path_data, "Shpfiles_iles_continent/Islands_Weigelt_reparees.shp"))

isl_select <- read.csv("data/derived-data/01_selected_islands.csv")

shp_55 <- subset(gadm_islands, ULM_ID %in% isl_select$ID)

##### Download data from CHELSA ####

# set the period to dwld
past_period <- c("1979", "1980", "1981", "1982", "1983")
present_period <- c("2015","2016","2017","2018", "2019")

# take all month for each year
month <- sprintf("%02d", 1:12)
year <- c(past_period, present_period)

# select the var to dwld
var <- "pr" # tas, tasmax, pr 
# ! for tas & tasmax, files start at 02_1989 (no january 89) => add 01 1984
# ! for pr, no files after 07 2019 => add file from 07 2014 to 01 2015
for(i in month){
  print(i)
  for(j in year){
    print(j)
    # download monthly timeseries 
    downloader::download(
      paste0("https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/monthly/", 
            var, "/CHELSA_", var, "_", i, "_", j, "_V.2.1.tif"), 
      paste0(here::here(), "/data/raw-data/climate/CHELSA_monthly_",
             var, "_", i, "_", j, "_V.2.1.tif"),
      mode = "wb")
  }
}

# must be 120 files per variable
# 360 files in total
length(list.files(paste0(here::here(), "/data/raw-data/climate/")))



####### Extract the data for the focal islands ########

# Function for extracting data,
# for each variable and present or past period
# input: 
# - the variable to extract (in tas, tasmax or pr)
# - the time period (past or present)
# - the island shapefile to extract data for
# output:
# a data-frame containing all the values for the 55 islands
# for each time period and climate variable

extract_val <- function(var, time, shp){
  # values for testing
  var = "pr"
  time = "present"
  # shp=shp_55
  
  if (!time %in% c("present","past")) stop("the time must be 'present' or 'past'")
  if (!var %in% c("tas","tasmax", "pr")) stop("var must be 'tas', 'tasmax', or 'pr'")
  
  # set the ouputs
  output <- data.frame()
  # make sure the variable is between underscores 
  varok <- paste0("_", var, "_")
  # list all files in directory
  files <- list.files(paste0(here::here(), "/data/raw-data/climate/"))
  # select only files related to the variable
  files_var <- files[grep(varok, files)]
  # and from the good time period
  if(time == "past"){
    period = paste(as.character(1979:1984), collapse='|')
    } else {
    period = paste(as.character(2014:2019), collapse='|')
      }
  files_var_time <- files_var[grep(period, files_var)]
  
  for (file in files_var_time){
    # test value
    #file = files_var_time[8]
    r <- terra::rast(paste0(here::here(), "/data/raw-data/climate/", file))
    sf::st_crs(shp) <- terra::crs(r)
    ext <- terra::extract(r, shp)
    colnames(ext) <- c('ID', var)
    ext$period <- time
    ext$file <- file
    output <- bind_rows(output, ext)
  }
  return(output)
}

tas_pres <- extract_val("tas", "present", shp_55)
tas_past <- extract_val("tas", "past", shp_55)
tasmax_pres <- extract_val("tasmax", "present", shp_55)
tasmax_past <- extract_val("tasmax", "past", shp_55)
pr_pres <- extract_val("pr", "present", shp_55)
pr_past <- extract_val("pr", "past", shp_55)

# save outputs for continuing analyses
saveRDS(tas_pres, paste0(here::here(), "/data/derived-data/21_tas_pres_55_isl.rds"))
saveRDS(tas_past, paste0(here::here(), "/data/derived-data/21_tas_past_55_isl.rds"))
saveRDS(tasmax_pres, paste0(here::here(), "/data/derived-data/21_tasmax_pres_55_isl.rds"))
saveRDS(tasmax_past, paste0(here::here(), "/data/derived-data/21_tasmax_past_55_isl.rds"))
saveRDS(pr_pres, paste0(here::here(), "/data/derived-data/21_pr_pres_55_isl.rds"))
saveRDS(pr_past, paste0(here::here(), "/data/derived-data/21_pr_past_55_isl.rds"))


####### Calculate delta between past and present climate #######

# variable 1: tas

tas_pres <- readRDS(paste0(here::here(), "/data/derived-data/21_tas_pres_55_isl.rds"))
tas_past <- readRDS(paste0(here::here(), "/data/derived-data/21_tas_past_55_isl.rds"))

head(tas_past)

# aggregate the data by island
# for past and present

tas_agg <- bind_rows(tas_past, tas_pres) %>% 
  group_by(ID, period) %>%
  summarise(
    mean = mean(tas),
    med = median(tas),
    sd = sd(tas)
  )

# do we observe climate change?
# all islands together
ggplot(tas_agg)+
  geom_boxplot(aes(x=period, y = sd))+
  geom_point(aes(x=period, y = sd, color = ID), position = "jitter")
t.test(mean ~ period, data = tas_agg)
t.test(sd ~ period, data = tas_agg)
# nothing really strong because climate is very different between islands
# add the archipelago
tas_agg_ar <- left_join(tas_agg, isl_select %>% select(X, Archip) %>% rename(ID=X))
ggplot(tas_agg_ar)+
  geom_boxplot(aes(x=period, y = mean))+
  geom_point(aes(x=period, y = mean, color = Archip), position = "jitter")

# calculate delta present vs past
delta_tas <- tas_agg %>% 
  pivot_wider(names_from = period, values_from = mean:sd) %>%
  mutate(d_mean_tas = mean_present - mean_past,
         d_med_tas = med_present - med_past,
         d_sd_tas = sd_present - sd_past)%>%
  rename(isl_num = ID) %>% 
  select(-c(contains("past"), contains("present")))
hist(delta_tas$d_mean_tas)  


# Variable 2: tasmax

tasmax_pres <- readRDS(paste0(here::here(), "/data/derived-data/21_tasmax_pres_55_isl.rds"))
tasmax_past <- readRDS(paste0(here::here(), "/data/derived-data/21_tasmax_past_55_isl.rds"))

# aggregate the data by island
tasmax_agg <- bind_rows(tasmax_past, tasmax_pres) %>% 
  group_by(ID, period) %>%
  summarise(
    mean = mean(tasmax),
    med = median(tasmax),
    sd = sd(tasmax))
# calculate delta present vs past
delta_tasmax <- tasmax_agg %>% 
  pivot_wider(names_from = period, values_from = mean:sd) %>%
  mutate(d_mean_tasmax = mean_present - mean_past,
         d_med_tasmax = med_present - med_past,
         d_sd_tasmax = sd_present - sd_past)%>%
  rename(isl_num = ID) %>% 
  select(-c(contains("past"), contains("present")))
hist(delta_tas$d_sd_tas)  


# Variable 3: pr

pr_pres <- readRDS(paste0(here::here(), "/data/derived-data/21_pr_pres_55_isl.rds"))
pr_past <- readRDS(paste0(here::here(), "/data/derived-data/21_pr_past_55_isl.rds"))

# aggregate the data by island
pr_agg <- bind_rows(pr_past, pr_pres) %>% 
  group_by(ID, period) %>%
  summarise(
    mean = mean(pr),
    med = median(pr),
    sd = sd(pr))
# calculate delta present vs past
delta_pr <- pr_agg %>% 
  pivot_wider(names_from = period, values_from = mean:sd) %>%
  mutate(d_mean_pr = mean_present - mean_past,
         d_med_pr = med_present - med_past,
         d_sd_pr = sd_present - sd_past)%>%
  rename(isl_num = ID) %>% 
  select(-c(contains("past"), contains("present")))
hist(delta_pr$d_sd_pr)  
hist(delta_pr$d_mean_pr)  

#### Climate velocity metric


####### Aggregate all var together #######

CC_exposure <- left_join(left_join(isl_select %>% select(X, ID) %>% 
                                     rename(isl_num=X), delta_tas), 
                         left_join(delta_tasmax, delta_pr) ) %>% select(-isl_num)

# save final db for exposure to CC
saveRDS(CC_exposure, "data/derived-data/21_CC_exposure_55_isl.rds")

####### Explore variation #######

cc <- readRDS("data/derived-data/21_CC_exposure_55_isl.rds")

# 1. check correlation between variables

corr = cor(cc %>% select(-ID))
p.mat <- ggcorrplot::cor_pmat(cc %>% select(-ID))

ggcorrplot::ggcorrplot(corr, p.mat = p.mat, hc.order = TRUE,
                       type = "lower", insig = "blank", lab = T)

# 2. check change within archipelagos

cc_ar <- left_join(cc, isl_select %>% select(ID, Archip, Long, Lat, Area, Dist))

# tas and sd tas
ggplot(cc_ar)+
  geom_point(aes(x=d_sd_tas, y = d_mean_tas, color = Archip), size = 2)+
  geom_smooth(aes(x=d_sd_tas, y = d_mean_tas), method = "lm")
# not correlated at all

ggplot(cc_ar)+
  geom_boxplot(aes(x=Archip, y = d_mean_tas), color = "firebrick")+
  geom_point(aes(x=Archip, y = d_mean_tas), color = "orange2", position = "jitter")
ggplot(cc_ar)+
  geom_boxplot(aes(x=Archip, y = d_sd_tas), color = "blue4")+
  geom_point(aes(x=Archip, y = d_sd_tas), color = "cyan3", position = "jitter")
# strong differences!

# is it explained by latitude?
ggplot(cc_ar)+
  geom_point(aes(x=d_mean_tas, y = Lat, color = Archip))
# not only, for instance big variability within Hawaii islands compared to Azores
ggplot(cc_ar)+
  geom_point(aes(x=d_sd_tas, y = Lat, color = Archip))
ggplot(cc_ar)+
  geom_point(aes(x=d_sd_tas, y = Area, color = Archip))


# tas and tasmax
ggplot(cc_ar)+
  geom_abline(slope = 1, intercept = 0, lty= 2, color = "grey20")+
  geom_point(aes(x=d_mean_tasmax, y = d_mean_tas, color = Archip), size = 2)


# pr and sd pr
ggplot(cc_ar)+
  geom_point(aes(x=d_sd_pr, y = d_mean_pr, color = Archip), size = 2)+
  geom_smooth(aes(x=d_sd_pr, y = d_mean_pr), method = "lm")

ggplot(cc_ar)+
  geom_boxplot(aes(x=Archip, y = d_mean_pr), color = "firebrick")+
  geom_point(aes(x=Archip, y = d_mean_pr), color = "orange2", position = "jitter")
ggplot(cc_ar)+
  geom_boxplot(aes(x=Archip, y = d_sd_pr), color = "blue4")+
  geom_point(aes(x=Archip, y = d_sd_pr), color = "cyan3", position = "jitter")
# strong differences!

# is it explained by latitude?
ggplot(cc_ar)+
  geom_point(aes(x=d_mean_tas, y = Lat, color = Archip))
# not only, for instance big variability within Hawaii islands compared to Azores
ggplot(cc_ar)+
  geom_point(aes(x=d_sd_tas, y = Lat, color = Archip))
ggplot(cc_ar)+
  geom_point(aes(x=d_sd_tas, y = Area, color = Archip))


