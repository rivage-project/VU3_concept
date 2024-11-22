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
var <- "tas" # tas, tasmax, pr 

# 
i = month[2]
j = year[2]

downloader::download(
  paste0("https://os.zhdk.cloud.switch.ch/chelsav2/GLOBAL/monthly/", 
         var, "/CHELSA_", var, "_", i, "_", j, "_V.2.1.tif"), 
  paste0(here::here(), "/data/raw-data/climate/CHELSA_monthly_",
         var, "_", i, "_", j, "_V.2.1SUPP.tif"),
  mode = "wb")



tas <- terra::rast(paste0(here::here(), "/data/raw-data/climate/CHELSA_monthly_",
                         var, "_", i, "_", j, "_V.2.1SUPP.tif"))


terra::plot(tas)

africa <- rnaturalearth::ne_countries(continent = "Africa", returnclass = "sf")

plot(africa %>% select(geometry))

# varifier les systèmes de coordonnées
sf::st_crs(tas) == sf::st_crs(africa)
#si pas le bon système, projeter l'objet dans le système du raster
africa_proj <- sf::st_transform(africa, sf::st_crs(tas))

# masquer les valeurs de raster qui ne sont pas dans le continent
tas_afr <- raster::mask(tas, africa)

terra::plot(tas_afr)


# calculer des distances
sum(sf::st_is_valid(africa))
sf::st_is_valid(africa)
africa <- sf::st_make_valid(africa)


bb <- sf::st_distance(africa %>% filter(sovereignt!="Sudan"))

bb2 <- as.data.frame(bb)

class(africa %>% filter(sovereignt == "Benin"))





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
  # var = "pr"
  # time = "present"
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
    # file = files_var_time[8]
    r <- terra::rast(paste0(here::here(), "/data/raw-data/climate/", file))
    sf::st_crs(shp) <- terra::crs(r)
    ext <- terra::extract(r, shp)
    colnames(ext) <- c('ID', var)
    ext$period <- time
    ext$file <- file
    ext$pixel <- 1:nrow(ext)
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




####### Calculate SED following Williams et al 2007 #######

# several options
# annual mean T + annual mean P
# mean T/P JJA and DJF

# variable 1: tas

tas_pres <- readRDS(paste0(here::here(), "/data/derived-data/21_tas_pres_55_isl.rds"))
tas_past <- readRDS(paste0(here::here(), "/data/derived-data/21_tas_past_55_isl.rds"))

# group by year first to have the total annual precipitation amount
# and then calculate sd and mean on the grouped values

tas_pres_pix_y <- tas_pres %>% 
  separate(col = file, 
           into = c("name1", "name2", "name3", "month", "year", "name4"),
           sep="_") %>%
  select(-contains("name")) %>%
  mutate(month_year = paste(year, month, sep="_"))

month_year_pres_tas <- data.frame(
  month_year = sort(unique(tas_pres_pix_y$month_year)),
  num_y = rep(1:5, each = 12)
)

tas_pres_pix_y_ok <- left_join(tas_pres_pix_y, month_year_pres_tas) %>%
  group_by(ID, pixel, num_y) %>%
  summarize(annual_tas = mean(tas))

tas_past_pix_y <- tas_past %>% 
  separate(col = file, 
           into = c("name1", "name2", "name3", "month", "year", "name4"),
           sep="_") %>%
  select(-contains("name")) %>%
  mutate(month_year = paste(year, month, sep="_"))

month_year_past_tas <- data.frame(
  month_year = sort(unique(tas_past_pix_y$month_year)),
  num_y = rep(1:5, each = 12)
)

tas_past_pix_y_ok <- left_join(tas_past_pix_y, month_year_past_tas) %>%
  group_by(ID, pixel, num_y) %>%
  summarize(annual_tas = mean(tas))

tas_t1_y <- tas_past_pix_y_ok %>%
  group_by(ID, pixel)%>%
  summarize(mean_tas_t1 = mean(annual_tas),
            sd_tas_t1 = sd(annual_tas))
tas_t2_y <- tas_pres_pix_y_ok %>%
  group_by(ID, pixel)%>%
  summarize(mean_tas_t2 = mean(annual_tas))

SED_tas_y <- left_join(tas_t1_y, tas_t2_y) %>%
  mutate(sed_tas = ((mean_tas_t2-mean_tas_t1)^2)/(sd_tas_t1^2))

SED_tas_isl_y <- SED_tas_y %>%
  group_by(ID) %>%
  summarize(sed_tas_isl_med = median(sed_tas),
            sed_tas_isl_mean = mean(sed_tas),
            sed_tas_isl_sd = sd(sed_tas)) %>%
  rename(X = ID) %>%
  mutate(ID = isl_select$ID)

hist(SED_tas_isl_y$sed_tas_isl_mean)
hist(SED_tas_isl_y$sed_tas_isl_med)
hist(log(SED_tas_isl_y$sed_tas_isl_mean+1))
hist(log(SED_tas_isl_y$sed_tas_isl_med+1))
summary(SED_tas_isl_y)
summary(SED_tas_isl)
ggplot(SED_tas_isl_y)+
  geom_point(aes(x=sed_tas_isl_mean, y = sed_tas_isl_med ))

# variable 2: precipitation amount

pr_pres <- readRDS(paste0(here::here(), "/data/derived-data/21_pr_pres_55_isl.rds"))
pr_past <- readRDS(paste0(here::here(), "/data/derived-data/21_pr_past_55_isl.rds"))

# group by year first to have the mean annual precipitation 
# and then calculate sd and mean on the grouped values
# to avoid having too high sd due to monthly variations

pr_pres_pix_y <- pr_pres %>% 
  separate(col = file, 
           into = c("name1", "name2", "name3", "month", "year", "name4"),
           sep="_") %>%
  select(-contains("name")) %>%
  mutate(month_year = paste(year, month, sep="_"))

month_year_pres <- data.frame(
  month_year = sort(unique(pr_pres_pix_y$month_year)),
  num_y = rep(1:5, each = 12)
)

pr_pres_pix_y_ok <- left_join(pr_pres_pix_y, month_year_pres) %>%
  group_by(ID, pixel, num_y) %>%
  summarize(annual_pr = mean(pr))

pr_past_pix_y <- pr_past %>% 
  separate(col = file, 
           into = c("name1", "name2", "name3", "month", "year", "name4"),
           sep="_") %>%
  select(-contains("name")) %>%
  mutate(month_year = paste(year, month, sep="_"))

month_year_past <- data.frame(
  month_year = sort(unique(pr_past_pix_y$month_year)),
  num_y = rep(1:5, each = 12)
)

pr_past_pix_y_ok <- left_join(pr_past_pix_y, month_year_past) %>%
  group_by(ID, pixel, num_y) %>%
  summarize(annual_pr = mean(pr))

pr_t1_y <- pr_past_pix_y_ok %>%
  group_by(ID, pixel)%>%
  summarize(mean_pr_t1 = mean(annual_pr),
            sd_pr_t1 = sd(annual_pr))
pr_t2_y <- pr_pres_pix_y_ok %>%
  group_by(ID, pixel)%>%
  summarize(mean_pr_t2 = mean(annual_pr))

SED_pr_y <- left_join(pr_t1_y, pr_t2_y) %>%
  mutate(sed_pr = ((mean_pr_t2-mean_pr_t1)^2)/(sd_pr_t1^2))

SED_pr_isl_y <- SED_pr_y %>%
  group_by(ID) %>%
  summarize(sed_pr_isl_med = median(sed_pr),
            sed_pr_isl_mean = mean(sed_pr),
            sed_pr_isl_sd = sd(sed_pr)) %>%
  rename(X = ID) %>%
  mutate(ID = isl_select$ID)


summary(SED_pr_isl_y)
ggplot(SED_pr_isl_y)+
  geom_point(aes(x=sed_pr_isl_mean, y = sed_pr_isl_med ))

hist(SED_pr_isl_y$sed_pr_isl_mean)
hist(SED_pr_isl_y$sed_pr_isl_med)
hist(log(SED_pr_isl_y$sed_pr_isl_mean))
hist(log(SED_pr_isl_y$sed_pr_isl_med))

# what's the matter with Hawaii island #1?
ggplot(pr_past_pix_y_ok %>% filter(ID==1))+
  geom_point(aes(x=num_y, y = annual_pr), position = "jitter", alpha=.5)
ggplot(pr_pres_pix_y_ok %>% filter(ID==1))+
  geom_point(aes(x=num_y, y = annual_pr), position = "jitter", alpha=.5)


ggplot(pr_past_pix_y_ok%>% filter(ID==1))+
  geom_boxplot(aes(x=as.factor(num_y), y = annual_pr))
ggplot(pr_pres_pix_y_ok%>% filter(ID==1))+
  geom_boxplot(aes(x=as.factor(num_y), y = annual_pr))


plot(SED_tas_isl_y$sed_tas_isl_mean, SED_tas_isl$sed_tas_isl_mean)



#### Combine tas and pr per pixel to obtain final SED 

# according to Williams: sum the variables and then sqrt
# mean or med by isl must be done after the transformation for each pixel

SED <- left_join(SED_tas_y, SED_pr_y) %>%
  mutate(SED_tot = sed_tas + sed_pr,
         SED_sqrt = sqrt(sed_tas + sed_pr)) %>%
  group_by(ID) %>%
  summarize(sed_pr_isl_med = median(sed_pr),
            sed_pr_isl_mean = mean(sed_pr),
            sed_pr_isl_sd = sd(sed_pr),
            sed_tas_isl_med = median(sed_tas),
            sed_tas_isl_mean = mean(sed_tas),
            sed_tas_isl_sd = sd(sed_tas),
            sed_tot_med = median(SED_tot),
            sed_tot_mean = mean(SED_tot),
            sed_tot_sd = sd(SED_tot),
            sed_sqrt_med = median(SED_sqrt),
            sed_sqrt_mean = mean(SED_sqrt),
            sed_sqrt_sd = sd(SED_sqrt)) %>%
  rename(X = ID) %>%
  mutate(ID = isl_select$ID)



summary(SED)
colnames(SED)

ggplot(SED)+
  geom_point(aes(x=sed_sqrt_med, y = sed_sqrt_mean))
hist(SED$sed_sqrt_med)
hist(SED$sed_tot_med)
hist(SED$sed_sqrt_mean)
ggplot(SED)+
  geom_point(aes(x=sed_tot_med, y = sed_sqrt_med))


# save file
saveRDS(SED %>% select(-X), "data/derived-data/21_CC_SED_exposure_55_isl.rds")


####### Explore variation SED #######

cc <- readRDS("data/derived-data/21_CC_SED_exposure_55_isl.rds") %>% 
  select(-contains("_sd"))

# 1. check correlation between variables

corr = cor(cc %>% select(-ID))
p.mat <- ggcorrplot::cor_pmat(cc %>% select(-ID))

ggcorrplot::ggcorrplot(corr, p.mat = p.mat, hc.order = TRUE,
                       type = "lower", insig = "blank", lab = T)

# 2. check change within archipelagos

cc_ar <- left_join(cc, isl_select %>% select(ID, Archip, Long, Lat, Area, Dist))


# PR and TAS
ggplot(cc_ar)+
  geom_point(aes(x=sed_tas_isl_med, y = sed_pr_isl_med, color = Archip), size = 2)+
  geom_abline(slope=1, intercept = 0, lty=2)
cor.test(cc_ar$sed_tas_isl_med, cc_ar$sed_pr_isl_med)
# not correlated at all

ggplot(cc_ar)+
  geom_boxplot(aes(x=Archip, y = sed_tot_med), color = "firebrick")+
  geom_point(aes(x=Archip, y = sed_tot_med), color = "orange2", position = "jitter")
ggplot(cc_ar)+
  geom_boxplot(aes(x=Archip, y = sed_sqrt_med), color = "blue4")+
  geom_point(aes(x=Archip, y = sed_sqrt_med), color = "cyan3", position = "jitter")

# effect of latitude
ggplot(cc_ar)+
  geom_point(aes(x=sed_tot_med, y = abs(Lat), color = Archip))





####### Explore variation v1 simple calc #######

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


